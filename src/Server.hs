{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
module Server where

import Data.Int (Int64)
import API
import Servant
import Schema.Latest 
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import Squeal.PostgreSQL hiding (Pool, name)
import GHC.Generics (Generic)
import qualified Generics.SOP as SOP
import System.Entropy
import Crypto.BCrypt
import Control.Monad.IO.Class
import qualified Data.Text.Encoding as Encoding
import Data.Pool

data NewUser = NewUser
  { name :: Text
  , passhash :: ByteString
  } deriving stock Generic
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

tx :: Pool (K Connection Schemas) -> PQ Schemas Schemas IO a -> Handler a
tx connPool pq = withResource connPool \conn -> liftIO $ (evalPQ . transactionallyRetry defaultMode) pq conn

hashpass :: MonadIO m => Text -> m (Either ServerError ByteString)
hashpass password = do
  let cost = 16
  entropy <- liftIO (getEntropy 32)
  pure do
    salt <- maybe (Left err500) Right (genSalt "$2b$" cost entropy)
    maybe (Left err500) Right (hashPassword (Encoding.encodeUtf8 password) salt)
    

signup :: Pool (K Connection Schemas) -> NameAndPassword -> Handler NoContent
signup connPool NameAndPassword{..} = either throwError pure =<< tx connPool do
  executeParams (query selectUserByName) (Only name) >>= firstRow
    >>= flip maybe (const @_ @User $ pure $ Left err403) do
      hashpass password >>= either (pure . Left) \passhash -> do
        handleSqueal 
          (pure . Left . squealExceptionToServerError)
          (Right NoContent <$ executeParams_ (manipulation insertNewUser) (NewUser name passhash))
  where
    squealExceptionToServerError :: SquealException -> ServerError
    squealExceptionToServerError = \case
      SQLException SQLState{sqlErrorMessage} ->
        if BS.isInfixOf "unique_name" sqlErrorMessage 
          then err403
          else err500
      ConnectionException _ -> err500
      DecodingException _ _ -> err500
      ColumnsException _ _ -> err500
      RowsException _ _ _ -> err500 

    insertNewUser :: Manipulation_ Schemas NewUser ()
    insertNewUser =
      insertInto_ #user 
        (Values_ (Default `as` #id 
               :* Set (param @1) `as` #name
               :* Set (param @2) `as` #passhash))

data User = User
  { userID :: Int64
  , name :: Text
  , passhash :: ByteString
  } deriving stock Generic
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

selectUserByName :: Query_ Schemas (Only Text) User
selectUserByName = select_
  (#id `as` #userID :*
   #name :*
   #passhash)
  (from (table #user)
  & where_ (#user ! #name .== param @1))

signin :: Pool (K Connection Schemas) -> NameAndPassword -> Handler Session
signin connPool NameAndPassword{..} = either throwError pure =<< tx connPool do
  (firstRow =<< executeParams (query selectUserByName) (Only name)) >>= \case
    Nothing -> pure (Left err404)
    Just User{userID, passhash} -> do
      if not $ validatePassword passhash (Encoding.encodeUtf8 password)
        then pure (Left err403)
        else do
          maybe (pure $ Left err500) (pure . Right)
            =<< (firstRow =<< executeParams (manipulation insertNewSession) (Only userID))
  where
    insertNewSession :: Manipulation_ Schemas (Only Int64) Session
    insertNewSession = insertInto #session
      (Values_ (
         Default `as` #id :*
         Set (param @1) `as` #user :*
         Set now `as` #creation :*
         Set (now !+ interval_ 1 Hours) `as` #expiration
        )
      )
      OnConflictDoRaise
      (Returning_ (#id `as` #sessionID))

changepass :: Pool (K Connection Schemas) -> WithSession Text -> Handler NoContent
changepass connPool (WithSession session newPassword) = either throwError pure =<< tx connPool do 
  (firstRow =<< executeParams (query selectUserWithValidSession) (Only $ sessionID session)) >>= \case
    Nothing -> pure (Left err403)
    Just User{userID} -> do
      hashpass newPassword >>= either (pure . Left) \passhash ->
        Right NoContent <$ executeParams_ (manipulation updatePasshash) (userID, passhash)
  where
    updatePasshash :: Manipulation_ Schemas (Int64, ByteString) ()
    updatePasshash = update_ #user (Set (param @2) `as` #passhash) (#user ! #id .== param @1)

selectUserWithValidSession :: Query_ Schemas (Only Int64) User
selectUserWithValidSession = select_
  (#user ! #id `as` #userID :*
   #user ! #name :*
   #user ! #passhash)
  (from (table #session & innerJoin (table #user) (#user ! #id .== #session ! #user)) & where_ (param @1 .== #session ! #id .&& now .< #session ! #expiration))

changename :: Pool (K Connection Schemas) -> WithSession Text -> Handler NoContent
changename connPool (WithSession session newName) = either throwError pure =<< tx connPool do
  (firstRow =<< executeParams (query selectUserWithValidSession) (Only $ sessionID session)) >>= \case
    Nothing -> pure (Left err403)
    Just User{userID} -> do
      handleSqueal (pure . Left . squealExceptionToServerError) $ Right NoContent <$ executeParams_ (manipulation updateName) (userID, newName)
  where
    squealExceptionToServerError :: SquealException -> ServerError
    squealExceptionToServerError = \case
      SQLException SQLState{sqlErrorMessage} ->
        if BS.isInfixOf "unique_name" sqlErrorMessage 
          then err403
          else err500
      ConnectionException _ -> err500
      DecodingException _ _ -> err500
      ColumnsException _ _ -> err500
      RowsException _ _ _ -> err500 
    updateName :: Manipulation_ Schemas (Int64, Text) ()
    updateName = update_ #user (Set (param @2) `as` #name) (#user ! #id .== param @1)

server :: Pool (K Connection Schemas) -> Server API
server connPool = signup connPool :<|> signin connPool :<|> (changepass connPool :<|> changename connPool)
