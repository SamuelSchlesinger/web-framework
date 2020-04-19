{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
module API where

import Data.Proxy
import Data.Text (Text)
import Servant.API
import GHC.Generics (Generic)
import Prelude
import Data.Aeson
import qualified Generics.SOP as SOP
import Data.Int (Int64)

data NameAndPassword = NameAndPassword
  { name :: Text
  , password :: Text
  } deriving stock (Show, Read, Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype Session = Session
  { sessionID :: Int64
  } deriving newtype (Show, Read, Eq, Ord, ToJSON, FromJSON)
    deriving stock Generic
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

data WithSession a = WithSession { session :: Session, payload :: a }
    deriving stock (Show, Read, Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON)

type API = "signup" :> ReqBody '[JSON] NameAndPassword :> Post '[JSON] NoContent
      :<|> "signin" :> ReqBody '[JSON] NameAndPassword :> Post '[JSON] Session
      :<|> "change" :> ("password" :> ReqBody '[JSON] (WithSession Text) :> Post '[JSON] NoContent
                   :<|> "name" :> ReqBody '[JSON] (WithSession Text) :> Post '[JSON] NoContent)

apiProxy :: Proxy API
apiProxy = Proxy
