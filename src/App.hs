{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module App where

import API
import Server
import Data.Function
import System.IO
import Servant.Server
import Network.Wai.Handler.Warp
import Squeal.PostgreSQL
import Network.Wai.Middleware.RequestLogger
import Data.Pool

main :: IO ()
main = do
  connPool <- createPool (connectdb "host=localhost port=5432 dbname=user_system") finish 1 10 10
  serve apiProxy (server connPool) & logStdoutDev  & run 8080
