{-# LANGUAGE NoImplicitPrelude #-}
module App where

import API
import Server
import Data.Function
import System.IO
import Servant.Server
import Network.Wai.Handler.Warp

main :: IO ()
main = serve apiProxy server & run 8080
