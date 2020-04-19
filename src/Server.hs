{-# LANGUAGE NoImplicitPrelude #-}
module Server where

import API
import Servant

server :: Server API
server = emptyServer
