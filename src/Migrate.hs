{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Migrate where

import Squeal.PostgreSQL
import qualified Schema.Prior
import qualified Schema.Latest
import Control.Category
import System.IO

migrations :: Path (Migration (IsoQ Definition)) Schema.Prior.Schemas Schema.Latest.Schemas
migrations = id

main :: IO ()
main = mainMigrateIso "<postgres-connection-string>" migrations
