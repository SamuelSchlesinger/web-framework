{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Migrate where

import Squeal.PostgreSQL
import Control.Category
import System.IO

import qualified Schema.V0 as V0

migrations :: Path (Migration (IsoQ Definition)) V0.Schemas V0.Schemas
migrations = id

main :: IO ()
main = mainMigrateIso "<postgres-connection-string>" migrations
