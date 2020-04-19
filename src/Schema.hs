{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Schema where

import Squeal.PostgreSQL
import System.IO

import qualified Schema.V0 as V0
import qualified Schema.V1 as V1
import qualified Schema.Latest as V2

migrations :: Path (Migration (IsoQ Definition)) V0.Schemas V2.Schemas
migrations = V1.setup :>> V2.setup :>> Done

main :: IO ()
main = mainMigrateIso "host=localhost port=5432 dbname=user_system" migrations
