{-# LANGUAGE DataKinds #-}
module Schema.Latest where

import Squeal.PostgreSQL

-- Here, we will place the latest schema, and we will modify this to be V1,
-- V2, etc. as we replace this file with the newest edition of the schema.

type Schemas = Public '[]
