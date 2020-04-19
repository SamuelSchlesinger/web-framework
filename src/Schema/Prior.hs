{-# LANGUAGE DataKinds #-}
module Schema.Prior where

import Squeal.PostgreSQL

-- Here, we place the schema we expect to exist in our database before we
-- run any migrations. This may include tables that postgres brings into
-- existence, for instance.

type Schemas = Public '[]
