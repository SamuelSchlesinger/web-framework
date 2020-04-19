{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
module Schema.V1 where

import Squeal.PostgreSQL
import qualified Schema.V0 as V0

type UserColumns =
  '[ "id" ::: 'Def :=> 'NotNull 'PGint8
   , "name" ::: 'NoDef :=> 'NotNull 'PGtext
   , "passhash" ::: 'NoDef :=> 'NotNull 'PGbytea
   ]

type UserConstraints =
  '[ "pk_user" ::: 'PrimaryKey '["id"] 
   , "unique_name" ::: 'Unique '["name"] ]

type Schemas = Public '[ "user" ::: 'Table (UserConstraints :=> UserColumns) ]

setup :: Migration (IsoQ Definition) V0.Schemas Schemas
setup = Migration
  { name = "User" 
  , migration = IsoQ 
    { up = createTable #user 
             (serial8           `as` #id 
           :* notNullable text  `as` #name 
           :* notNullable bytea `as` #passhash)
             (primaryKey #id    `as` #pk_user
           :* unique #name      `as` #unique_name)
             
    , down = dropTable #user
    } 
  }
