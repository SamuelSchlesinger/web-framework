{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
module Schema.Latest where

import Squeal.PostgreSQL
import qualified Schema.V1 as V1

type SessionColumns =
  '[ "id" ::: 'Def :=> 'NotNull 'PGint8
   , "user" ::: 'NoDef :=> 'NotNull 'PGint8
   , "creation" ::: 'NoDef :=> 'NotNull 'PGtimestamptz
   , "expiration" ::: 'NoDef :=> 'NotNull 'PGtimestamptz
   ]

type SessionConstraints =
  '[ "pk_session" ::: 'PrimaryKey '["id"] 
   , "fk_user" ::: 'ForeignKey '["user"] "user" '["id"]]

type Schemas = Public '[ "user" ::: 'Table (V1.UserConstraints :=> V1.UserColumns) 
                       , "session" ::: 'Table (SessionConstraints :=> SessionColumns) ]

setup :: Migration (IsoQ Definition) V1.Schemas Schemas
setup = Migration
  { name = "Session" 
  , migration = IsoQ 
    { up = createTable #session 
             (serial8           `as` #id 
             -- ^ big bad, switch to uuid v4 or otherwise random id
           :* notNullable int8  `as` #user 
           :* notNullable timestamptz  `as` #creation
           :* notNullable timestamptz  `as` #expiration) 
             (primaryKey #id    `as` #pk_session
           :* foreignKey #user #user #id OnDeleteCascade OnUpdateNoAction `as` #fk_user)
             
    , down = dropTable #session
    } 
  }
