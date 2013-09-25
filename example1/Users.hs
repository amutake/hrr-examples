{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}

module Users where

import Data.Int (Int32)
import Database.Record.TH (derivingShow)
import Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import Database.HDBC.Schema.Driver (typeMap)
import Database.HDBC.Query.TH (defineTableDefault')
import Language.Haskell.TH.Name.CamelCase (ConName)


import DataSource (connection)

$(defineTableDefault'
  "public"
  "users"
  [ ("user_id", [t|Int32|])
  , ("name", [t|String|])
  , ("password", [t|String|])
  , ("age", [t|Int32|])
  ]
  [derivingShow])
