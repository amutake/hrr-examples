{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}

module Family where

import Database.HDBC.Query.TH (defineTableFromDB)
import Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import Database.Record.TH (derivingShow)

import DataSource (connect, schema)

defineTableFromDB connect driverPostgreSQL schema "family" [derivingShow]
