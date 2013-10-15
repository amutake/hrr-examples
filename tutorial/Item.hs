{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}

module Item where

import Database.HDBC.Query.TH (defineTableFromDB)
import Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import Database.Record.TH (derivingShow)

import DataSource (connect)

$(defineTableFromDB connect driverPostgreSQL "TUTORIAL" "item" [derivingShow])
