module DataSource
  ( connect
  , schema
  ) where

import Database.HDBC.PostgreSQL (connectPostgreSQL, Connection)

connect :: IO Connection
connect = connectPostgreSQL "dbname=testdb"

schema :: String
schema = "DQ"
