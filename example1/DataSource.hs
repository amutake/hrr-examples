module DataSource
  ( connection
  ) where

import Database.HDBC.PostgreSQL (connectPostgreSQL, Connection)

connection :: IO Connection
connection = connectPostgreSQL "dbname=rr-example1"
