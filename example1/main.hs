module Main where

import Database.HDBC.PostgreSQL (withPostgreSQL)
import Database.HDBC.Record.Query (runQuery)
import Database.HDBC.Session (handleSqlError', withConnectionIO)

import Database.Relational.Query (relationalQuery)

import DataSource (connection)
import Query
import Users

main :: IO ()
main = handleSqlError' $ withConnectionIO connection $ \conn -> do
  -- withPostgreSQL "dbname=rr-example1" $ \conn -> do
  putStrLn "hello"

  records <- runQuery conn () $ relationalQuery allUsers
  mapM_ print records
