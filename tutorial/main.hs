module Main where

import Database.Relational.Query (relationalQuery)
import Database.HDBC.Session (withConnectionIO, handleSqlError')
import Database.HDBC.Record.Query (runQuery)

import DataSource (connect)
import Query (lessThan100yen)

main :: IO ()
main = handleSqlError' $ withConnectionIO connect $ \conn -> do
    runQuery conn (relationalQuery lessThan100yen) () >>= print
