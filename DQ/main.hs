module Main where

import Database.Relational.Query (relationalQuery)
import Database.HDBC.Session (withConnectionIO, handleSqlError')
import Database.HDBC.Record.Query (runQuery)

import DataSource (connect)
import Query

run conn rel param = runQuery conn param (relationalQuery rel) >>= print

main :: IO ()
main = handleSqlError' $ withConnectionIO connect $ \conn -> do

    run conn familyMonsters ()

    run conn findMonstersByLevelRange (20, 40)

    run conn findMonstersByFamilyName "slime"
    run conn findMonstersByFamilyName "dragon"

    run conn findBoss ()
