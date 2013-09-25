{-# LANGUAGE FlexibleContexts #-}

module Runner
  ( runRelation
  , runInsert
  , runDelete
  ) where

import Database.HDBC (IConnection (commit), SqlValue)
import Database.HDBC.Record.Insert (prepareInsert, runPreparedInsert)
import Database.HDBC.Record.Delete (prepareDelete, runPreparedDelete)
import Database.HDBC.Record.Query (runQuery)
import Database.Relational.Query (Relation, Insert, Delete, relationalQuery)
import Database.Record (FromSql, ToSql)

-- like Database.HDBC.Record.Query.runQuery, but for debug
runRelation :: (IConnection c, Show a, FromSql SqlValue a, ToSql SqlValue p)
            => c -> Relation p a -> p -> IO [a]
runRelation conn rel param = do
  putStrLn $ "SQL: " ++ show rel
  runQuery conn param (relationalQuery rel)

-- like Database.HDBC.Record.Insert.runInsert, but for debug
runInsert :: (IConnection c, Show a, ToSql SqlValue a)
          => c -> Insert a -> a -> IO ()
runInsert conn ins param = do
  putStrLn $ "SQL: " ++ show ins
  ps <- prepareInsert conn ins
  runPreparedInsert param ps
  return ()

-- like Database.HDBC.Record.Delete.runDelete, but for debug
runDelete :: (IConnection c, Show p, ToSql SqlValue p)
          => c -> Delete p -> p -> IO ()
runDelete conn del param = do
  putStrLn $ "SQL: " ++ show del
  ps <- prepareDelete conn del
  runPreparedDelete param ps
  return ()
