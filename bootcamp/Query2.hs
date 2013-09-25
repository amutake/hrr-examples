{-# LANGUAGE FlexibleContexts #-}

module Query2
  ( module Query1
  , findPointSystems
  ) where

import Data.Int (Int32)
import Database.Relational.Query

import Query1
import qualified PointSystems
import PointSystems (PointSystemsTable, pointSystemsTable)

-- | 2.1.5.1 ポイント処理方法テーブルを取得する SQL
-- @
--   SELECT POINT_SYSTEM FROM POINT_SYSTEMS_TABLE WHERE MEMBER_ID = ?;
-- @
findPointSystems :: Relation Int32 Int32
findPointSystems = relation' $ do
    p <- query pointSystemsTable
    (ph, ()) <- placeholder $ \memberId -> wheres $
        p ! PointSystems.memberId' .=. memberId
    return (ph, p ! PointSystems.pointSystem')
