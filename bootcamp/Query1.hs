{-# LANGUAGE FlexibleContexts #-}

module Query1 where

import Data.Int (Int32)
import Data.Time.Calendar (Day)
import Database.Relational.Query
  ( Relation, Insert, Delete
  , relation, relation', aggregateRelation'
  , query, query'
  , (.<=.), (.>=.), (.<.), (!), (><)
  , placeholder
  , wheres
  , value
  , typedDelete
  , restriction
  , max', min', and', fst', snd'
  , asc
  , piZip -- :: Pi a b -> Pi a c -> Pi a (b, c)
  )
import Database.Relational.Query.Monad.Class (MonadAggregate (..))

import DataSource (connect)
import qualified Pos
import Pos (PosTable, posTable)
import qualified PosDetail
import PosDetail (PosDetailTable, posDetailTable)
import qualified ItemMst
import ItemMst (ItemMstTable, itemMstTable)
import qualified PointManagement
import PointManagement (PointManagementTable, pointManagementTable)
import qualified ErrorPos
import ErrorPos (ErrorPosTable, errorPosTable)

-- | 1.1.5.1 POS データを取得する SQL
-- @
--   SELECT POS_ID, MEMBER_ID FROM POS_TABLE WHERE CREATED_DATE >=
--   TO_DATE(?, 'yyyy/mm') AND CREATED_DATE < TO_DATE(?, 'yyyy/mm')
--   ORDER BY POS_ID;
-- @
findPos :: Relation (Day, Day) (Int32, Int32) -- Relation (String, String) (Int32, Int32)
-- TO_DATE に相当するものがない？ので Day を入力にとるようにしている
findPos = relation' $ do
    p <- query posTable
    (ph, ()) <- placeholder $ \monthTuple -> wheres $
        (p ! Pos.createdDate' .>=. monthTuple ! fst') `and'`
        (p ! Pos.createdDate' .<. monthTuple ! snd')
    asc $ p ! Pos.posId'
    return (ph, p ! piZip Pos.posId' Pos.memberId')

-- | 1.1.5.2 POS 詳細データ取得時に POS_ID の範囲を絞るための最大値、最小値を取る SQL
-- @
--   SELECT MAX(POS_ID), MIN(POS_ID) FROM POS_TABLE WHERE CREATED_DATE
--   >= TO_DATE(?, 'yyyy/mm') AND CREATED_DATE < TO_DATE(?, 'yyyy/mm')
--   ORDER BY POS_ID;
-- @
findMaxMinPosId :: Relation (Day, Day) (Int32, Int32)
findMaxMinPosId = aggregateRelation' $ do
    (ph, p) <- query' findPos
    let posId = p ! fst'
    return (ph, max' posId >< min' posId) -- QueryAggregate (PlaceHolder (Day, Day), Projection Aggregated (Int32, Int32))

-- | 1.1.5.3 POS 詳細データを取得する SQL
-- @
--   SELECT POS_ID, ITEM_ID, AMOUNT, SALES_PRICE, OFFSET_PRICE FROM
--   POS_DETAIL_TABLE WHERE POS_ID <= ? AND POS_ID >= ? ORDER BY
--   POS_ID;
-- @
findPosDetail :: Relation (Int32, Int32) PosDetailTable
findPosDetail = relation' $ do
    p <- query posDetailTable -- p :: Projection Flat PosDetailTable
    (ph, ()) <- placeholder $ \posIdTuple -> wheres $ -- posId :: Projection Flat (Int32, Int32)
        (p ! PosDetail.posId' .>=. posIdTuple ! fst') `and'`
        (p ! PosDetail.posId' .<=. posIdTuple ! snd')
    asc $ p ! PosDetail.posId'
    return (ph, p) -- :: QuerySimple (PlaceHolder (Int32, Int32), Projection Flat PosDetailTable)

-- | 1.1.5.4 商品マスタを取得する SQL
-- @
--   SELECT ITEM_ID, ITEM_NAME, TAG_PRICE FROM ITEM_MST_TABLE;
-- @
findItemMst :: Relation () ItemMstTable
findItemMst = itemMstTable

-- | 1.1.5.5 翌月加算ポイントテーブルへ値を入力する SQL
-- @
--   INSERT INTO POINT_MANAGEMENT_TABLE(MEMBER_ID, MANAGED_POINT,
--   ISSUE_DATE) VALUES (?, ?, SYSDATE);
-- @
insertPointManagement :: Insert PointManagementTable -- Insert (Int32, Int32) にはできない
insertPointManagement = PointManagement.insertPointManagementTable

-- | 1.1.5.6 エラー POS データテーブルへ値を入力する SQL
-- @
--   INSERT INTO ERROR_POS_TABLE(POS_ID) VALUES (?);
-- @
insertErrorPos :: Insert ErrorPosTable
insertErrorPos = ErrorPos.insertErrorPosTable -- made by TH

-- | 1.1.5.7 エラー POS データテーブルを初期化する SQL
-- @
--   DELETE FROM ERROR_POS_TABLE;
-- @
initializeErrorPos :: Delete ()
initializeErrorPos = typedDelete ErrorPos.tableOfErrorPosTable . restriction $ const $ return ()
