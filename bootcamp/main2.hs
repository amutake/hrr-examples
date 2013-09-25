module Main where

import Control.Applicative ((<$>))
import Control.Monad (mapM_, when, join)
import Data.Int (Int32)
import Data.Maybe (catMaybes)
import Data.List (partition)

import Database.HDBC.Session (withConnectionIO, handleSqlError')
import Database.HDBC (IConnection (commit))
import Database.Relational.Query (typedDelete, restriction)

import DataSource (connect)
import Query2
import Runner
import Util
import qualified PointManagement as PM
import qualified PointSystems as PS

acc :: [(Int32, Int32)] -> [(Int32, Int32, Int32)]
acc [] = []
acc xs@((m, _):_) = (m, times, total) : acc bs
  where
    (as, bs) = partition (\(a, b) -> m == a) xs
    times = fromIntegral $ length as
    total = sum $ map snd $ as

point :: IConnection conn => conn -> (Int32, Int32, Int32) -> IO (Int32, Int32)
point conn (memberId, times, total) = do
    ps <- runRelation conn findPointSystems memberId
    when (null ps) $ logger errMsg >> error errMsg
    let point = round $ sum $ map (\p -> PS.calcPoint p times total) ps
    return (memberId, point)
  where
    errMsg = "Missing point system: MEMBER_ID: " ++ show memberId ++ "\n"

main :: IO ()
main = do
    (from, to) <- getFromTo

    handleSqlError' $ withConnectionIO connect $ \conn -> do

        -- 初期化
        runDelete conn initializeErrorPos ()
        runDelete conn (typedDelete PM.tableOfPointManagementTable . restriction $ const $ return ()) ()

        -- 対象となる POS の取得
        poss <- runRelation conn findPos (from, to)

        -- [(会員ID, 翌月加算ポイント)] の計算
        memberPoints <- join $ mapM (point conn) . acc . catMaybes <$> mapM (memberTotal conn) poss
        print memberPoints

        -- 翌月加算ポイントの書き込み
        mapM_ (putPoint conn) memberPoints

        -- トランザクションのコミット
        commit conn
