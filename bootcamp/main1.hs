module Main where

import Control.Applicative ((<$>))
import Control.Monad (mapM_)
import Data.Int (Int32)
import Data.Maybe (catMaybes)
import Data.List (partition)

import Database.HDBC.Session (withConnectionIO, handleSqlError')
import Database.HDBC (commit)
import Database.Relational.Query (typedDelete, restriction)

import DataSource (connect)
import Query1
import Runner
import Util
import qualified PointManagement as PM

acc :: (Eq a, Num b) => [(a, b)] -> [(a, b)]
acc [] = []
acc xs@((m, _):_) = (m, total) : acc bs
  where
    (as, bs) = partition (\(a, b) -> m == a) xs
    total = sum $ map snd $ as

point :: Int32 -> Int32
point = round . (* 0.01) . fromIntegral

main :: IO ()
main = do
    (from, to) <- getFromTo

    handleSqlError' $ withConnectionIO connect $ \conn -> do

        -- 初期化
        runDelete conn initializeErrorPos ()
        runDelete conn (typedDelete PM.tableOfPointManagementTable . restriction $ const $ return ()) () -- 課題としては必要ない

        -- 対象となる POS の取得
        poss <- runRelation conn findPos (from, to)

        -- [(会員ID, 翌月加算ポイント)] の計算
        memberPoints <- map (fmap point) . acc . catMaybes <$> mapM (memberTotal conn) poss
        print memberPoints

        -- 翌月加算ポイントの書き込み
        mapM_ (putPoint conn) memberPoints

        -- トランザクションのコミット
        commit conn
