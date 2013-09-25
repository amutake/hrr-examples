module Main where

import Control.Monad
import Data.Time.Calendar

import Database.HDBC (IConnection (commit), SqlValue)
import Database.HDBC.Session (handleSqlError', withConnectionIO)
import Database.Relational.Query

import DataSource (connect, dayFrom, dayTo)
import Runner
import qualified Pos as P
import qualified PosDetail as PD
import qualified ItemMst as IM
import qualified PointManagement as PM
import qualified ErrorPos as EP
import qualified PointSystems as PS

main :: IO ()
main = handleSqlError' $ withConnectionIO connect $ \conn -> do
    -- delete
    let del t = typedDelete t . restriction $ const $ return ()
    runDelete conn (del PD.tableOfPosDetailTable) ()
    runDelete conn (del EP.tableOfErrorPosTable) ()
    runDelete conn (del P.tableOfPosTable) ()
    runDelete conn (del PM.tableOfPointManagementTable) ()
    runDelete conn (del IM.tableOfItemMstTable) ()
    runDelete conn (del PS.tableOfPointSystemsTable) ()

    -- insert
    forM_ items $ \i -> runInsert conn IM.insertItemMstTable i
    forM_ poss $ \p -> runInsert conn P.insertPosTable p
    forM_ details $ \d -> runInsert conn PD.insertPosDetailTable d
    forM_ pointSyss $ \p -> runInsert conn PS.insertPointSystemsTable p

    commit conn

items :: [IM.ItemMstTable]
items = map conv
    [ (1, "みかん", 100)
    , (2, "りんご", 120)
    , (3, "ひき肉", 80) --80 yen / 100 g
    , (4, "高級ステーキ", 10000)
    ]
  where
    conv (i, n, p) = IM.ItemMstTable i n p

poss :: [P.PosTable]
poss = map conv
    [ (1, 1, dayFrom)
    , (2, 2, dayFrom)
    , (3, 1, dayTo)
    , (4, 3, dayTo)
    , (5, 1, dayTo)
    , (6, 4, addGregorianMonthsClip (-1) dayTo)
    , (7, 4, addGregorianMonthsClip (-1) dayTo)
    , (8, 4, addGregorianMonthsClip (-1) dayTo)
    , (9, 4, addGregorianMonthsClip (-1) dayTo)
    , (10, 4, addGregorianMonthsClip (-1) dayTo)
    , (11, 4, addGregorianMonthsClip (-1) dayTo)
    , (12, 4, addGregorianMonthsClip (-1) dayTo)
    , (13, 4, addGregorianMonthsClip (-1) dayTo)
    , (14, 4, addGregorianMonthsClip (-1) dayTo)
    , (15, 4, addGregorianMonthsClip (-1) dayTo)
    , (16, 4, addGregorianMonthsClip (-1) dayTo)
    ]
  where
    conv (i, m, d) = P.PosTable i m d

details :: [PD.PosDetailTable]
details = map conv
    -- (id, pos_id, item_id, amount, sales_price, offset)
    [ (1, 1, 1, 3, 100, 0)
    , (2, 1, 3, 10, 80, -10)

    , (3, 2, 2, 1, 120, 30)

    , (4, 3, 1, 3, 80, 20)

    , (5, 4, 1, 3, 100, -10)
    , (6, 4, 2, 3, 120, -20)

    , (7, 5, 4, 2, 10000, -2000)

    , (8, 6, 1, 1, 100, 0)
    , (9, 7, 1, 1, 100, 0)
    , (10, 8, 1, 1, 100, 0)
    , (11, 9, 1, 1, 100, 0)
    , (12, 10, 1, 1, 100, 0)
    , (13, 11, 1, 1, 100, 0)
    , (14, 12, 1, 1, 100, 0)
    , (15, 13, 1, 1, 100, 0)
    , (16, 14, 1, 1, 100, 0)
    , (17, 15, 1, 1, 100, 0)
    , (18, 16, 1, 1, 100, 0)
    ]
  where
    conv (i, p, it, a, s, o) = PD.PosDetailTable i p it a s o

pointSyss :: [PS.PointSystemsTable]
pointSyss = map conv
    [ (1, 1, 3)
    , (2, 1, 6)
    , (3, 1, 9)
    , (4, 2, 1)
    , (5, 2, 5)
    , (6, 3, 8)
    , (7, 4, 10)
    , (8, 4, 11)
    ]
  where
    conv (i, m, p) = PS.PointSystemsTable i m p
