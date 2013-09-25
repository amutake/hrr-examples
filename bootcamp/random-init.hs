module Main where

import Control.Monad
import Data.Int (Int32)
import Data.Time.Calendar (Day (..))
import qualified Data.Time.Calendar as Days
import System.Random

import Database.HDBC (IConnection (commit), SqlValue)
import Database.HDBC.Record.Insert (prepareInsert, runPreparedInsert)
import Database.HDBC.Record.Query (runQuery)
import Database.HDBC.Session (withConnectionIO, handleSqlError')
import Database.Record (ToSql)
import Database.Relational.Query (typedDelete, restriction)

import DataSource (connect, dayFrom, dayTo)
import qualified Pos
import Pos (PosTable, posTable, insertPosTable)
import qualified PosDetail
import PosDetail (PosDetailTable, posDetailTable, insertPosDetailTable)
import qualified ItemMst
import ItemMst (ItemMstTable, itemMstTable, insertItemMstTable)
import qualified PointManagement
import PointManagement (PointManagementTable, pointManagementTable, insertPointManagementTable)
import qualified ErrorPos
import ErrorPos (ErrorPosTable, errorPosTable, insertErrorPosTable)
import qualified PointSystems
import PointSystems (PointSystemsTable, pointSystemsTable, insertPointSystemsTable)
import Runner

instance Random Day where
  randomR (ModifiedJulianDay lo, ModifiedJulianDay hi) g = (ModifiedJulianDay i, g')
    where
      (i, g') = randomR (lo, hi) g
  random g = (ModifiedJulianDay i, g')
    where
      (i, g') = random g

memberFrom, memberTo :: Int32
memberFrom = 1
memberTo = 100 -- 00

itemFrom, itemTo :: Int32
itemFrom = 1
itemTo = 100 -- 00

posFrom, posTo :: Int32
posFrom = 1
posTo = 10000 -- 000

detailFrom, detailTo :: Int32
detailFrom = 1
detailTo = 100000 -- 000

pointSysFrom, pointSysTo :: Int32
pointSysFrom = 1
pointSysTo = 1000

getRandomPos :: Int32 -> IO PosTable
getRandomPos i = do
  member <- randomRIO (memberFrom, memberTo)
  day <- randomRIO (dayFrom, dayTo)
  return $ Pos.PosTable i member day

getRandomPosDetail :: Int32 -> IO PosDetailTable
getRandomPosDetail i = do
  pos <- randomRIO (posFrom, posTo)
  item <- randomRIO (itemFrom, itemTo)
  amount <- randomRIO (1, 100)
  -- sales <- randomRIO (100, 10000)
  let sales = 10000
  offset <- randomRIO (-1000, 1000)
  return $ PosDetail.PosDetailTable i pos item amount sales offset

getRandomErrorPos :: IO ErrorPosTable
getRandomErrorPos = do
  pos <- randomRIO (posFrom, posTo)
  return $ ErrorPos.ErrorPosTable pos

getRandomItemMst :: Int32 -> IO ItemMstTable
getRandomItemMst i = do
  name <- replicateM 10 $ randomRIO ('a', 'z')
  -- price <- randomRIO (100, 10000)
  price <- randomRIO (9999, 10000)
  return $ ItemMst.ItemMstTable i name price

getRandomPointManagement :: Int32 -> IO PointManagementTable
getRandomPointManagement i = do
  point <- randomRIO (1, 1000)
  date <- randomRIO (dayFrom, dayTo)
  return $ PointManagement.PointManagementTable i point date

getRandomPointSystems :: Int32 -> IO PointSystemsTable
getRandomPointSystems i = do
  member <- randomRIO (memberFrom, memberTo)
  pointSys <- randomRIO (1, 11)
  return $ PointSystems.PointSystemsTable i member pointSys

main :: IO ()
main = handleSqlError' $ withConnectionIO connect $ \conn -> do
  -- delete
  let del t = typedDelete t . restriction $ const $ return ()
  runDelete conn (del PosDetail.tableOfPosDetailTable) ()
  runDelete conn (del ErrorPos.tableOfErrorPosTable) ()
  runDelete conn (del Pos.tableOfPosTable) ()
  runDelete conn (del PointManagement.tableOfPointManagementTable) ()
  runDelete conn (del ItemMst.tableOfItemMstTable) ()
  runDelete conn (del PointSystems.tableOfPointSystemsTable) ()

  -- insert
  forM_ [itemFrom .. itemTo] $ \i -> do
    item <- getRandomItemMst i
    runInsert conn insertItemMstTable item
  -- forM_ [memberFrom .. memberTo] $ \i -> do
  --   member <- getRandomPointManagement i
  --   runInsert conn insertPointManagementTable member
  forM_ [posFrom .. posTo] $ \i -> do
    pos <- getRandomPos i
    runInsert conn insertPosTable pos
  forM_ [detailFrom .. detailTo] $ \i -> do
    detail <- getRandomPosDetail i
    runInsert conn insertPosDetailTable detail
  forM_ [pointSysFrom .. pointSysTo] $ \i -> do
    pointSys <- getRandomPointSystems i
    runInsert conn insertPointSystemsTable pointSys

  commit conn
