module Util where

import Control.Applicative ((<$>))
import Control.Monad (forM, when)
import Data.Int (Int32)
import Data.Maybe (catMaybes)
import Data.List (partition, find)
import Data.Time.Calendar (Day, toGregorian, fromGregorian, addGregorianMonthsClip)
import Data.Time.Clock (utctDay, getCurrentTime)

import Database.HDBC (IConnection)
import Database.Relational.Query

import DataSource (dayFrom, dayTo)
import Query1
import Runner
import qualified Pos as P
import qualified PosDetail as PD
import qualified ItemMst as IM
import qualified PointManagement as PM
import qualified ErrorPos as EP

-- | 年と月を入力し、その月の一日と翌月の一日を返す
getFromTo :: IO (Day, Day)
getFromTo = do
    putStr $ "year (" ++ show yearFrom ++ " - " ++ show yearTo ++ "): "
    year <- read <$> getLine
    if year < yearFrom || yearTo < year
        then putStrLn "invalid year" >> getFromTo
        else do
            putStr $ "month (" ++ show monthFrom ++ " - " ++ show monthTo ++ "): "
            month <- read <$> getLine
            if month < monthFrom || monthTo < month
                then putStrLn "invalid month" >> getFromTo
                else do
                    let from = fromGregorian year month 1
                        to = addGregorianMonthsClip 1 from
                    return (from, to)
  where
    (yearFrom, monthFrom', _) = toGregorian dayFrom
    (yearTo, monthTo', _) = toGregorian dayTo
    (monthFrom, monthTo)
        | yearFrom == yearTo = (monthFrom', monthTo')
        | otherwise = (1, 12)

logger :: String -> IO ()
logger = appendFile "bootcamp.log"

-- | 商品マスタの定価と販売額が一致するかどうか
checkPrice :: [IM.ItemMstTable] -> PD.PosDetailTable -> Bool
checkPrice itemMsts detail =
    case find (\item -> IM.itemId item == PD.itemId detail) itemMsts of
        Just item -> IM.tagPrice item == PD.salesPrice detail
        Nothing -> error $ "Missing item: POSID: " ++ show (PD.posId detail)

-- | POSID と 会員ID を受け取って、(会員ID, 合計購入額) を返す
--   商品マスタの定価と販売額が一致しない場合は、error_pos_table に書きこんで Nothing を返す
memberTotal :: IConnection conn => conn -> (Int32, Int32) -> IO (Maybe (Int32, Int32))
memberTotal conn (posId, memberId) = do
    details <- runRelation conn findPosDetail (posId, posId)
    when (null details) $ logger errMsg >> error errMsg
    itemMsts <- runRelation conn findItemMst ()
    if and $ map (checkPrice itemMsts) details
        then do
            let total = sum $ map calcPrice details
            return $ Just (memberId, total)
        else do
            runInsert conn insertErrorPos $
                EP.ErrorPosTable { EP.posId = posId }
            return Nothing
  where
    calcPrice d = PD.amount d * (PD.salesPrice d + PD.offsetPrice d)
    errMsg = "Missing pos detail: POSID: " ++ show posId ++ "\n"

-- | point_management_table に書き込む
putPoint :: IConnection conn => conn -> (Int32, Int32) -> IO ()
putPoint conn (memberId, total) = do
    today <- utctDay <$> getCurrentTime
    runInsert conn insertPointManagement $ PM.PointManagementTable
        { PM.memberId = memberId
        , PM.managedPoint = total
        , PM.issueDate = today
        }
