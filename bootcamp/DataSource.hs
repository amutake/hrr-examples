module DataSource
  ( connect
  , defineTable
  , dayFrom, dayTo
  ) where

import Language.Haskell.TH (Q, Dec, TypeQ)
import Language.Haskell.TH.Name.CamelCase (ConName)
import qualified Data.Time.Calendar as D
import Database.HDBC.Query.TH (defineTableFromDB)
import Database.HDBC.PostgreSQL (connectPostgreSQL, Connection)
import Database.HDBC.Schema.Driver (typeMap)
import Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)

connect :: IO Connection
connect = connectPostgreSQL "dbname=testdb"

defineTable :: [(String, TypeQ)] -> String -> String -> [ConName] -> Q [Dec]
defineTable tmap scm tbl derives = do
  defineTableFromDB
    connect
    (driverPostgreSQL { typeMap = tmap })
    scm tbl derives

dayFrom, dayTo :: D.Day
dayFrom = D.fromGregorian 2013 1 1
dayTo = D.fromGregorian 2013 12 31
