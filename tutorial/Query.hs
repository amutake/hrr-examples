module Query
    ( lessThan100yen
    ) where

import Database.Relational.Query

import Item (Item, item)
import qualified Item as I

lessThan100yen :: Relation () String
lessThan100yen = relation $ do
    i <- query item
    wheres $ i ! I.price' .<. value 100
    return (i ! I.itemName')
