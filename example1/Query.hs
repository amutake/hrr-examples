{-# LANGUAGE MonadComprehensions #-}

module Query where

import Data.Int (Int32)
import Database.Relational.Query

import Users

allUsers :: Relation () Users
allUsers = users

findUsersByAgeRange :: Relation (Int32, Int32) Users
findUsersByAgeRange = relation' $ do
    u <- query users
    let age = u ! age'
    (ph, ()) <- placeholder $ \ageRange -> wheres $
        (age .>=. ageRange ! fst') `and'`
        (age .<=. ageRange ! snd')
    asc $ u ! userId'
    return (ph, u)

findUsersByAgeRange' :: Relation (Int32, Int32) Users
findUsersByAgeRange' = relation'
    [ (ph, u)
    | u <- query users
    , let age = u ! age'
    , (ph, ()) <- placeholder $ \ageRange -> wheres $
        (age .>=. ageRange ! fst') `and'`
        (age .<=. ageRange ! snd')
    , () <- asc $ u ! userId'
    ]
