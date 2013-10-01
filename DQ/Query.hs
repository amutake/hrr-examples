module Query where

import Data.Int (Int32)

import Database.Relational.Query

import Family (Family, family)
import qualified Family as F
import Monster (Monster, monster)
import qualified Monster as M

familyMonsters :: Relation () (Family, Monster)
familyMonsters = relation $ do
    f <- query family
    m <- query monster
    on $ just (f ! F.familyId') .=. m ! M.familyId'
    asc $ m ! M.monsterId'
    return (f >< m)

findMonstersByLevelRange :: Relation (Int32, Int32) Monster
findMonstersByLevelRange = relation' $ do
    m <- query monster
    (ph, ()) <- placeholder $ \levelRange -> wheres $
        (m ! M.level' .>=. levelRange ! fst') `and'`
        (m ! M.level' .<=. levelRange ! snd')
    return (ph, m)

findFamilyIdByName :: Relation String Int32
findFamilyIdByName = relation' $ do
    f <- query family
    (ph, ()) <- placeholder $ \name ->
        wheres $ f ! F.familyName' .=. name
    return (ph, f ! F.familyId')

findMonstersByFamilyName :: Relation String Monster
findMonstersByFamilyName = relation' $ do
    m <- query monster
    (ph, i) <- query' findFamilyIdByName
    on $ m ! M.familyId' .=. just i
    return (ph, m)

findBoss :: Relation () Monster
findBoss = relation $ do
    m <- query monster
    wheres $ isNull $ m ! M.familyId'
    return m
