{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}

module PointSystems where

import Prelude hiding (id)

import Data.Int (Int32)
import Database.Record.TH (derivingShow)

import DataSource (defineTable)

calcPoint :: Int32 -> Int32 -> Int32 -> Float
calcPoint 1 _ n = fromIntegral n * 0.01
calcPoint 2 _ n = fromIntegral n * 0.02
calcPoint 3 _ n = fromIntegral n * 0.04
calcPoint 4 _n n
    | n <= 10000 = calcPoint 1 _n n
    | otherwise = calcPoint 1 _n 10000 + calcPoint 2 _n (n - 10000)
calcPoint 5 _n n
    | n <= 10000 = calcPoint 1 _n n
    | otherwise = calcPoint 1 _n 10000 + fromIntegral (n - 10000) * 0.03
calcPoint 6 _n n
    | n <= 10000 = calcPoint 2 _n n
    | otherwise = calcPoint 2 _n 10000 + fromIntegral (n - 10000) * 0.03
calcPoint 7 _ n = fromIntegral $ (n `div` 1000) * 10
calcPoint 8 _ n = fromIntegral $ (n `div` 1000) * 11
calcPoint 9 _ n = fromIntegral $ (n `div` 1000) * 12
calcPoint 10 n _ = fromIntegral n
calcPoint 11 n _ = fromIntegral $ (n `div` 10) * 5
calcPoint n _ _ = error $ "Undefined point system number: " ++ show n

defineTable [] "BOOTCAMP" "point_systems_table" [derivingShow]
