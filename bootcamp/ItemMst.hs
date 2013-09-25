{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}

module ItemMst where

import Database.Record.TH (derivingShow)

import DataSource (defineTable)

defineTable [] "BOOTCAMP" "item_mst_table" [derivingShow]
