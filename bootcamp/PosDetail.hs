{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}

module PosDetail where

import Prelude hiding (id)
import Database.Record.TH (derivingShow)

import DataSource (defineTable)

defineTable [] "BOOTCAMP" "pos_detail_table" [derivingShow]
