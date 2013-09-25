{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}

module ErrorPos where

import Database.Record.TH (derivingShow)

import DataSource (defineTable)

defineTable [] "BOOTCAMP" "error_pos_table" [derivingShow]
