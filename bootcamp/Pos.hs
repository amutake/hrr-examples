{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}

module Pos where

import Database.Record.TH (derivingShow)

import DataSource (defineTable)

defineTable [] "BOOTCAMP" "pos_table" [derivingShow]
