{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}

module PointManagement where

import Database.Record.TH (derivingShow)

import DataSource (defineTable)

defineTable [] "BOOTCAMP" "point_management_table" [derivingShow]
