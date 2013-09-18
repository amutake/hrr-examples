{-# LANGUAGE MonadComprehensions #-}

module Query where

import Database.Relational.Query (Relation, relation, query)

import Users (Users, users)

allUsers :: Relation () Users
allUsers = relation $ query users
