{-# LANGUAGE PatternSynonyms #-}

module Data.FHList
  ( -- * List type
    FHList (.., (:&>:))
    -- * Construction
  , empty
  , singleton
    -- * Basic functions
  , Concat (..)
  , null
  , length
  , Replicate (..)
    -- * List transformations
  , Map (..)
    -- * Folds
  , Fold (..)
    -- ** Monadic
  , Traverse (..)
    -- * Sublists
  , Take (..)
  , Drop (..)
    -- * Searching
    -- ** Searching by equality
  , elem
  , Elem
  , lookup
    -- * Indexing
  , At (..)
    -- * \"Set\" operations
  , Nub (..)
  , delete
  , Retrieve
    -- * Re-exports
  , Proxy (..)
  ) where

import           Data.FHList.Internal

import           Data.Proxy
