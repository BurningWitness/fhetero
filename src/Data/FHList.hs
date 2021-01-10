{-# LANGUAGE PatternSynonyms #-}

module Data.FHList
  ( -- * List type
    FHList (.., (:&>:))
    -- * Construction
  , empty
  , singleton
    -- * Basic functions
  , (++)
  , null
  , length
    -- * List transformations
  , map
    -- * Folds
  , foldl
  , foldl'
  , foldr
  , foldr'
  , foldMap
    -- ** Monadic
  , foldlM
  , foldrM
  , traverse_
  , for_
    -- * Sublists
  , take
  , drop
    -- * Searching
  , elem
    -- * Indexing
  , (!!)
    -- * \"Set\" operations
  , nub
  , delete
  ) where

import           Data.FHList.Internal
