{-# LANGUAGE DataKinds
           , GADTs
           , PolyKinds
           , TypeFamilies
           , TypeOperators
           , UndecidableInstances #-}

module Data.Type.Map where

import           Data.Type.Length

import           GHC.TypeLits as Lits



-- | Type-level Map. Parametrized over a common key, but each element is allowed
--   to have its own type.
data M k where
  B :: k -> a -> M k -> M k -> M k
  T :: M k



type instance Length 'T           = 0
type instance Length ('B _ _ l r) = 1 + Length l + Length r
