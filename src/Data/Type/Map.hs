{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Map where

import           Data.Type.Extra

import           GHC.TypeLits as Lits



-- | Type-level Map. Parametrized over a common key, but each element is allowed
--   to have its own type.
data M k where
  B :: k -> a -> M k -> M k -> M k
  T :: M k

type instance Length1 M 'T           = 0
type instance Length1 M ('B _ _ l r) = 1 + Length1 M l + Length1 M r
