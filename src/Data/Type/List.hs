{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Type.List where

import           Data.Type.Length

import           GHC.TypeLits



type instance Length '[]    = 0
type instance Length (a:as) = 1 + Length as



-- | Type-level concatenation.
type family (++) as bs :: [k] where
  '[]    ++ bs = bs
  (a:as) ++ bs = a : as ++ bs
