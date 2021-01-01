{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Type.List where

import           Data.Type.Extra

import           GHC.TypeLits



type instance Length1 [] '[]    = 0
type instance Length1 [] (a:as) = 1 + Length1 [] as



-- | Type-level concatenation.
type family (++) as bs :: [k] where
  '[]    ++ bs = bs
  (a:as) ++ bs = a : as ++ bs
