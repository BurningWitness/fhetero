{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Type.Eq where

import           Data.Type.Bool
import           Prelude



infix 4 ==
type family (==) (a :: k) (b :: k) :: Bool where
  a == a = 'True
  _ == _ = 'False

infix 4 /=
type a /= b = Not (a == b)
