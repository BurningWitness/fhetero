{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Extra where

import           Data.Bool
import           Data.Functor.Identity
import           Data.Ord
import           Data.Proxy
import           Data.Type.Bool
import           GHC.TypeLits



-- | Class of kinds that can be transported to the data level. This can probably be
--   done through "Data.Data", but we only need variants for 'Bool', so who cares.
class Materialize a (k :: a) | k -> a where
  materialize :: Proxy k -> a

instance Materialize Bool 'False where
  materialize _ = False

instance Materialize Bool 'True where
  materialize _ = True



-- | Container length for single-argument type constructors.
type family Length1 (f :: i -> *) (ks :: f (k :: i)) :: Nat

type family NoIdentity m a :: * where
  NoIdentity Identity a = a
  NoIdentity m        a = m a



-- * Comparison operators

infix 4 ==
type family (==) (a :: k) (b :: k) :: Bool where
  a == a = 'True
  _ == _ = 'False

infix 4 /=
type a /= b = Not (a == b)

infix 4 <
type a <  b = Not (a >= b)

infix 4 <=
type a <= b = Not (a > b)

class Comp k where
  infix 4 >
  type (>)  (a :: k) (b :: k) :: Bool

  infix 4 >=
  type (>=) (a :: k) (b :: k) :: Bool

instance Comp Nat where
  type a >  b = a `CmpNat` b == 'GT
  type a >= b = a `CmpNat` b /= 'LT

instance Comp Symbol where
  type a >  b = a `CmpSymbol` b == 'GT
  type a >= b = a `CmpSymbol` b /= 'LT
