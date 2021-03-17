{-# LANGUAGE DataKinds
           , KindSignatures
           , PolyKinds
           , TypeFamilies
           , TypeOperators
           , UndecidableInstances #-}

module Data.Type.Ord where

import           Data.Type.Eq
import           Data.Type.Length

import           Data.Type.Bool
import           GHC.TypeLits
import           Prelude



class TypeOrd k where
  type Compare (a :: k) (b :: k) :: Ordering

  infix 4 >
  type (>)  (a :: k) (b :: k) :: Bool
  type a >  b = Compare a b == 'GT

  infix 4 >=
  type (>=) (a :: k) (b :: k) :: Bool
  type a >= b = Compare a b /= 'LT

  infix 4 <
  type (<)  (a :: k) (b :: k) :: Bool
  type a <  b = Compare a b == 'LT

  infix 4 <=
  type (<=) (a :: k) (b :: k) :: Bool
  type a <= b = Compare a b /= 'GT

instance TypeOrd Nat where
  type Compare a b = CmpNat a b

instance TypeOrd Symbol where
  type Compare a b = CmpSymbol a b

instance TypeOrd Ordering where
  type Compare 'GT 'GT = 'EQ
  type Compare 'GT 'EQ = 'GT
  type Compare 'GT 'LT = 'GT
  type Compare 'EQ 'GT = 'LT
  type Compare 'EQ 'EQ = 'EQ
  type Compare 'EQ 'LT = 'GT
  type Compare 'LT 'GT = 'LT
  type Compare 'LT 'EQ = 'LT
  type Compare 'LT 'LT = 'EQ

instance TypeOrd (Maybe a) where
  type Compare ('Just a1) ('Just a2) = Compare a1 a2
  type Compare ('Just _)  'Nothing   = 'GT
  type Compare 'Nothing   ('Just _)  = 'LT
  type Compare 'Nothing   'Nothing   = 'EQ

instance TypeOrd (Either a b) where
  type Compare ('Right a1) ('Right b1) = Compare a1 b1
  type Compare ('Right _)  ('Left _)   = 'GT
  type Compare ('Left _)   ('Right _)  = 'LT
  type Compare ('Left a1)  ('Left b1)  = Compare a1 b1

instance TypeOrd [a] where
  type Compare (a1 ': as) (b1 ': bs) = If (Length as == Length bs)
                                        (Compare (Compare a1 b1) (Compare as bs))
                                        (Compare (Length as) (Length bs))
  type Compare '[]       (_ ': _ )   = 'LT
  type Compare (_ ': _ ) '[]         = 'GT
  type Compare '[]       '[]         = 'EQ

instance TypeOrd (a, b) where
  type Compare '(a1, b1) '(a2, b2) = Compare (Compare a1 a2) (Compare b1 b2)

instance (TypeOrd a, TypeOrd b, TypeOrd c) => TypeOrd (a, b, c) where
  type Compare '(a1, b1, c1) '(a2, b2, c2) =
    Compare (Compare a1 a2) (Compare '(b1, c1) '(b2, c2))

instance (TypeOrd a, TypeOrd b, TypeOrd c, TypeOrd d) => TypeOrd (a, b, c, d) where
  type Compare '(a1, b1, c1, d1) '(a2, b2, c2, d2) =
    Compare (Compare a1 a2) (Compare '(b1, c1, d1) '(b2, c2, d2))

instance (TypeOrd a, TypeOrd b, TypeOrd c, TypeOrd d, TypeOrd e) => TypeOrd (a, b, c, d, e) where
  type Compare '(a1, b1, c1, d1, e1) '(a2, b2, c2, d2, e2) =
    Compare (Compare a1 a2) (Compare '(b1, c1, d1, e1) '(b2, c2, d2, e2))
