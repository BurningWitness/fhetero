{-# LANGUAGE BangPatterns
           , ConstraintKinds
           , DataKinds
           , EmptyCase
           , FlexibleContexts
           , FlexibleInstances
           , FunctionalDependencies
           , GADTs
           , KindSignatures
           , PolyKinds
           , Rank2Types
           , ScopedTypeVariables
           , StandaloneDeriving
           , TypeOperators
           , UndecidableInstances #-}

{-# OPTIONS_HADDOCK not-home #-}

module Data.OneOf.Internal where

import           Data.FHFoldable
import           Data.FHFunctor
import           Data.FHTraversable
import           Data.Type.Map

import           Control.DeepSeq
import           Data.Proxy
import           Data.Type.Eq
import           GHC.TypeLits
import           Prelude hiding (foldl)



-- | A datatype representing a potentially unlimited number of possibilities,
--   elements in each wrapped in @f@.
--
--   Type-wise it is not limited by constructors, instead relying on '(>.<)' and
--   '(>!<)' to constrain 'OneOf' according to the values provided. Therefore
--   failing to mention any single index between 0 and whichever the size you choose
--   will result in an ambiguity type error.
--
--   There also exists no @OneOf f 'T@ on the data level, so you are free to use
--   @undefined@ in those instances.
--
--   Four-way example:
--
--   @
--     f n = case n of
--             1 -> (Proxy :: Proxy 0) '>.<' Just 2
--             2 -> (Proxy :: Proxy 1) '>.<' Just ()
--             3 -> (Proxy :: Proxy 2) '>.<' Nothing
--             _ -> (Proxy :: Proxy 3) '>!<' Just 'a'
--   @
data OneOf f (as :: M ()) where
  SkipL :: OneOf f l -> OneOf f ('B '() a l r)

  SkipR :: OneOf f r -> OneOf f ('B '() a l r)

  Item :: f a -> OneOf f ('B '() a l r)

instance Show (OneOf f 'T) where
  show a = case a of

instance (Show (f a), Show (OneOf f l), Show (OneOf f r)) => Show (OneOf f ('B '() a l r)) where
  showsPrec d (SkipL a) = showParen (d > 10) $ showString "SkipL " . showsPrec 11 a

  showsPrec d (SkipR a) = showParen (d > 10) $ showString "SkipR " . showsPrec 11 a

  showsPrec d (Item v) = showParen (d > 10) $ showString "Item " . showsPrec 11 v

instance ( NFData (OneOf f l)
         , NFData (f a)
         , NFData (OneOf f r)
         )
        => NFData (OneOf f ('B k a l r)) where
  rnf (SkipL l) = rnf l
  rnf (SkipR r) = rnf r
  rnf (Item a) = rnf a

instance NFData (OneOf f 'T) where
  rnf !a = case a of



instance FHFunctor OneOf p 'T where
  fhfmap _ _ a = case a of

instance ( p a
         , FHFunctor OneOf p l
         , FHFunctor OneOf p r
         )
        => FHFunctor OneOf p ('B '() a l r) where
  fhfmap p f (SkipL l) = SkipL $ fhfmap p f l
  fhfmap p f (SkipR r) = SkipR $ fhfmap p f r
  fhfmap _ f (Item a) = Item $ f a


instance FHFoldable OneOf p 'T where
  fhfoldr _ _ = const

instance ( p a
         , FHFoldable OneOf p l
         , FHFoldable OneOf p r
         )
        => FHFoldable OneOf p ('B '() a l r) where
  fhfoldr p f acc (SkipL l) = fhfoldr p f acc l
  fhfoldr p f acc (SkipR r) = fhfoldr p f acc r
  fhfoldr _ f acc (Item a) = f a acc


instance FHTraversable OneOf p 'T where
  fhtraverse _ _ a = case a of

instance ( p a
         , FHTraversable OneOf p l
         , FHTraversable OneOf p r
         )
        => FHTraversable OneOf p ('B '() a l r) where
  fhtraverse p f (SkipL l) = SkipL <$> fhtraverse p f l
  fhtraverse p f (SkipR r) = SkipR <$> fhtraverse p f r
  fhtraverse _ f (Item a) = Item <$> f a



class Fit (n :: Nat) (a :: v) as where
  fit :: Proxy n -> f a -> OneOf f as

instance Fit' (n == 1) (Div n 2) (Mod n 2) a as => Fit n a as where
  fit _ = fit'
            (Proxy :: Proxy (n == 1))
            (Proxy :: Proxy (Div n 2))
            (Proxy :: Proxy (Mod n 2))

class Fit' (zero :: Bool) (n :: Nat) (mod :: Nat) (a :: v) (as :: M ()) where
  fit' :: Proxy zero -> Proxy n -> Proxy mod -> f a -> OneOf f as

instance bs ~ 'B '() a l r => Fit' 'True n m a bs where
  fit' _ _ _ = Item

instance Fit n a l => Fit' 'False n 0 a ('B '() b l r) where
  fit' _ n _ = SkipL . fit n

instance Fit n a r => Fit' 'False n 1 a ('B '() b l r) where
  fit' _ n _ = SkipR . fit n



type Limit n as = Limit' n as as

class Limit' (n :: Nat) (as :: k) (bs :: k) | n as -> bs

instance Limit'' (n == 0) (Div n 2) (n - 1 - Div n 2) as bs => Limit' n as bs

class Limit'' (zero :: Bool) (ln :: Nat) (rn :: Nat) (as :: k) (bs :: k)
               | zero ln rn as -> bs

instance as ~ 'T => Limit'' 'True ln rn as 'T

instance ( as ~ 'B k a l r
         , Limit' ln l bl
         , Limit' rn r br
         )
        => Limit'' 'False ln rn as ('B k a bl br)

-- | Constructs an element of 'OneOf'. Indices start at @0@.
(>.<) :: Fit (n + 1) a as => Proxy n -> f a -> OneOf f as
(>.<) (Proxy :: Proxy n) = fit (Proxy :: Proxy (n + 1))

-- | Same as '(>.<)', but this also assumes this elements index is the largest
--   in the data structure.
(>!<) :: (Fit (n + 1) a as, Limit (n + 1) as) => Proxy n -> f a -> OneOf f as
(>!<) = (>.<)

-- | Limits a 'OneOf' to holding @n@ elements indexed @[ 0 .. n - 1 ]@. Can be
--   used instead of putting '(>!<)' at the largest element.
limit :: Limit n as => Proxy n -> OneOf f as -> OneOf f as
limit _ = id
