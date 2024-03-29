{-# LANGUAGE ConstraintKinds
           , DataKinds
           , FlexibleInstances
           , FunctionalDependencies
           , GADTs
           , MultiParamTypeClasses
           , PolyKinds
           , Rank2Types
           , ScopedTypeVariables
           , StandaloneDeriving
           , UndecidableInstances #-}

module Data.Type.Maybe
  ( TypeMaybe (..)
  , FromTypeMaybe (..)
  , MapMaybe
  , IsJust
  ) where

import           Data.Proxy
import           Prelude



data TypeMaybe (f :: k -> *) (a :: Maybe k) where
  TypeJust :: f a -> TypeMaybe f ('Just a)

  TypeNothing :: TypeMaybe f 'Nothing

class Elevate p f a where
  elevate
    :: Proxy p
    -> (forall b. p (f b) => arg -> f b -> c)
    -> c
    -> arg
    -> TypeMaybe f a
    -> c

  elevate2
    :: Proxy p
    -> (forall b. p (f b) => f b -> f b -> c)
    -> c
    -> TypeMaybe f a
    -> TypeMaybe f a
    -> c

instance p (f a) => Elevate p f ('Just a) where
  elevate _ f _ arg (TypeJust a) = f arg a

  elevate2 _ f _ (TypeJust a) (TypeJust b) = f a b

instance Elevate p f 'Nothing where
  elevate _ _ c _ _ = c

  elevate2 _ _ c _ _ = c

instance Elevate Show f a => Show (TypeMaybe f a) where
  showsPrec = elevate (Proxy :: Proxy Show)
                (\d a -> showParen (d > 10) $ showString "TypeJust " . showsPrec 11 a)
                (showString "TypeNothing")

instance Elevate Eq f a => Eq (TypeMaybe f a) where
  (==) = elevate2 (Proxy :: Proxy Eq) (==) True

instance (Elevate Eq f a, Elevate Ord f a) => Ord (TypeMaybe f a) where
  compare = elevate2 (Proxy :: Proxy Ord) compare EQ



class FromTypeMaybe (a :: v) (b :: Maybe v) (c :: v) | a b -> c where
  fromTypeMaybe :: f a -> TypeMaybe f b -> f c

instance FromTypeMaybe a ('Just b) b where
  fromTypeMaybe _ (TypeJust a) = a

instance FromTypeMaybe a 'Nothing a where
  fromTypeMaybe a _ = a



class MapMaybe (f :: x -> y) (a :: Maybe x) (b :: Maybe y) | f a -> b

instance MapMaybe f 'Nothing 'Nothing

instance MapMaybe f ('Just a) ('Just (f a))



class IsJust (a :: Maybe x) (b :: Bool) | a -> b

instance IsJust 'Nothing 'False

instance IsJust ('Just a) 'True
