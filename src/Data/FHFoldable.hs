{-# LANGUAGE ConstraintKinds
           , MultiParamTypeClasses
           , PolyKinds
           , Rank2Types #-}

module Data.FHFoldable where

import           Data.Proxy

import           Data.Semigroup (Dual (..), Endo (..))
import           Prelude



class FHFoldable g p as where
  {-# MINIMAL fhfoldMap | fhfoldr #-}

  fhfoldMap :: Monoid m => Proxy p -> (forall a. p a => f a -> m) -> g f as -> m
  fhfoldMap p f = fhfoldr p (mappend . f) mempty

  fhfoldr :: Proxy p -> (forall a. p a => f a -> b -> b) -> b -> g f as -> b
  fhfoldr p f z t = appEndo (fhfoldMap p (Endo . f) t) z

  fhfoldr' :: Proxy p -> (forall a. p a => f a -> b -> b) -> b -> g f as -> b
  fhfoldr' p f z0 xs = fhfoldl p (\k x z -> k $! f x z) id xs z0

  fhfoldl :: Proxy p -> (forall a. p a => b -> f a -> b) -> b -> g f as -> b
  fhfoldl p f z t = appEndo (getDual (fhfoldMap p (Dual . Endo . flip f) t)) z

  fhfoldl' :: Proxy p -> (forall a. p a => b -> f a -> b) -> b -> g f as -> b
  fhfoldl' p f z0 xs = fhfoldr p (\x k z -> k $! f z x) id xs z0



fhfoldrM
  :: (FHFoldable g p as, Monad m)
  => Proxy p
  -> (forall a. p a => f a -> b -> m b)
  -> b
  -> g f as
  -> m b
fhfoldrM p f z0 xs = fhfoldl p (\k x z -> f x z >>= k) return xs z0

fhfoldlM
  :: (FHFoldable g p as, Monad m)
  => Proxy p
  -> (forall a. p a => b -> f a -> m b)
  -> b
  -> g f as
  -> m b
fhfoldlM p f z0 xs = fhfoldr p (\x k z -> f z x >>= k) return xs z0

fhtraverse_
  :: (FHFoldable g p as, Applicative m)
  => Proxy p
  -> (forall a. p a => f a -> m ())
  -> g f as
  -> m ()
fhtraverse_ p f = fhfoldr p (\x -> (f x *>)) $ pure ()

fhfor_
  :: (FHFoldable g p as, Applicative m)
  => Proxy p
  -> g f as
  -> (forall a. p a => f a -> m ())
  -> m ()
fhfor_ p f d = fhtraverse_ p d f
