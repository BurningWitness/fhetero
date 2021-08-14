{-# LANGUAGE ConstraintKinds
           , DataKinds
           , FlexibleContexts
           , FlexibleInstances
           , GADTs
           , KindSignatures
           , MultiParamTypeClasses
           , Rank2Types
           , ScopedTypeVariables
           , UndecidableInstances #-}

module Data.FHDeep
  ( module Data.FHDeep
  , module Data.Type.Deep
  ) where

import           Data.FHList (FHList (..))
import qualified Data.FHList as FHList
import           Data.FHFoldable
import           Data.FHFunctor
import           Data.FHTraversable
import           Data.Type.Deep

import           Data.Proxy
import           Prelude hiding (map, foldl, foldMap, foldr, traverse, zip, zip3)



-- | A heterogenous list of arbitrary depth.
data FHDeep f (as :: Depth) where
  FHShallow :: f a -> FHDeep f ('S a)

  FHDeep :: FHList (FHDeep f) as -> FHDeep f ('D as)

instance Show (f a) => Show (FHDeep f ('S a)) where
  showsPrec n (FHShallow a) =
    showParen (n > 10) $ showString "Shallow " . showsPrec 11 a

instance Show (FHList (FHDeep f) as) => Show (FHDeep f ('D as)) where
  showsPrec n (FHDeep as) =
    showParen (n > 10) $ showString "Deep " . showsPrec 11 as



instance Map p as => FHFunctor FHDeep p as where
  fhfmap = map

class Map p as where
  map :: Proxy p -> (forall a. p a => f a -> g a) -> FHDeep f as -> FHDeep g as

instance p a => Map p ('S a) where
  map _ f (FHShallow a) = FHShallow $ f a

instance FHList.Map (Map p) as => Map p ('D as) where
  map p f (FHDeep as) =
    FHDeep $ FHList.map (Proxy :: Proxy (Map p)) (map p f) as




instance Fold p as => FHFoldable FHDeep p as where
  fhfoldMap = foldMap
  fhfoldl = foldl
  fhfoldl' = foldl'
  fhfoldr = foldr
  fhfoldr' = foldr'

class Fold p as where
  foldMap :: Monoid m => Proxy p -> (forall a. p a => f a -> m) -> FHDeep f as -> m

  foldr :: Proxy p -> (forall a. p a => f a -> b -> b) -> b -> FHDeep f as -> b

  foldr' :: Proxy p -> (forall a. p a => f a -> b -> b) -> b -> FHDeep f as -> b

  foldl :: Proxy p -> (forall a. p a => b -> f a -> b) -> b -> FHDeep f as -> b

  foldl' :: Proxy p -> (forall a. p a => b -> f a -> b) -> b -> FHDeep f as -> b

  zip
    :: Proxy p
    -> (forall a. p a => f a -> g a -> h a)
    -> FHDeep f as
    -> FHDeep g as
    -> FHDeep h as

  zip3
    :: Proxy p
    -> (forall a. p a => f a -> g a -> h a -> i a)
    -> FHDeep f as
    -> FHDeep g as
    -> FHDeep h as
    -> FHDeep i as

  zip4
    :: Proxy p
    -> (forall a. p a => f a -> g a -> h a -> i a -> j a)
    -> FHDeep f as
    -> FHDeep g as
    -> FHDeep h as
    -> FHDeep i as
    -> FHDeep j as

  zip5
    :: Proxy p
    -> (forall a. p a => f a -> g a -> h a -> i a -> j a -> l a)
    -> FHDeep f as
    -> FHDeep g as
    -> FHDeep h as
    -> FHDeep i as
    -> FHDeep j as
    -> FHDeep l as

  zip6
    :: Proxy p
    -> (forall a. p a => f a -> g a -> h a -> i a -> j a -> l a -> m a)
    -> FHDeep f as
    -> FHDeep g as
    -> FHDeep h as
    -> FHDeep i as
    -> FHDeep j as
    -> FHDeep l as
    -> FHDeep m as

  zip7
    :: Proxy p
    -> (forall a. p a => f a -> g a -> h a -> i a -> j a -> l a -> m a -> n a)
    -> FHDeep f as
    -> FHDeep g as
    -> FHDeep h as
    -> FHDeep i as
    -> FHDeep j as
    -> FHDeep l as
    -> FHDeep m as
    -> FHDeep n as

instance p a => Fold p ('S a) where
  foldMap _ f (FHShallow a) = f a

  foldr _ f t (FHShallow a) = f a t

  foldr' _ f t (FHShallow a) = id $! f a t

  foldl _ f t (FHShallow a) = f t a

  foldl' _ f t (FHShallow a) = id $! f t a

  zip _ f (FHShallow a) (FHShallow b) = FHShallow $ f a b

  zip3 _ f (FHShallow a) (FHShallow b) (FHShallow c) = FHShallow $ f a b c

  zip4 _ f (FHShallow a) (FHShallow b) (FHShallow c) (FHShallow d) = FHShallow $ f a b c d

  zip5 _ f (FHShallow a) (FHShallow b) (FHShallow c) (FHShallow d) (FHShallow e) =
    FHShallow $ f a b c d e

  zip6 _ f (FHShallow a) (FHShallow b) (FHShallow c) (FHShallow d) (FHShallow e) (FHShallow g) =
    FHShallow $ f a b c d e g

  zip7 _ f (FHShallow a) (FHShallow b) (FHShallow c) (FHShallow d) (FHShallow e) (FHShallow g)
              (FHShallow h) =
    FHShallow $ f a b c d e g h

instance FHList.Fold (Fold p) as => Fold p ('D as) where
  foldMap p f (FHDeep as) = FHList.foldMap (Proxy :: Proxy (Fold p)) (foldMap p f) as

  foldr p f t (FHDeep as) = FHList.foldr (Proxy :: Proxy (Fold p)) (flip $ foldr p f) t as

  foldr' p f t (FHDeep as) = FHList.foldr' (Proxy :: Proxy (Fold p)) (flip $ foldr' p f) t as

  foldl p f t (FHDeep as) = FHList.foldl (Proxy :: Proxy (Fold p)) (foldl p f) t as

  foldl' p f t (FHDeep as) = FHList.foldl' (Proxy :: Proxy (Fold p)) (foldl' p f) t as

  zip p f (FHDeep as) (FHDeep bs) =
    FHDeep $ FHList.zip (Proxy :: Proxy (Fold p)) (zip p f) as bs

  zip3 p f (FHDeep as) (FHDeep bs) (FHDeep cs) =
    FHDeep $ FHList.zip3 (Proxy :: Proxy (Fold p)) (zip3 p f) as bs cs

  zip4 p f (FHDeep as) (FHDeep bs) (FHDeep cs) (FHDeep ds) =
    FHDeep $ FHList.zip4 (Proxy :: Proxy (Fold p)) (zip4 p f) as bs cs ds

  zip5 p f (FHDeep as) (FHDeep bs) (FHDeep cs) (FHDeep ds) (FHDeep es) =
    FHDeep $ FHList.zip5 (Proxy :: Proxy (Fold p)) (zip5 p f) as bs cs ds es

  zip6 p f (FHDeep as) (FHDeep bs) (FHDeep cs) (FHDeep ds) (FHDeep es) (FHDeep gs) =
    FHDeep $ FHList.zip6 (Proxy :: Proxy (Fold p)) (zip6 p f) as bs cs ds es gs

  zip7 p f (FHDeep as) (FHDeep bs) (FHDeep cs) (FHDeep ds) (FHDeep es) (FHDeep gs) (FHDeep hs) =
    FHDeep $ FHList.zip7 (Proxy :: Proxy (Fold p)) (zip7 p f) as bs cs ds es gs hs



instance Traverse p as => FHTraversable FHDeep p as where
  fhtraverse = traverse

class Traverse p as where
  traverse
    :: Applicative m
    => Proxy p
    -> (forall a. p a => f a -> m (g a))
    -> FHDeep f as
    -> m (FHDeep g as)

  mapAccumL :: Proxy p -> (forall a. p a => b -> f a -> (b, g a)) -> b -> FHDeep f as -> (b, FHDeep g as)

  mapAccumR :: Proxy p -> (forall a. p a => b -> f a -> (b, g a)) -> b -> FHDeep f as -> (b, FHDeep g as)

instance p a => Traverse p ('S a) where
  traverse _ f (FHShallow a) = FHShallow <$> f a

  mapAccumL _ f acc (FHShallow a) = let (acc', a') = f acc a
                                    in (acc', FHShallow a')

  mapAccumR _ f acc (FHShallow a) = let (acc', a') = f acc a
                                    in (acc', FHShallow a')

instance (FHList.Traverse (Traverse p) as) => Traverse p ('D as) where
  traverse p f (FHDeep as) = FHDeep <$> FHList.traverse (Proxy :: Proxy (Traverse p)) (traverse p f) as

  mapAccumL p f acc (FHDeep as) =
    FHDeep <$> FHList.mapAccumL (Proxy :: Proxy (Traverse p)) (mapAccumL p f) acc as

  mapAccumR p f acc (FHDeep as) =
    FHDeep <$> FHList.mapAccumR (Proxy :: Proxy (Traverse p)) (mapAccumR p f) acc as
