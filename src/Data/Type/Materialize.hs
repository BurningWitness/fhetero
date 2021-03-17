{-# LANGUAGE DataKinds
           , FlexibleInstances
           , FunctionalDependencies
           , KindSignatures
           , PolyKinds
           , ScopedTypeVariables #-}

module Data.Type.Materialize where

import           Data.Bool
import           Data.Proxy
import           Prelude



-- | Class of kinds that can be transported to the data level.
class Materialize a (k :: a) | k -> a where
  materialize :: Proxy k -> a


instance Materialize () '() where
  materialize _ = ()


instance Materialize Bool 'False where
  materialize _ = False

instance Materialize Bool 'True where
  materialize _ = True


instance Materialize Ordering 'GT where
  materialize _ = GT

instance Materialize Ordering 'EQ where
  materialize _ = EQ

instance Materialize Ordering 'LT where
  materialize _ = LT


instance Materialize a k => Materialize (Maybe a) ('Just k) where
  materialize _ = Just $ materialize (Proxy :: Proxy k)

instance Materialize (Maybe a) 'Nothing where
  materialize _ = Nothing


instance Materialize a k => Materialize (Either a b) ('Left k) where
  materialize _ = Left $ materialize (Proxy :: Proxy k)

instance Materialize b l => Materialize (Either a b) ('Right l) where
  materialize _ = Right $ materialize (Proxy :: Proxy l)
