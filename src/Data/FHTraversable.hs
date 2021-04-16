{-# LANGUAGE ConstraintKinds
           , MultiParamTypeClasses
           , PolyKinds
           , Rank2Types #-}

module Data.FHTraversable where

import           Data.Proxy

import           Prelude



class FHTraversable h p as where
  fhtraverse
    :: Applicative m
    => Proxy p
    -> (forall a. p a => f a -> m (g a))
    -> h f as
    -> m (h g as)
