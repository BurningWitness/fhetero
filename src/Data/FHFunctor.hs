{-# LANGUAGE ConstraintKinds
           , MultiParamTypeClasses
           , PolyKinds
           , Rank2Types #-}

module Data.FHFunctor where

import           Data.Proxy



class FHFunctor h p as where
  fhfmap
    :: Proxy p
    -> (forall a. p a => f a -> g a)
    -> h f as
    -> h g as
