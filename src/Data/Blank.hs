{-# LANGUAGE FlexibleInstances
           , PolyKinds #-}

module Data.Blank where



-- | A class for which anything is an instance.
class Blank a

instance Blank a
