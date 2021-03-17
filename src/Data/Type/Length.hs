{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Type.Length where

import           GHC.TypeLits



type family Length (a :: k) :: Nat
