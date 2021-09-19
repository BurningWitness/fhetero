{-# LANGUAGE DataKinds
           , FlexibleInstances
           , FunctionalDependencies
           , GADTs
           , KindSignatures
           , PolyKinds
           , TypeFamilies
           , TypeOperators
           , UndecidableInstances #-}

module Data.Type.Map.Internal where

import           Data.Type.Length
import           Data.Type.Maybe
import           Data.Type.Ord

import           GHC.TypeLits as Lits
import           Prelude



-- | Type-level Map. Parametrized over a common key, but each element is allowed
--   to have its own type.
data M k where
  B :: k -> a -> M k -> M k -> M k
  T :: M k



type instance Length 'T           = 0
type instance Length ('B _ _ l r) = 1 + Length l + Length r



-- | Describes position of a single element within 'M'.
data Path = L Path
          | I
          | R Path



class Pave (key :: k) (map :: M k) (path :: Path) | key map -> path

instance ( PaveMay key map mayPath
         , Pave' key mayPath path
         )
        => Pave key map path

class Pave' (key :: k) (mayPath :: Maybe Path) (path :: Path) | key mayPath -> path

instance TypeError ( 'Text "Key "
               ':<>: 'ShowType key
               ':<>: 'Text " is not in the map"
                   )
      => Pave' key 'Nothing 'I

instance Pave' key ('Just path) path



class PaveMay (key :: k) (map :: M k) (path :: Maybe Path) | key map -> path

instance PaveMay key 'T 'Nothing

instance ( flag ~ Compare key k
         , PaveMay' flag key l r path
         )
        => PaveMay key ('B k a l r) path

class PaveMay' (flag :: Ordering) (key :: k) (l :: M k) (r :: M k) (path :: Maybe Path)
                | flag key l r -> path

instance PaveMay' 'EQ key l r ('Just 'I)

instance ( PaveMay key l mayOther
         , MapMaybe 'L mayOther mayPath
         )
        => PaveMay' 'LT key l r mayPath

instance ( PaveMay key r mayOther
         , MapMaybe 'R mayOther mayPath
         )
        => PaveMay' 'GT key l r mayPath
