{- | = Finite type-level maps (lazy interface)
     
     The @'HMap' (t :: k -> *) (vs :: M Symbol)@ represents a finite heterogenous map
     (sometimes called a dictionary) from keys of kind 'GHC.TypeLits.Symbol' to values
     of arbitrary types of kind @k@, each wrapped into @t@.

     These modules are intended to be imported qualified, to avoid name
     clashes with Prelude or
     [@containers@](https://hackage.haskell.org/package/containers)
     functions, e.g.

     >  import qualified Data.Map.Hetero.Lazy as HMap

     Note that the implementation is generally left-biased. Functions that take
     two maps as arguments and combine them, such as 'union' and 'intersection',
     prefer the values in the first argument to those in the second.

     Every element stored will be visible on the type level, so you might
     wish to use
     [@PartialTypeSignatures@](https://gitlab.haskell.org/ghc/ghc/-/wikis/partial-type-signatures)
     (together with @-fno-warn-partial-type-signatures@ if you happen to be a fan
     of @-Wall@) to wildcard such problematic types.

     = Non-warning

     The size of the map is allowed to exceed @'Prelude.maxBound'::'Data.Int.Int'@
     since 'GHC.TypeLits.Nat's are not bounded by 'Data.Int.Int'.

     = Implementation

     The implementation of 'HMap' is a shameless translation of functions from
     @Data.Map.Lazy@ from the
     [containers](https://hackage.haskell.org/package/containers)
     package into type class definitions with results based on functional dependencies.

-}

module Data.FHMap
  ( -- * Map type
    FHMap (..)
    -- * Construction
  , empty
  , singleton
    -- ** From unordered lists
  , fromList
    -- * Insertion
  , insert
  , insertUnique
  , insertId
    -- * Deletion/Update
  , delete
  , delete'
  , adjust
  , adjust'
    -- * Query
    -- ** Lookup
  , lookup
  , (!)
  , lookupMay
  , (!?)
  , member
    -- ** Size
  , null
  , size
    -- * Combine
    -- ** Union
  , union
  , unionUnique
  , unionId
    -- ** Difference
  , difference
  , (\\)
    -- ** Intersection
  , intersection
    -- * Traversal
    -- ** Map
  , map
  , mapWithKey
  , traverse
  , traverseWithKey
    -- * Folds
  , foldr
  , foldl
  , foldrWithKey
  , foldlWithKey
  , foldMapWithKey
    -- ** Strict folds
  , foldr'
  , foldl'
  , foldrWithKey'
  , foldlWithKey'
    -- * Conversion
    -- ** Lists
  , toList
    -- ** Ordered lists
  , toAscList
  , toDescList
    -- * Re-exports
  , module Data.Type.Map
  , module Data.Proxy
  , module Data.Void
  ) where

import           Data.FHMap.Internal
import           Data.Type.Map

import           Data.Proxy
import           Data.Void
