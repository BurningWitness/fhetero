{- | = Finite type-level maps (lazy interface)
     
     The @'HMap' (t :: k -> *) (vs :: M s)@ represents a finite heterogenous map
     (sometimes called a dictionary) from keys of kind 's' (comparable via
     'Data.Type.Ord.TypeOrd')
     to values of arbitrary types of kind @k@, each wrapped into @t@.

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
    -- | This can most definitely be ported info a function of type
    --   @FHList Identity as -> FHMap f bs@, where @as ~ ([(Proxy k, f bs)] :: *)@,
    --   but it leads to an atrocious six type class ladder.
    --
    --   You should probably just make a type class that loops 'insert's instead, which
    --   won't lock you into 2-tuples with 'Proxy'.
    --
    --   If you really think you need that code it's at commit
    --   @f9080719a75eb32cf608fc862fd2a1d9b09a4fae@.

    -- * Insertion
  , Insert
  , insert
  , InsertUnique
  , insertUnique
  , InsertId
  , insertId
    -- * Deletion/Update
  , delete
  , Delete'
  , delete'
  , Adjust
  , adjust
  , Adjust'
  , adjust'
    -- * Query
    -- ** Lookup
  , Lookup
  , lookup
  , (!)
  , LookupMay
  , lookupMay
  , (!?)
  , Member
  , member
    -- ** Size
  , null
  , size
    -- * Combine
    -- ** Union
  , Union
  , union
  , UnionUnique
  , unionUnique
  , UnionId
  , unionId
    -- ** Difference
  , Difference (..)
  , (\\)
    -- ** Intersection
  , Intersection (..)
    -- * Traversal
    -- ** Map
  , Map
  , map
  , MapWithKey (..)
  , Traverse
  , traverse
  , TraverseWithKey (..)
  , traverseWithKey_
    -- * Folds
  , Blank
  , Fold
  , foldr
  , foldl
  , foldrWithKey
  , foldlWithKey
  , foldMapWithKey
  , FoldWithKey
    -- ** Strict folds
  , foldr'
  , foldl'
  , foldrWithKey'
  , foldlWithKey'
    -- * Conversion
    -- ** Lists
  , toList
    -- ** Ordered lists
  , ToAscList
  , toAscList
  , ToDescList
  , toDescList
    -- * Re-exports
  , module Data.Type.Map
  , Proxy (..)
  , Void
  ) where

import           Data.FHMap.Internal
import           Data.Type.Map

import           Data.Proxy
import           Data.Void
