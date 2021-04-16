{-# LANGUAGE ConstraintKinds
           , DataKinds
           , DerivingStrategies
           , FlexibleContexts
           , FlexibleInstances
           , FunctionalDependencies
           , GADTs
           , GeneralizedNewtypeDeriving
           , KindSignatures
           , PartialTypeSignatures
           , PolyKinds
           , Rank2Types
           , ScopedTypeVariables
           , StandaloneDeriving
           , TypeFamilies
           , TypeFamilyDependencies
           , TypeOperators
           , UndecidableInstances #-}

{-# OPTIONS_HADDOCK not-home #-}

module Data.FHDecision.Internal where

import           Data.Type.Decision
import           Data.FHFoldable
import           Data.FHFunctor
import           Data.FHList hiding (Fold (..), Traverse (..))
import qualified Data.FHList as FHList
import           Data.FHMap (M (..))
import           Data.OneOf

import           Control.Monad hiding ((>>))
import           Data.Coerce
import           GHC.TypeLits
import           Prelude hiding ((>>))



data FHSingle (f :: k -> *) (as :: Single) where
  FHAction    :: f a -> FHSingle f ('Action a)

  FHDataChain :: [FHDecision f as] -> FHSingle f ('DChain as)

  FHDataChoice
    :: OneOf (FHDecision f) as
    -> FHSingle f ('DChoice as)

  FHTypeChoice
    :: Proxy cond
    -> FHDecision f tru
    -> FHDecision f fal
    -> FHSingle f ('TChoice cond tru fal)

instance Show (f a) => Show (FHSingle f ('Action a)) where
  showsPrec d (FHAction a) = showParen (d > 10) $ showString "FHAction " . showsPrec 11 a

instance Show (FHDecision f a) => Show (FHSingle f ('DChain a)) where
  showsPrec d (FHDataChain a) = showParen (d > 10) $
                                    showString "FHDataChain "
                                  . showsPrec 11 a

instance Show (OneOf (FHDecision f) as) => Show (FHSingle f ('DChoice as)) where
  showsPrec d (FHDataChoice choice) =
      showParen (d > 10) $ showString "FHDataChoice " . showsPrec 11 choice

instance (Show (FHDecision f a), Show (FHDecision f b))
      => Show (FHSingle f ('TChoice cond a b)) where
  showsPrec d (FHTypeChoice cond tru fal) =
      showParen (d > 10) $
          showString "FHTypeChoice "
        . showsPrec 11 cond
        . showChar ' '
        . showsPrec 11 tru
        . showChar ' '
        . showsPrec 11 fal


instance p a => FHFunctor FHSingle p ('Action a) where
  fhfmap _ f (FHAction a) = FHAction $ f a

instance FHFunctor FHDecision p as
      => FHFunctor FHSingle p ('DChain as) where
  fhfmap p f (FHDataChain as) =
    FHDataChain $ fmap (fhfmap p f) as

instance FHFunctor OneOf (FHFunctor FHDecision p) as
      => FHFunctor FHSingle p ('DChoice as) where
  fhfmap p f (FHDataChoice choice) =
    FHDataChoice $ fhfmap (Proxy :: Proxy (FHFunctor FHDecision p)) (fhfmap p f) choice

instance ( FHFunctor FHDecision p a
         , FHFunctor FHDecision p b
         )
      => FHFunctor FHSingle p ('TChoice cond a b) where
  fhfmap p f (FHTypeChoice cond tru fal) =
    FHTypeChoice cond (fhfmap p f tru) (fhfmap p f fal)


instance p a => FHFoldable FHSingle p ('Action a) where
  fhfoldr _ f acc (FHAction a) = f a acc

instance FHFoldable FHDecision p as
      => FHFoldable FHSingle p ('DChain as) where
  fhfoldr p f acc (FHDataChain as) =
    foldr (flip $ fhfoldr p f) acc as

instance FHFoldable OneOf (FHFoldable FHDecision p) as
      => FHFoldable FHSingle p ('DChoice as) where
  fhfoldr p f acc (FHDataChoice choice) =
    fhfoldr (Proxy :: Proxy (FHFoldable FHDecision p)) (flip $ fhfoldr p f) acc choice

instance FHFoldable FHDecision p a
      => FHFoldable FHSingle p ('TChoice 'True a b) where
  fhfoldr p f acc (FHTypeChoice _ tru _) = fhfoldr p f acc tru

instance FHFoldable FHDecision p b
      => FHFoldable FHSingle p ('TChoice 'False a b) where
  fhfoldr p f acc (FHTypeChoice _ _ fal) = fhfoldr p f acc fal



-- | A simple decision tree where each action is wrapped into @f@, supporting chaining and
--   branching.
--
--   There are four distinct things 'FHDecision' allows for:
--
--     * Creating simple values by using 'action' and 'actions' (latter simply allowing lists
--       of values of the same type);
--
--     * Chaining different trees with '(>>)' (use @{-\# LANGUAGE RebindableSyntax \#-}@);
--
--     * Branching using '(>..)' and '(>!!)' (see "Data.OneOf" for explanation of that system);
--
--     * Type-level branching using 'boolD' and @('Proxy' :: 'Proxy' 'Bool')@ as a condition.
--
--   To obtain the result you need just fold the tree.
--
--   Example (using 'Maybe' as the @f@):
--
--   @
--     {-\# LANGUAGE FlexibleContexts
--                , DataKinds
--                , RebindableSyntax \#-}
--
--     {-\# OPTIONS_GHC -Wno-unused-do-bind \#-} -- For if you have -Wall on
--
--     import           Data.FHDecision
--
--     import           Data.Proxy
--     import           Prelude hiding ((>>))
--
--     f n = do
--       'action' $ Just 2
--       case n of
--         Just (Left a)  -> (Proxy :: Proxy 0) '>..' do 'action' $ Just a
--         Just (Right _) -> (Proxy :: Proxy 1) '>..' do 'action' $ Just ()
--         Nothing        -> (Proxy :: Proxy 2) '>!!' do 'action' $ Just False
--       'actions' [Just (), Just (), Just ()]
--       'boolD' (Proxy :: Proxy 'True)
--         ('action' $ Just \'a\')
--         ('action' $ Just "a")
--   @
data FHDecision (f :: k -> *) (as :: Dec) where
  FHSingle :: FHSingle f as -> FHDecision f ('Single as)

  FHTypeChain :: FHList (FHDecision f) as -> FHDecision f ('TChain as)



instance Show (FHSingle f a) => Show (FHDecision f ('Single a)) where
  showsPrec d (FHSingle a) = showParen (d > 10) $ showString "FHSingle " . showsPrec 11 a

instance Show (FHList (FHDecision f) as) => Show (FHDecision f ('TChain as)) where
  showsPrec d (FHTypeChain a) = showParen (d > 10) $
                                    showString "FHTypeChain "
                                  . showsPrec 11 a


instance FHFunctor FHSingle p a
      => FHFunctor FHDecision p ('Single a) where
  fhfmap p f (FHSingle a) = FHSingle $ fhfmap p f a

instance FHFunctor FHList (FHFunctor FHDecision p) as
      => FHFunctor FHDecision p ('TChain as) where
  fhfmap p f (FHTypeChain as) =
    FHTypeChain $ fhfmap (Proxy :: Proxy (FHFunctor FHDecision p)) (fhfmap p f) as


instance FHFoldable FHSingle p a
      => FHFoldable FHDecision p ('Single a) where
  fhfoldr p f acc (FHSingle a) = fhfoldr p f acc a

instance FHFoldable FHList (FHFoldable FHDecision p) as
      => FHFoldable FHDecision p ('TChain as) where
  fhfoldr p f acc (FHTypeChain as) =
    fhfoldr (Proxy :: Proxy (FHFoldable FHDecision p)) (flip $ fhfoldr p f) acc as



class ReboundSeq a b c | a b -> c where
  -- | Rebind of '(Pre.>>)' that chains operations in 'FHDecision' trees.
  (>>) :: f ~ g => FHDecision f a -> FHDecision g b -> FHDecision g c

instance ReboundSeq ('Single a) ('Single b) ('TChain '[ 'Single a, 'Single b ]) where
  a >> b = FHTypeChain $ coerce a :&>: coerce b

instance ReboundSeq ('Single a) ('TChain bs) ('TChain ('Single a ': bs)) where
  a >> FHTypeChain b = FHTypeChain $ coerce a :&> b

instance FHList.Concat as '[ 'Single b ] cs
      => ReboundSeq ('TChain as) ('Single b) ('TChain cs) where
  FHTypeChain a >> b = FHTypeChain $ coerce a FHList.++ FHList.singleton b

instance FHList.Concat as bs cs
      => ReboundSeq ('TChain as) ('TChain bs) ('TChain cs) where
  FHTypeChain a >> FHTypeChain b = FHTypeChain $ coerce a FHList.++ coerce b



class BoolD cond a b c | cond a b -> c, cond a c -> b, cond b c -> a where
  -- | A variant of 'bool' that wraps both sides into an 'FHDecision'.
  --
  --   Allows for @'Proxy' 'Bool'@s as conditionals. In these proxied cases branches
  --   that remain unused are removed from the 'Collect' type list too.
  boolD :: cond -> a -> b -> c

instance f ~ g
      => BoolD (Proxy cond) (FHDecision f a) (FHDecision g b)
                 (FHDecision g ('Single ('TChoice cond a b))) where
  boolD cond tru fal = FHSingle $ FHTypeChoice cond tru fal

instance f ~ g
      => BoolD Bool (FHDecision f a) (FHDecision g b)
                 (FHDecision g ('Single ('DChoice ('B '() a ('B '() b 'T 'T) 'T)))) where
  boolD cond tru fal =
    FHSingle . FHDataChoice $ if cond
                                then (Proxy :: Proxy 0) >.< tru
                                else (Proxy :: Proxy 1) >!< fal



action :: f a -> FHDecision f ('Single ('Action a))
action = FHSingle . FHAction



actions :: [f a] -> FHDecision f ('Single ('DChain ('Single ('Action a))))
actions = FHSingle . FHDataChain . fmap (FHSingle . FHAction)



-- | Collects all the types (or kinds) inside of a 'Dec' into a list.
class Collect k (as :: Dec) (p :: [k]) | k as -> p

instance Collect k ('Single ('Action a)) '[a]

instance Collect k as p => Collect k ('Single ('DChain as)) p

instance Collect k ('Single ('DChoice 'T)) '[]

instance ( Collect k ('Single ('DChoice l)) p1
         , Collect k a p2
         , Collect k ('Single ('DChoice r)) p4
         , FHList.Concat p1 p2 p3
         , FHList.Concat p3 p4 p5
         )
        => Collect k ('Single ('DChoice ('B '() a l r))) p5

instance Collect k a p
      => Collect k ('Single ('TChoice 'True a b)) p

instance Collect k b p
      => Collect k ('Single ('TChoice 'False a b)) p

instance Collect k ('TChain '[]) '[]

instance ( Collect k a p1
         , Collect k ('TChain as) p2
         , FHList.Concat p1 p2 p
         ) => Collect k ('TChain (a ': as)) p



-- | '(>.<)' lifted to 'FHDecision's.
(>..) :: Fit (n + 1) a as => Proxy n -> FHDecision f a -> FHDecision f ('Single ('DChoice as))
(>..) n = FHSingle . FHDataChoice . (>.<) n

-- | '(>!<)' lifted to 'FHDecision's.
(>!!)
  :: ( Fit (n + 1) a as
     , Limit (n + 1) as
     )
  => Proxy n -> FHDecision f a -> FHDecision f ('Single ('DChoice as))
(>!!) n = FHSingle . FHDataChoice . (>!<) n

-- | 'limit' lifted to 'FHDecision's.
limitD :: Limit (n + 1) as => Proxy n -> FHDecision f ('Single ('DChoice as)) -> FHDecision f ('Single ('DChoice as))
limitD _ = id
