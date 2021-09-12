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

import           Control.DeepSeq
import           Control.Monad hiding ((>>))
import           Data.Coerce
import           Data.Proxy
import           GHC.TypeLits
import           Prelude hiding ((>>))
import qualified Prelude as Pre




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


instance NFData (f a) => NFData (FHSingle f ('Action a)) where
  rnf (FHAction a) = rnf a

instance NFData (FHDecision f as) => NFData (FHSingle f ('DChain as)) where
  rnf (FHDataChain as) = rnf as

instance NFData (OneOf (FHDecision f) as) => NFData (FHSingle f ('DChoice as)) where
  rnf (FHDataChoice as) = rnf as

instance NFData (FHDecision f tru) => NFData (FHSingle f ('TChoice 'True tru fal)) where
  rnf (FHTypeChoice Proxy tru _) = rnf tru

instance NFData (FHDecision f fal) => NFData (FHSingle f ('TChoice 'False tru fal)) where
  rnf (FHTypeChoice Proxy _ fal) = rnf fal



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
--     * Type-level branching using 'ifThenElse' and @('Proxy' :: 'Proxy' 'Bool')@ as a condition.
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
--     {-\# OPTIONS_GHC -Wno-unused-do-bind \#-}               -- For if you have -Wall on
--     {-\# OPTIONS_GHC -fno-warn-partial-type-signatures \#-} -- Silences type inference warnings
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
--       if Proxy :: Proxy 'True
--         then 'action' $ Just \'a\'
--         else 'action' $ Just "a"
--   @
--
--   It should be noted that in spite the fact that every single type-level value is
--   represented in terms of some data constructor, you cannot produce an infinite type
--   in Haskell. You can still wrap things into a list on the outside though ;)
data FHDecision (f :: k -> *) (as :: Dec) where
  FHSingle :: FHSingle f as -> FHDecision f ('Single as)

  FHTypeChain :: FHList (FHDecision f) as -> FHDecision f ('TChain as)


instance Show (FHSingle f a) => Show (FHDecision f ('Single a)) where
  showsPrec d (FHSingle a) = showParen (d > 10) $ showString "FHSingle " . showsPrec 11 a

instance Show (FHList (FHDecision f) as) => Show (FHDecision f ('TChain as)) where
  showsPrec d (FHTypeChain a) = showParen (d > 10) $
                                    showString "FHTypeChain "
                                  . showsPrec 11 a


instance NFData (FHSingle f as) => NFData (FHDecision f ('Single as)) where
  rnf (FHSingle as) = rnf as

instance NFData (FHList (FHDecision f) as) => NFData (FHDecision f ('TChain as)) where
  rnf (FHTypeChain as) = rnf as



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



class ReboundSeq m a b c | m a b -> c where
  -- | Rebind of '(Pre.>>)' that can also chain operations in 'FHDecision' trees.
  --
  --   Similar type problems to 'ifThenElse'. The compiler needs to know whether
  --   @m a@ or @m b@ is an 'FHDecision' to choose an instance.
  (>>) :: m a -> m b -> m c

instance (IsDecision (m a) (m b) ~ flag, ReboundSeq' flag m a b c) => ReboundSeq m a b c where
  (>>) = (>>?) (Proxy :: Proxy flag)

class ReboundSeq' flag m a b c | flag m a b -> c where
  (>>?) :: Proxy flag -> m a -> m b -> m c

instance ReboundSeq' 'True (FHDecision f) ('Single a) ('Single b) ('TChain '[ 'Single a, 'Single b ]) where
  (>>?) _ a b = FHTypeChain $ coerce a :&>: coerce b

instance ReboundSeq' 'True (FHDecision f) ('Single a) ('TChain bs) ('TChain ('Single a ': bs)) where
  (>>?) _ a (FHTypeChain b) = FHTypeChain $ coerce a :&> b

instance FHList.Concat as '[ 'Single b ] cs
      => ReboundSeq' 'True (FHDecision f) ('TChain as) ('Single b) ('TChain cs) where
  (>>?) _ (FHTypeChain a) b = FHTypeChain $ coerce a FHList.++ FHList.singleton b

instance FHList.Concat as bs cs
      => ReboundSeq' 'True (FHDecision f) ('TChain as) ('TChain bs) ('TChain cs) where
  (>>?) _ (FHTypeChain a) (FHTypeChain b) = FHTypeChain $ coerce a FHList.++ coerce b

instance Monad m => ReboundSeq' 'False (m :: * -> *) a b b where
  (>>?) _ = (Pre.>>)



class IfThenElse cond a b c | cond a b -> c where
  -- | A variant of 'if' that wraps both sides into an 'FHDecision'.
  --
  --   Allows for @'Proxy' 'Bool'@s as conditionals. In these proxied cases branches
  --   that remain unused are removed from the 'Collect' type list too.
  --
  --   These 'if's do require more type info than usual ones, the compiler
  --   needs to know what type the condition is and whether any of the branches
  --   are 'FHDecision's.
  ifThenElse :: cond -> a -> b -> c

type family IsDecision (a :: *) (b :: *) :: Bool where
  IsDecision (FHDecision _ _) _ = 'True
  IsDecision _ (FHDecision _ _) = 'True
  IsDecision _                _ = 'False

instance ( flag ~ IsDecision a b
         , IfThenElse' flag cond a b c
         )
        => IfThenElse cond a b c where
  ifThenElse = ifThenElse' (Proxy :: Proxy flag)

class IfThenElse' flagA cond a b c | flagA cond a b -> c where
  ifThenElse' :: Proxy flagA -> cond -> a -> b -> c

instance f ~ g
      => IfThenElse' 'True (Proxy cond) (FHDecision f a) (FHDecision g b)
                                        (FHDecision g ('Single ('TChoice cond a b))) where
  ifThenElse' _ cond tru fal = FHSingle $ FHTypeChoice cond tru fal

instance a ~ b => IfThenElse' 'False (Proxy 'True) a b b where
  ifThenElse' _ _ a _ = a

instance a ~ b => IfThenElse' 'False (Proxy 'False) a b b where
  ifThenElse' _ _ _ a = a

instance f ~ g
      => IfThenElse' 'True Bool (FHDecision f a) (FHDecision g b)
                       (FHDecision g ('Single ('DChoice ('B '() a ('B '() b 'T 'T) 'T)))) where
  ifThenElse' _ cond tru fal =
    FHSingle . FHDataChoice $ if cond
                                then (Proxy :: Proxy 0) >.< tru
                                else (Proxy :: Proxy 1) >!< fal

instance a ~ b => IfThenElse' 'False Bool a b b where
  ifThenElse' _ cond a b = if cond then a else b




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
