{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_HADDOCK not-home #-}

module Data.FHMap.Internal where

import           Data.FHList (FHList (..))
import           Data.Type.Eq
import           Data.Type.Length
import           Data.Type.Map
import           Data.Type.Materialize
import           Data.Type.Ord

import           Data.Type.Bool
import           Control.Applicative (liftA3)
import           Control.Monad (guard)
import           Data.Kind
import           Data.Ord
import           Data.Proxy
import           Data.Void
import           GHC.Generics
import           GHC.TypeLits as Lits hiding (type (<=))
import           Prelude hiding (lookup, map, foldr)



--   A heterogenous map from keys @k@ of kind 'Symbol' to values @vs@ of arbitrary type
--   of kind @v@, each wrapped into @t@.
--
--   The 'SemigroupH' operation for 'FHMap' is 'union', which prefers values from the
--   left operand. If @m1@ maps a key @k@ to a value @a1@, and @m2@ maps the same key
--   to a different value @a2@, then their union @m1 '<>' m2@ maps @k@ to @a1@.
data FHMap (f :: v -> *) (vs :: M Symbol) where
  FHBin :: Proxy k -> f a -> !(FHMap f l) -> !(FHMap f r) -> FHMap f ('B k (a :: v) l r)
  FHTip :: FHMap f 'T

instance (Functor f, ToAscList f as bs, Show (FHList f bs)) => Show (FHMap f as) where
  show = show . toAscList



instance GenericH f as => Generic (FHMap f as) where
  type Rep (FHMap f as) =
         D1 ('MetaData "FHMap" "Data.FHMap" "fhetero" 'False)
           ( RepH f as )

  from = M1 <$> fromH

  to = toH . unM1

class GenericH f as where
  type RepH f as :: * -> *

  fromH :: FHMap f as -> RepH f as x

  toH :: RepH f as x -> FHMap f as

instance GenericH f 'T where
  type RepH f 'T = C1 ('MetaCons "FHTip" 'PrefixI 'False)
                     U1

  fromH FHTip = M1 U1

  toH (M1 U1) = FHTip

instance (GenericH f l, GenericH f r) => GenericH (f :: v -> *) ('B k (a :: v) l r) where
  type RepH f ('B k a l r) = C1 ('MetaCons "FHBin" 'PrefixI 'False)
                               ( S1 ('MetaSel 'Nothing
                                                'NoSourceUnpackedness
                                                'NoSourceStrictness
                                                'DecidedLazy)
                                       (Rec0 (Proxy k))
                                  :*:
                                 S1 ('MetaSel 'Nothing
                                                'NoSourceUnpackedness
                                                'NoSourceStrictness
                                                'DecidedLazy)
                                       (Rec0 (f a))
                                  :*:
                                 RepH f l
                                  :*:
                                 RepH f r
                               )

  fromH (FHBin k a l r) = M1 $ M1 (K1 k) :*: M1 (K1 a) :*: fromH l :*: fromH r

  toH (M1 (M1 (K1 k) :*: M1 (K1 a) :*: l :*: r)) = FHBin k a (toH l) (toH r)



-- | The empty map.
empty :: FHMap f 'T
empty = FHTip

-- | A map with a single element.
singleton
  :: Proxy k
  -> f a
  -> FHMap f ('B k a 'T 'T)
singleton p a = FHBin p a FHTip FHTip



-- | Is the map empty?
null :: FHMap f as -> Bool
null FHTip = True
null _    = False



-- | The number of elements in the map.
size :: KnownNat (Length as) => FHMap f as -> Integer
size (_ :: FHMap f as) = natVal (Proxy :: Proxy (Length as))



class Member (k :: Symbol) (as :: M Symbol) (e :: Bool) | k as -> e

instance Member k 'T 'False

instance ( flag ~ CmpSymbol q k
         , Member' flag q ('B k b l r) e
         )
      => Member q ('B k b l r) e



class Member' (flag :: Ordering) k as e | flag k as -> e

instance Member' 'EQ k ('B k a ls ms) 'True

instance Member q l e => Member' 'LT q ('B k b l r) e

instance Member q r e => Member' 'GT q ('B k b l r) e



-- | Is the key a member of the map?
member
  :: (Materialize Bool e, Member k as e)
  => Proxy k -> FHMap f as -> Bool
member (_ :: Proxy k) (_ :: FHMap f as) =
  materialize (Proxy :: Member k as e => Proxy e)



-- | Synonym to 'lookup', arguments are flipped.
(!) :: Lookup k as exists a => FHMap f as -> Proxy k -> f a
(!) = flip lookup



type LookupMay = LookupI 'EitherWay

-- | Lookup the value at a key in the map.
--
--   The function will return the corresponding value as @('Just' value)@,
--   or @'Nothing'@ if the key isn't in the map.
--
--   NOTE: Since in case of 'Nothing' __something__ still has to be returned on the type level
--         and kinds do not have a 'Void' type, this function will not work if @a@ is a kind.
lookupMay :: LookupMay k as e a => Proxy k -> FHMap f as -> Maybe (f a)
lookupMay k m =
  let (p, v) = lookupI (Proxy :: Proxy 'EitherWay) k m
  in v <$ guard (materialize p)



-- | Synonym to 'lookupMay', arguments are flipped.
(!?) :: LookupMay k as exists a => FHMap f as -> Proxy k -> Maybe (f a)
(!?) = flip lookupMay



type Lookup = LookupI 'IfExists

-- | Lookup the value at a key in the map.
--   
--   The function will return a type error if the key isn't in the map.
lookup :: Lookup k as exists a => Proxy k -> FHMap f as -> f a
lookup k = snd . lookupI (Proxy :: Proxy 'IfExists) k



class Materialize Bool e
   => LookupI (i :: IfExists) (k :: Symbol) (as :: M Symbol) (e :: Bool) a
                | i k as -> e a where
  lookupI :: Proxy i -> Proxy k -> FHMap f as -> (Proxy e, f a)

instance LookupI i k 'T 'False Void where
  lookupI _ _ FHTip = (Proxy :: Proxy 'False, undefined) -- undefined is @t Void@

instance ( flag ~ CmpSymbol q k
         , Materialize Bool e
         , Exists i e "Key " q " is not in the map" 
         , LookupI' flag i q ('B k b l r) e a
         )
      => LookupI i q ('B k b l r) e a where
  lookupI = lookupI' (Proxy :: Proxy flag)



class LookupI' (flag :: Ordering) i k as e a | flag i k as -> e a where
  lookupI' :: Proxy flag -> Proxy i -> Proxy k -> FHMap f as -> (Proxy e, f a)

instance LookupI' 'EQ i k ('B k a ls ms) 'True a where
  lookupI' _ _ _ (FHBin _ a _ _) = (Proxy :: Proxy 'True, a)

instance LookupI i q l e a => LookupI' 'LT i q ('B k b l r) e a where
  lookupI' _ i q (FHBin _ _ l _) = lookupI i q l

instance LookupI i q r e a => LookupI' 'GT i q ('B k b l r) e a where
  lookupI' _ i q (FHBin _ _ _ r) = lookupI i q r



map
  :: MapWithKey p as
  => Proxy p
  -> (forall a. p a => f a -> g a)
  -> FHMap f as
  -> FHMap g as
map p f = mapWithKey p $ const f

class MapWithKey p as where
  mapWithKey
    :: Proxy p
    -> (forall k a. (KnownSymbol k, p a) => Proxy k -> f a -> g a)
    -> FHMap f as
    -> FHMap g as

instance MapWithKey p 'T where
  mapWithKey _ _ FHTip = FHTip

instance ( KnownSymbol k
         , p a
         , MapWithKey p l
         , MapWithKey p r
         )
      => MapWithKey p ('B k a l r) where
  mapWithKey q f (FHBin p a l r) =
    FHBin p (f p a) (mapWithKey q f l) (mapWithKey q f r)



foldMap
  :: (FoldWithKey p as, Monoid m)
  => Proxy p
  -> (forall a. p a => f a -> m)
  -> FHMap f as
  -> m
foldMap p f = foldMapWithKey p $ const f

foldr
  :: FoldWithKey p as
  => Proxy p
  -> (forall a. p a => f a -> b -> b)
  -> b
  -> FHMap f as
  -> b
foldr p f = foldrWithKey p $ const f

foldr'
  :: FoldWithKey p as
  => Proxy p
  -> (forall a. p a => f a -> b -> b)
  -> b
  -> FHMap f as
  -> b
foldr' p f = foldrWithKey' p $ const f

foldl
  :: FoldWithKey p as
  => Proxy p
  -> (forall a. p a => b -> f a -> b)
  -> b
  -> FHMap f as
  -> b
foldl p f = foldlWithKey p $ \b _ -> f b

foldl'
  :: FoldWithKey p as
  => Proxy p
  -> (forall a. p a => b -> f a -> b)
  -> b
  -> FHMap f as
  -> b
foldl' p f = foldlWithKey' p $ \b _ -> f b




class FoldWithKey p as where
  foldMapWithKey
    :: Monoid m
    => Proxy p
    -> (forall k a. (KnownSymbol k, p a) => Proxy k -> f a -> m)
    -> FHMap f as
    -> m
  
  foldrWithKey
    :: Proxy p
    -> (forall k a. (KnownSymbol k, p a) => Proxy k -> f a -> b -> b)
    -> b
    -> FHMap f as
    -> b

  foldrWithKey'
    :: Proxy p
    -> (forall k a. (KnownSymbol k, p a) => Proxy k -> f a -> b -> b)
    -> b
    -> FHMap f as
    -> b

  foldlWithKey
    :: Proxy p
    -> (forall k a. (KnownSymbol k, p a) => b -> Proxy k -> f a -> b)
    -> b
    -> FHMap f as
    -> b

  foldlWithKey'
    :: Proxy p
    -> (forall k a. (KnownSymbol k, p a) => b -> Proxy k -> f a -> b)
    -> b
    -> FHMap f as
    -> b

instance FoldWithKey p 'T where
  foldMapWithKey _ _ FHTip = mempty
  
  foldrWithKey _ _ t FHTip = t

  foldrWithKey' _ _ !t FHTip = t

  foldlWithKey _ _ t FHTip = t

  foldlWithKey' _ _ !t FHTip = t

instance ( KnownSymbol k
         , p a
         , FoldWithKey p l
         , FoldWithKey p r
         )
      => FoldWithKey p ('B k a l r) where
  foldMapWithKey p f (FHBin k a l r) =
    foldMapWithKey p f l <> f k a <> foldMapWithKey p f r
 
  foldrWithKey p f t (FHBin k a l r) = 
    foldrWithKey p f (f k a $ foldrWithKey p f t r) l

  foldrWithKey' p f t (FHBin k a l r) = 
    foldrWithKey' p f (f k a $ foldrWithKey' p f t r) l

  foldlWithKey p f t (FHBin k a l r) =
    foldlWithKey p f (f (foldlWithKey p f t l) k a) r

  foldlWithKey' p f t (FHBin k a l r) =
    foldlWithKey' p f (f (foldlWithKey' p f t l) k a) r



traverse
  :: (TraverseWithKey p as, Applicative m)
  => Proxy p
  -> (forall a. p a => f a -> m (g a))
  -> FHMap f as
  -> m (FHMap g as)
traverse p f = traverseWithKey p $ const f

class TraverseWithKey p as where
  traverseWithKey
    :: Applicative m
    => Proxy p
    -> (forall k a. (KnownSymbol k, p a) => Proxy k -> f a -> m (g a))
    -> FHMap f as
    -> m (FHMap g as)

instance TraverseWithKey f 'T where
  traverseWithKey _ _ FHTip = pure FHTip

instance ( KnownSymbol k
         , p a
         , TraverseWithKey p l
         , TraverseWithKey p r
         )
        => TraverseWithKey p ('B k a l r) where
  traverseWithKey p f (FHBin k a l r) =
    liftA3 (flip $ FHBin k) (traverseWithKey p f l) (f k a) (traverseWithKey p f r)



traverse_
  :: (FoldWithKey p as, Applicative m)
  => Proxy p
  -> (forall a. p a => f a -> m ())
  -> FHMap f as
  -> m ()
traverse_ p f = foldr p (\a acc -> f a *> acc) $ pure ()

traverseWithKey_
  :: (FoldWithKey p as, Applicative m)
  => Proxy p
  -> (forall k a. (KnownSymbol k, p a) => Proxy k -> f a -> m ())
  -> FHMap f as
  -> m ()
traverseWithKey_ p f = foldrWithKey p (\k a acc -> f k a *> acc) $ pure ()



-- | Convert the map to a list of key proxy/value pairs.
toList :: (Functor f, ToAscList f as bs) => FHMap f as -> FHList f bs
toList = toAscList


-- | Convert the map to a list of key proxy/value pairs where
--   the keys are in ascending order.
toAscList :: (Functor f, ToAscList f as bs) => FHMap f as -> FHList f bs
toAscList = flip toAscListL FHZero

type ToAscList f as bs = ToAscListL f as '[] bs

class ToAscListL f as bs cs | as bs -> cs where
  toAscListL :: FHMap f as -> FHList f bs -> FHList f cs

instance ToAscListL f 'T as as where
  toAscListL FHTip = id

instance ( ToAscListL f r as cs
         , ToAscListL f l ((Proxy k, a) ': cs) bs
         , Functor f
         )
        => ToAscListL f ('B k a l r) as bs where
  toAscListL (FHBin k a l r) as = toAscListL l (((,) k <$> a) :&> toAscListL r as)



-- | Convert the map to a list of key proxy/value pairs where
--   the keys are in descending order.
toDescList :: (Functor f, ToDescList f as bs) => FHMap f as -> FHList f bs
toDescList = flip toDescListL FHZero

type ToDescList f as bs = ToDescListL f as '[] bs

class ToDescListL f as bs cs | as bs -> cs where
  toDescListL :: FHMap f as -> FHList f bs -> FHList f cs

instance ToDescListL f 'T as as where
  toDescListL FHTip = id

instance ( ToDescListL f l as cs
         , ToDescListL f r ((Proxy k, a) ': cs) bs
         , Functor f
         )
        => ToDescListL f ('B k a l r) as bs where
  toDescListL (FHBin k a l r) as =
    toDescListL r (((,) k <$> a) :&> toDescListL l as)



type Delta = 3
type Ratio = 2



type Insert = InsertC 'Clobber

-- | Insert a new key and value in the map. If the key is already present in the map,
--   the associated value is replaced with the supplied value.
insert :: Insert k a m z => Proxy k -> f a -> FHMap f m -> FHMap f z
insert = insertC (Proxy :: Proxy 'Clobber)



type InsertUnique = InsertC 'NoClobber

-- | Insert a new key and value in the map. If the key is already present in the map,
--   a type error occurs.
insertUnique :: InsertUnique k a m z => Proxy k -> f a -> FHMap f m -> FHMap f z
insertUnique = insertC (Proxy :: Proxy 'NoClobber)



type InsertId = InsertC 'IfIdentical

-- | Insert a new key and value in the map. If the key is already present in the map,
--   the associated value is only replaced with the supplied value if the
--   previously stored type at that position matches the new one, otherwise a
--   type error occurs.
insertId :: InsertId k a m z => Proxy k -> f a -> FHMap f m -> FHMap f z
insertId = insertC (Proxy :: Proxy 'IfIdentical)



data Clobber = NoClobber
             | IfIdentical
             | Clobber

class InsertC (clobber :: Clobber) k a as bs | clobber k a as -> bs where
  insertC :: Proxy clobber -> Proxy k -> f a -> FHMap f as -> FHMap f bs

instance InsertC c k a 'T ('B k a 'T 'T) where
  insertC _ k a FHTip = FHBin k a FHTip FHTip

instance ( flag ~ CmpSymbol kx k
         , InsertC' flag clobber kx x ('B k a l r) zs
         )
        => InsertC clobber kx x ('B k a l r) zs where
  insertC = insertC' (Proxy :: Proxy flag)



class InsertC' (cmp :: Ordering) clobber k a as bs | cmp clobber k a as -> bs where
  insertC' :: Proxy cmp -> Proxy clobber -> Proxy k -> f a -> FHMap f as -> FHMap f bs

instance ( InsertC c kx x l l'
         , BalanceL k a l' r bs
         )
      => InsertC' 'LT c kx x ('B k a l r) bs where
  insertC' _ c kx x (FHBin k a l r) = balanceL k a (insertC c kx x l) r

instance ( InsertC c kx x r r'
         , BalanceR k a l r' bs
         )
      => InsertC' 'GT c kx x ('B k a l r) bs where
  insertC' _ c kx x (FHBin k a l r) = balanceR k a l (insertC c kx x r)

instance InsertC' 'EQ 'Clobber k x ('B k a l r) ('B k x l r) where
  insertC' _ _ k x (FHBin _ _ l r) = FHBin k x l r

instance ( flag ~ (x == a)
         , InsertCId flag k x ('B k a l r)
         )
        => InsertC' 'EQ 'IfIdentical k x ('B k a l r) ('B k a l r) where
  insertC' _ _ = insertCId (Proxy :: Proxy flag)

instance TypeError ( 'Text "Key "
               ':<>: 'ShowType k
               ':<>: 'Text " already present in the map"
                   )
      => InsertC' 'EQ 'NoClobber k x ('B k a l r) 'T where
  insertC' = undefined



class InsertCId (isEq :: Bool) k a as where
  insertCId :: Proxy isEq -> Proxy k -> f a -> FHMap f as -> FHMap f as

instance InsertCId 'True k x ('B k x l r) where
  insertCId _ k x (FHBin _ _ l r) = FHBin k x l r

instance TypeError ( 'Text "Expected type "
               ':<>: 'ShowType x
               ':<>: 'Text " on identical insert of key "
               ':<>: 'ShowType k
               ':<>: 'Text ", but found "
               ':<>: 'ShowType a
                   )
      => InsertCId 'False k x ('B k a l r) where
  insertCId = undefined



class Balance k a l r z | k a l r -> z where
  balance :: Proxy k -> f a -> FHMap f l -> FHMap f r -> FHMap f z

instance Balance k a 'T 'T ('B k a 'T 'T) where
  balance = FHBin

instance BalanceL k a l 'T z => Balance k a l 'T z where
  balance = balanceL

instance BalanceR k a 'T r z => Balance k a 'T r z where
  balance = balanceR

instance ( l ~ 'B lk la ll lr
         , r ~ 'B rk ra rl rr
         , leftCondition ~ (Length l > Delta Lits.* Length r)
         , rightCondition ~ (Length r > Delta Lits.* Length l)
         , isOther      ~ (Not rightCondition || Not leftCondition)
         , isRight      ~ rightCondition
         , isOtherInner ~ If isRight
                            ( Length rl < Ratio Lits.* Length rr )
                            ( Length lr < Ratio Lits.* Length ll )
         , Balance' isOther isRight isOtherInner k a l r z
         )
        => Balance k a ('B lk la ll lr) ('B rk ra rl rr) z where
  balance = balance' (Proxy :: Proxy isOther) (Proxy :: Proxy isRight) (Proxy :: Proxy isOtherInner)



class Balance' isOther isRight isOtherInner k a l r z | isOther isRight isOtherInner k a l r -> z where
  balance'
    :: Proxy isOther
    -> Proxy isRight
    -> Proxy isOtherInner
    -> Proxy k
    -> f a
    -> FHMap f l
    -> FHMap f r
    -> FHMap f z

instance Balance' 'False 'False 'True k a ('B lk la ('B llk lla lll llr) ('B lrk lra lrl lrr)) ('B rk ra rl rr)
                     ('B lk la ('B llk lla lll llr) ('B k a ('B lrk lra lrl lrr) ('B rk ra rl rr))) where 
  balance' _ _ _ k a (FHBin lk la ll lr) r = FHBin lk la ll (FHBin k a lr r)

instance Balance' 'False 'False 'False k a ('B lk la ('B llk lla lll llr) ('B lrk lra lrl lrr)) ('B rk ra rl rr)
                     ('B lrk lra ('B lk la ('B llk lla lll llr) lrl) ('B k a lrr ('B rk ra rl rr))) where
  balance' _ _ _ k a (FHBin lk la ll (FHBin lrk lra lrl lrr)) r =
    FHBin lrk lra (FHBin lk la ll lrl) (FHBin k a lrr r)

instance Balance' 'False 'True 'True k a ('B lk la ll lr) ('B rk ra ('B rlk rla rll rlr) ('B rrk rra rrl rrr))
                     ('B rk ra ('B k a ('B lk la ll lr) ('B rlk rla rll rlr)) ('B rrk rra rrl rrr)) where
  balance' _ _ _ k a l (FHBin rk ra rl rr) = FHBin rk ra (FHBin k a l rl) rr

instance Balance' 'False 'True 'False k a ('B lk la ll lr) ('B rk ra ('B rlk rla rll rlr) ('B rrk rra rrl rrr))
                     ('B rlk rla ('B k a ('B lk la ll lr) rll) ('B rk ra rlr ('B rrk rra rrl rrr))) where
  balance' _ _ _ k a l (FHBin rk ra (FHBin rlk rla rll rlr) rr) =
    FHBin rlk rla (FHBin k a l rll) (FHBin rk ra rlr rr)

instance Balance' 'True o2 o3 k a l r ('B k a l r) where
  balance' _ _ _ = FHBin



class BalanceL k a l r z | k a l r -> z where
  balanceL :: Proxy k -> f a -> FHMap f l -> FHMap f r -> FHMap f z

instance BalanceL k a 'T 'T ('B k a 'T 'T) where
  balanceL = FHBin

instance BalanceL k a ('B lk la 'T 'T) 'T ('B k a ('B lk la 'T 'T) 'T) where
  balanceL = FHBin

instance BalanceL k a ('B lk la 'T ('B lrk lra 'T 'T)) 'T ('B lrk lra ('B lk la 'T 'T) ('B k a 'T 'T)) where
  balanceL k a (FHBin lk la FHTip (FHBin lrk lra _ _)) FHTip =
    FHBin lrk lra (FHBin lk la FHTip FHTip) (FHBin k a FHTip FHTip)

instance BalanceL k a ('B lk la ('B llk lla lll llr) 'T) 'T ('B lk la ('B llk lla lll llr) ('B k a 'T 'T)) where
  balanceL k a (FHBin lk la ll FHTip) FHTip = FHBin lk la ll (FHBin k a FHTip FHTip)

instance ( ll ~ 'B llk lla lll llr
         , lr ~ 'B lrk lra lrl lrr
         , l ~ 'B lk la ll lr
         , flag ~ (Length lr < Ratio Lits.* Length ll)
         , BalanceL' flag k a l 'T z
         )
        => BalanceL k a ('B lk la ('B llk lla lll llr) ('B lrk lra lrl lrr)) 'T z where
  balanceL = balanceL' (Proxy :: Proxy flag)

instance BalanceL k a 'T ('B rk ra 'T 'T) ('B k a 'T ('B rk ra 'T 'T)) where
  balanceL = FHBin

instance ( l ~ 'B lk la ll lr
         , r ~ 'B rk ra rl rr
         , isOther      ~ Not (Length l > Delta Lits.* Length r)
         , isRight      ~ 'False
         , isOtherInner ~ (Length lr < Ratio Lits.* Length ll)
         , Balance' isOther isRight isOtherInner k a l r z
         )
        => BalanceL k a ('B lk la ll lr) ('B rk ra rl rr) z where
  balanceL = balance' (Proxy :: Proxy isOther) (Proxy :: Proxy isRight) (Proxy :: Proxy isOtherInner)



class BalanceL' o k a l r z | o k a l r -> z where
  balanceL' :: Proxy o -> Proxy k -> f a -> FHMap f l -> FHMap f r -> FHMap f z

instance BalanceL' 'True k a ('B lk la ll lr) 'T ('B lk la ll ('B k a lr 'T)) where
  balanceL' _ k a (FHBin lk la ll lr) FHTip = FHBin lk la ll (FHBin k a lr FHTip)

instance BalanceL' 'False k a ('B lk la ll ('B lrk lra lrl lrr)) 'T
                      ('B lrk lra ('B lk la ll lrl) ('B k a lrr 'T)) where
  balanceL' _ k a (FHBin lk la ll (FHBin lrk lra lrl lrr)) FHTip =
    FHBin lrk lra (FHBin lk la ll lrl) (FHBin k a lrr FHTip)



class BalanceR k a l r z | k a l r -> z where
  balanceR :: Proxy k -> f a -> FHMap f l -> FHMap f r -> FHMap f z

instance BalanceR k a 'T 'T ('B k a 'T 'T) where
  balanceR = FHBin

instance BalanceR k a 'T ('B rk ra 'T 'T) ('B k a 'T ('B rk ra 'T 'T)) where
  balanceR = FHBin

instance BalanceR k a 'T ('B rk ra 'T ('B rrk rra rrl rrr)) ('B rk ra ('B k a 'T 'T) ('B rrk rra rrl rrr)) where
  balanceR k a FHTip (FHBin rk ra FHTip rr) = FHBin rk ra (FHBin k a FHTip FHTip) rr

instance BalanceR k a 'T ('B rk ra ('B rlk rla 'T 'T) 'T) ('B rlk rla ('B k a 'T 'T) ('B rk ra 'T 'T)) where
  balanceR k a FHTip (FHBin rk ra (FHBin rlk rla FHTip FHTip) FHTip) =
    FHBin rlk rla (FHBin k a FHTip FHTip) (FHBin rk ra FHTip FHTip)

instance ( rl ~ 'B rlk rla rll rlr
         , rr ~ 'B rrk rra rrl rrr
         , r ~ 'B rk ra rl rr
         , flag ~ (Length rl < Ratio Lits.* Length rr)
         , BalanceR' flag k a 'T r z
         )
        => BalanceR k a 'T ('B rk ra ('B rlk rla rll rlr) ('B rrk rra rrl rrr)) z where
  balanceR = balanceR' (Proxy :: Proxy flag)

instance BalanceR k a ('B lk la 'T 'T) 'T ('B k a ('B lk la 'T 'T) 'T) where
  balanceR = FHBin

instance ( l ~ 'B lk la ll lr
         , r ~ 'B rk ra rl rr
         , isOther      ~ Not (Length r > Delta Lits.* Length l)
         , isRight      ~ 'True
         , isOtherInner ~ (Length rl < Ratio Lits.* Length rr)
         , Balance' isOther isRight isOtherInner k a l r z
         )
        => BalanceR k a ('B lk la ll lr) ('B rk ra rl rr) z where
  balanceR = balance' (Proxy :: Proxy isOther) (Proxy :: Proxy isRight) (Proxy :: Proxy isOtherInner)



class BalanceR' o k a l r z | o k a l r -> z where
  balanceR' :: Proxy o -> Proxy k -> f a -> FHMap f l -> FHMap f r -> FHMap f z

instance BalanceR' 'True k a 'T ('B rk ra rl rr) ('B rk ra ('B k a 'T rl) rr) where
  balanceR' _ k a FHTip (FHBin rk ra rl rr) = FHBin rk ra (FHBin k a FHTip rl) rr

instance BalanceR' 'False k a 'T ('B rk ra ('B rlk rla rll rlr) rr)
                      ('B rlk rla ('B k a 'T rll) ('B rk ra rlr rr)) where
  balanceR' _ k a FHTip (FHBin rk ra (FHBin rlk rla rll rlr) rr) =
    FHBin rlk rla (FHBin k a FHTip rll) (FHBin rk ra rlr rr)



class InsertMax k a m z | k a m -> z where
  insertMax :: Proxy k -> f a -> FHMap f m -> FHMap f z

instance InsertMax k a 'T ('B k a 'T 'T) where
  insertMax k a FHTip = singleton k a

instance (InsertMax q b r y, BalanceR k a l y z) => InsertMax q b ('B k a l r) z where
  insertMax q b (FHBin k a l r) = balanceR k a l (insertMax q b r)



class InsertMin k a m z | k a m -> z where
  insertMin :: Proxy k -> f a -> FHMap f m -> FHMap f z

instance InsertMin k a 'T ('B k a 'T 'T) where
  insertMin k a FHTip = singleton k a

instance (InsertMin q b l y, BalanceL k a y r z) => InsertMin q b ('B k a l r) z where
  insertMin q b (FHBin k a l r) = balanceL k a (insertMin q b l) r



class Link k a l r z | k a l r -> z where
  link :: Proxy k -> f a -> FHMap f l -> FHMap f r -> FHMap f z

instance InsertMin k a r z => Link k a 'T r z where
  link k a FHTip = insertMin k a

instance InsertMax k a ('B lk la ll lr) z => Link k a ('B lk la ll lr) 'T z where
  link k a l FHTip = insertMax k a l

instance ( l ~ 'B lk la ll lr
         , r ~ 'B rk ra rl rr
         , flag1 ~ (Delta Lits.* Length l < Length r)
         , flag2 ~ (Delta Lits.* Length r < Length l)
         , Link' flag1 flag2 k a l r z
         )
        => Link k a ('B lk la ll lr) ('B rk ra rl rr) z where
  link = link' (Proxy :: Proxy flag1) (Proxy :: Proxy flag2)

class Link' o1 o2 k a l r z | o1 o2 k a l r -> z where
  link' :: Proxy o1 -> Proxy o2 -> Proxy k -> f a -> FHMap f l -> FHMap f r -> FHMap f z

instance ( Link k a ('B lk la ll lr) rl y
         , BalanceL rk ra y rr z
         )
        => Link' 'True o2 k a ('B lk la ll lr) ('B rk ra rl rr) z where
  link' _ _ k a l (FHBin rk ra rl rr) = balanceL rk ra (link k a l rl) rr

instance ( Link k a lr ('B rk ra rl rr) y
         , BalanceR lk la ll y z
         )
        => Link' 'False 'True k a ('B lk la ll lr) ('B rk ra rl rr) z where
  link' _ _ k a (FHBin lk la ll lr) r = balanceR lk la ll (link k a lr r)

instance Link' 'False 'False k a l r ('B k a l r) where
  link' _ _ = FHBin



class Link2 l r z | l r -> z where
  link2 :: FHMap f l -> FHMap f r -> FHMap f z

instance Link2 'T r r where
  link2 FHTip r = r

instance Link2 ('B lk la ll lr) 'T ('B lk la ll lr) where
  link2 l FHTip = l

instance ( l ~ 'B lk la ll lr
         , r ~ 'B rk ra rl rr
         , flag1 ~ (Delta Lits.* Length l < Length r)
         , flag2 ~ (Delta Lits.* Length r < Length l)
         , Link2' flag1 flag2 l r z
         )
        => Link2 ('B lk la ll lr) ('B rk ra rl rr) z where
  link2 = link2' (Proxy :: Proxy flag1) (Proxy :: Proxy flag2)

class Link2' o1 o2 l r z | o1 o2 l r -> z where
  link2' :: Proxy o1 -> Proxy o2 -> FHMap f l -> FHMap f r -> FHMap f z

instance ( Link2 ('B lk la ll lr) rl y
         , BalanceL rk ra y rr z
         )
        => Link2' 'True o2 ('B lk la ll lr) ('B rk ra rl rr) z where
  link2' _ _ l (FHBin rk ra rl rr) = balanceL rk ra (link2 l rl) rr

instance ( Link2 lr ('B rk ra rl rr) y
         , BalanceR lk la ll y z
         )
        => Link2' 'False 'True ('B lk la ll lr) ('B rk ra rl rr) z where
  link2' _ _ (FHBin lk la ll lr) r = balanceR lk la ll (link2  lr r)

instance Glue l r z => Link2' 'False 'False l r z where
  link2' _ _ = glue



split :: SplitS k m l r => Proxy k -> FHMap f m -> (FHMap f l, FHMap f r)
split k = toPair . splitS k

class SplitS k m l r | k m -> l r where
  splitS :: Proxy k -> FHMap f m -> StrictPair (FHMap f l) (FHMap f r)

instance SplitS k 'T 'T 'T where
  splitS _ FHTip = FHTip :&: FHTip

instance ( flag ~ CmpSymbol q k
         , SplitS' flag q ('B k a l r) l' r'
         )
        => SplitS q ('B k a l r) l' r' where
  splitS = splitS' (Proxy :: Proxy flag)

class SplitS' o k m l r | o k m -> l r where
  splitS' :: Proxy o -> Proxy k -> FHMap f m -> StrictPair (FHMap f l) (FHMap f r)

instance ( SplitS q l lt gt
         , Link k a gt r r'
         )
        => SplitS' 'LT q ('B k a l r) lt r' where
  splitS' _ q (FHBin k a l r) =
    let lt :&: gt = splitS q l
    in lt :&: link k a gt r

instance ( SplitS q r lt gt
         , Link k a l lt l'
         )
        => SplitS' 'GT q ('B k a l r) l' gt where
  splitS' _ q (FHBin k a l r) =
    let lt :&: gt = splitS q r
    in link k a l lt :&: gt

instance SplitS' 'EQ k ('B k a l r) l r where
  splitS' _ _ (FHBin _ _ l r) = l :&: r



-- | The same as a regular Haskell pair, but
--
-- @
-- (x :*: _|_) = (_|_ :*: y) = _|_
-- @
data StrictPair a b = !a :&: !b

infixr 1 :&:

-- | Convert a strict pair to a standard pair.
toPair :: StrictPair a b -> (a, b)
toPair (x :&: y) = (x, y)
{-# INLINE toPair #-}

data StrictTriple a b c = StrictTriple !a !b !c



splitMember :: SplitMemberS k m l mv r => Proxy k -> FHMap f m -> (FHMap f l, Proxy mv, FHMap f r)
splitMember k m =
  let StrictTriple l mv r = splitMemberS k m
  in (l, mv, r)

class SplitMemberS k m l mv r | k m -> l mv r where
  splitMemberS :: Proxy k -> FHMap f m -> StrictTriple (FHMap f l) (Proxy mv) (FHMap f r)

instance SplitMemberS k 'T 'T 'False 'T where
  splitMemberS _ FHTip = StrictTriple FHTip (Proxy :: Proxy 'False) FHTip

instance ( flag ~ CmpSymbol q k
         , SplitMemberS' flag q ('B k a l r) l' mv' r'
         )
        => SplitMemberS q ('B k a l r) l' mv' r' where
  splitMemberS = splitMemberS' (Proxy :: Proxy flag)

class SplitMemberS' o k m l mv r | o k m -> l mv r where
  splitMemberS' :: Proxy o -> Proxy k -> FHMap f m -> StrictTriple (FHMap f l) (Proxy mv) (FHMap f r)

instance ( SplitMemberS q l lt mv gt
         , Link k a gt r r'
         )
        => SplitMemberS' 'LT q ('B k a l r) lt mv r' where
  splitMemberS' _ q (FHBin k a l r) =
    let StrictTriple lt z gt = splitMemberS q l
        !gt' = link k a gt r
    in StrictTriple lt z gt'

instance ( SplitMemberS q r lt mv gt
         , Link k a l lt l'
         )
        => SplitMemberS' 'GT q ('B k a l r) l' mv gt where
  splitMemberS' _ q (FHBin k a l r) =
    let StrictTriple lt z gt = splitMemberS q r
        !lt' = link k a l lt
    in StrictTriple lt' z gt

instance SplitMemberS' 'EQ k ('B k a l r) l 'True r where
  splitMemberS' _ _ (FHBin _ _ l r) = StrictTriple l (Proxy :: Proxy 'True) r



type Union = UnionC 'Clobber

-- | The expression @('union' t1 t2)@ takes the left-biased union of @t1@ and @t2@.
--   It prefers @t1@ when duplicate keys are encountered.
--
--   For every key already present in the map,
--   the associated value is replaced with the supplied value.
union :: Union l r z => FHMap f l -> FHMap f r -> FHMap f z
union = unionC (Proxy :: Proxy 'Clobber)



type UnionUnique = UnionC 'NoClobber

-- | 'union' based on 'insertUnique'.
unionUnique :: UnionUnique l r z => FHMap f l -> FHMap f r -> FHMap f z
unionUnique = unionC (Proxy :: Proxy 'NoClobber)



type UnionId = UnionC 'IfIdentical

-- | 'union' based on 'insertId'.
unionId :: UnionId l r z => FHMap f l -> FHMap f r -> FHMap f z
unionId = unionC (Proxy :: Proxy 'IfIdentical)



class UnionC (c :: Clobber) l r z | c l r -> z where
  unionC :: Proxy c -> FHMap f l -> FHMap f r -> FHMap f z

instance UnionC c l 'T l where
  unionC _ l FHTip = l

instance UnionC c 'T r r where
  unionC _ FHTip r = r

instance ( flag1 ~ (ll == 'T && lr == 'T)
         , flag2 ~ (rl == 'T && rr == 'T)
         , UnionC' flag1 flag2 c ('B lk la ll lr) ('B rk ra rl rr) z 
         )
        => UnionC c ('B lk la ll lr) ('B rk ra rl rr) z where
  unionC = unionC' (Proxy :: Proxy flag1) (Proxy :: Proxy flag2)

class UnionC' o1 o2 (c :: Clobber) l r z | o1 o2 c l r -> z where
  unionC' :: Proxy o1 -> Proxy o2 -> Proxy c -> FHMap f l -> FHMap f r -> FHMap f z

instance InsertC c rk ra l z => UnionC' 'False 'True c l ('B rk ra 'T 'T) z where
  unionC' _ _ c l (FHBin rk ra FHTip FHTip) = insertC c rk ra l

instance InsertC c lk la r z => UnionC' 'True o2 c ('B lk la 'T 'T) r z where
  unionC' _ _ c (FHBin lk la FHTip FHTip) = insertC c lk la

instance ( SplitS lk r l2 r2
         , UnionC c ll l2 l1l2
         , UnionC c lr r2 r1r2
         , Link lk la l1l2 r1r2 z
         )
       => UnionC' 'False 'False c ('B lk la ll lr) r z where
  unionC' _ _ c (FHBin lk la ll lr) r =
    let (l2, r2) = split lk r
        !l1l2 = unionC c ll l2
        !r1r2 = unionC c lr r2
    in link lk la l1l2 r1r2



type FromList = FromListC 'Clobber

fromList :: (Functor f, FromList f as bs) => FHList f as -> FHMap f bs
fromList = fromListC (Proxy :: Proxy 'Clobber)



type FromListUnique = FromListC 'NoClobber

-- | 'fromList' based on 'insertUnique'.
fromListUnique :: (Functor f, FromListUnique f as bs) => FHList f as -> FHMap f bs
fromListUnique = fromListC (Proxy :: Proxy 'NoClobber)



type FromListId = FromListC 'IfIdentical

-- | 'fromList' based on 'insertId'.
fromListId :: (Functor f, FromListId f as bs) => FHList f as -> FHMap f bs
fromListId = fromListC (Proxy :: Proxy 'IfIdentical)



type family NotOrdered a :: Bool where
  NotOrdered ((Proxy kx, _) ': (Proxy ky, _) ': _) = kx >= ky
  NotOrdered _                                     = 'False



class FromListC (c :: Clobber) f as bs | c as -> bs where
  fromListC :: Proxy c -> FHList f as -> FHMap f bs

instance FromListC c f '[] 'T where
  fromListC _ FHZero = FHTip

instance Functor f => FromListC c f '[(Proxy k, a)] ('B k a 'T 'T) where
  fromListC _ (v :&> FHZero) = FHBin (Proxy :: Proxy k) (snd <$> v) FHTip FHTip

instance ( flag ~ NotOrdered ((Proxy k, a) ': (Proxy l, b) ': as)
         , FromListC' flag c f ((Proxy k, a) ': (Proxy l, b) ': as) bs
         )
      => FromListC c f ((Proxy k, a) ': (Proxy l, b) ': as) bs where
  fromListC c m = fromListC' (Proxy :: Proxy flag) c m



class FromListFold (c :: Clobber) f as bs cs | c as bs -> cs where
  fromListFold :: Proxy c -> FHList f as -> FHMap f bs -> FHMap f cs

instance FromListFold c f '[] m m where
  fromListFold _ FHZero !m = m

instance ( Functor f
         , InsertC c k a bs cs
         , FromListFold c f as cs ds
         )
        => FromListFold c f ((Proxy k, a) ': as) bs ds where
  fromListFold c (v :&> as) = fromListFold c as . insertC c (Proxy :: Proxy k) (snd <$> v)



class FromListC' o (c :: Clobber) f as bs | o c as -> bs where
  fromListC' :: Proxy o -> Proxy c -> FHList f as -> FHMap f bs

instance FromListFold c f as 'T bs => FromListC' 'True c f as bs where
  fromListC' _ c as = fromListFold c as FHTip

instance ( Functor f
         , FromListGo 1 c f ('B k a 'T 'T) as bs
         )
      => FromListC' 'False c f ((Proxy k, a) ': as) bs where
  fromListC' _ c (v :&> as) =
    fromListGo (Proxy :: Proxy 1) c (FHBin (Proxy :: Proxy k) (snd <$> v) FHTip FHTip) as



class FromListGo (n :: Nat) (c :: Clobber) f as bs cs | n c as bs -> cs where
  fromListGo :: Proxy n -> Proxy c -> FHMap f as -> FHList f bs -> FHMap f cs

instance FromListGo n c f as '[] as where
  fromListGo _ _ t FHZero = t

instance ( Functor f
         , InsertMax k a as bs
         )
        => FromListGo n c f as '[(Proxy k, a)] bs where
  fromListGo _ _ t (v :&> FHZero) = insertMax (Proxy :: Proxy k) (snd <$> v) t

instance ( Functor f
         , bss ~ ((Proxy l, b) ': bs)
         , bsss ~ ((Proxy k, a) ': bss)
         , flag1 ~ (n == 1)
         , flag2 ~ NotOrdered bss
         , FromListCreate flag1 flag2 n f bss tree ys zs
         , flag3 ~ NotOrdered bsss
         , flag4 ~ (zs == '[])
         , FromListGo' flag3 flag4 n c f as bsss cs
         )
        => FromListGo n c f as ((Proxy k, a) ': (Proxy l, b) ': bs) cs where
  fromListGo = fromListGo' (Proxy :: Proxy flag3) (Proxy :: Proxy flag4)



class FromListGo' o1 o2 (n :: Nat) (c :: Clobber) f as bs cs | o1 o2 n c as bs -> cs where
  fromListGo' :: Proxy o1 -> Proxy o2 -> Proxy n -> Proxy c -> FHMap f as -> FHList f bs -> FHMap f cs

instance FromListFold c f bs as cs => FromListGo' 'True o2 n c f as bs cs where
  fromListGo' _ _ _ c = flip (fromListFold c)

instance ( Functor f
         , flag1 ~ (n == 1)
         , flag2 ~ NotOrdered bs
         , FromListCreate flag1 flag2 n f bs tree ys '[]
         , Link k a as tree ls
         , FromListGo (n Lits.* 2) c f ls ys cs
         )
        => FromListGo' 'False 'True n c f as ((Proxy k, a) ': bs) cs where
  fromListGo' _ _ n c l (v :&> xss) =
    let (r, ys, FHZero) = fromListCreate (Proxy :: Proxy flag1) (Proxy :: Proxy flag2) n xss
    in fromListGo (Proxy :: Proxy (n Lits.* 2)) c (link (Proxy :: Proxy k) (snd <$> v) l r) ys

instance ( Functor f
         , flag1 ~ (n == 1)
         , flag2 ~ NotOrdered bs
         , FromListCreate flag1 flag2 n f bs tree zs ys
         , Link k a as tree ls
         , FromListFold c f ys ls cs
         )
        => FromListGo' 'False 'False n c f as ((Proxy k, a) ': bs) cs where
  fromListGo' _ _ n c l (v :&> xss) =
    let (r, _, ys) = fromListCreate (Proxy :: Proxy flag1) (Proxy :: Proxy flag2) n xss
    in fromListFold c ys (link (Proxy :: Proxy k) (snd <$> v) l r)




class FromListCreate o1 o2 (n :: Nat) f as tree ys zs | o1 o2 n as -> tree ys zs where
  fromListCreate :: Proxy o1 -> Proxy o2 -> Proxy n -> FHList f as -> (FHMap f tree, FHList f ys, FHList f zs)

instance FromListCreate o1 o2 n f '[] 'T '[] '[] where
  fromListCreate _ _ _ FHZero = (FHTip, FHZero, FHZero)

instance Functor f => FromListCreate 'True 'True 1 f ((Proxy k, a) ': as) ('B k a 'T 'T) '[] as where
  fromListCreate _ _ _ (v :&> as) = (FHBin (Proxy :: Proxy k) (snd <$> v) FHTip FHTip, FHZero, as)

instance Functor f => FromListCreate 'True 'False 1 f ((Proxy k, a) ': as) ('B k a 'T 'T) as '[] where
  fromListCreate _ _ _ (v :&> as) = (FHBin (Proxy :: Proxy k) (snd <$> v) FHTip FHTip, as, FHZero)

instance ( Functor f
         , flag ~ (Div n 2 == 1)
         , FromListCreate flag o2 (n `Div` 2) f ((Proxy k, a) ': as) tree ys zs
         , FromListCreate' tree ys zs (NotOrdered ys) n f as tree' ys' zs'
         )
        => FromListCreate 'False o2 n f ((Proxy k, a) ': as) tree' ys' zs' where
  fromListCreate _ _ _ (v :&> as) =
    let (tree, ys, zs) = fromListCreate
                           (Proxy :: Proxy flag)
                           (Proxy :: Proxy o2)
                           (Proxy :: Proxy (n `Div` 2))
                           (v :&> as)
    in fromListCreate' tree ys zs (Proxy :: Proxy (NotOrdered ys)) (Proxy :: Proxy n) as



class FromListCreate' tree ys zs o (n :: Nat) f as tree' ys' zs'
                        | tree ys zs o n as -> tree' ys' zs' where
  fromListCreate'
    :: FHMap f tree
    -> FHList f ys
    -> FHList f zs
    -> Proxy o
    -> Proxy n
    -> FHList f as
    -> (FHMap f tree', FHList f ys', FHList f zs')

instance FromListCreate' tree '[] zs o n f as tree '[] zs where
  fromListCreate' tree ys zs _ _ _ = (tree, ys, zs)

instance ( Functor f
         , InsertMax k a tree tree'
         )
        => FromListCreate' tree '[(Proxy k, a)] zs o n f as tree' '[] zs where
  fromListCreate' l (v :&> FHZero) zs _ _ _ =
    (insertMax (Proxy :: Proxy k) (snd <$> v) l, FHZero, zs)

instance FromListCreate' tree ((Proxy k, a) ': (Proxy l, b) ': ys) zs 'True n f as
           tree '[] ((Proxy k, a) ': (Proxy l, b) ': ys) where
  fromListCreate' l ys _ _ _ _ = (l, FHZero, ys)

instance ( Functor f
         , yss ~ ((Proxy l, b) ': ys)
         , flag1 ~ (Div n 2 == 1)
         , flag2 ~ NotOrdered ((Proxy k, a) ': yss)
         , FromListCreate flag1 flag2 (n `Div` 2) f yss r zs' ws
         , Link k a tree r tree'
         )
        => FromListCreate' tree ((Proxy k, a) ': (Proxy l, b) ': ys) zs 'False n f as 
             tree' zs' ws where
  fromListCreate' l (v :&> ys) _ _ _ _ =
    let (r, zs, ws) = fromListCreate (Proxy :: Proxy flag1) (Proxy :: Proxy flag2)
                                       (Proxy :: Proxy (n `Div` 2)) ys
    in (link (Proxy :: Proxy k) (snd <$> v) l r, zs, ws)




class Glue l r z | l r -> z where
  glue :: FHMap f l -> FHMap f r -> FHMap f z

instance Glue 'T r r where
  glue FHTip r = r

instance Glue ('B lk la ll lr) 'T ('B lk la ll lr) where
  glue l FHTip = l

instance ( l ~ 'B lk la ll lr
         , r ~ 'B rk ra rl rr
         , flag ~ (Length l > Length r)
         , Glue' flag l r z
         )
        => Glue ('B lk la ll lr) ('B rk ra rl rr) z where
  glue = glue' (Proxy :: Proxy flag)



class Glue' o l r z | o l r -> z where
  glue' :: Proxy o -> FHMap f l -> FHMap f r -> FHMap f z

instance ( MaxViewSure lk la ll lr mk ma l'
         , BalanceR mk ma l' ('B rk ra rl rr) z
         )
        => Glue' 'True ('B lk la ll lr) ('B rk ra rl rr) z where
  glue' _ (FHBin lk la ll lr) r =
    let !(MaxView mk ma l') = maxViewSure lk la ll lr
    in balanceR mk ma l' r

instance ( MinViewSure rk ra rl rr mk ma r'
         , BalanceL mk ma ('B lk la ll lr) r' z
         )
        => Glue' 'False ('B lk la ll lr) ('B rk ra rl rr) z where
  glue' _ l (FHBin rk ra rl rr) =
    let !(MinView mk ma r') = minViewSure rk ra rl rr
    in balanceL mk ma l r'



data MinView f k a as = MinView !(Proxy k) (f a) !(FHMap f as)

class MinViewSure k a l r k' a' z | k a l r -> k' a' z where
  minViewSure :: Proxy k -> f a -> FHMap f l -> FHMap f r -> MinView f k' a' z

instance MinViewSure k a 'T r k a r where
  minViewSure k a FHTip r = MinView k a r

instance ( MinViewSure lk la ll lr k' a' l'
         , BalanceR k a l' r z
         )
        => MinViewSure k a ('B lk la ll lr) r k' a' z where
  minViewSure k a (FHBin lk la ll lr) r =
    let MinView mk ma l' = minViewSure lk la ll lr
    in MinView mk ma (balanceR k a l' r)



data MaxView f k a as = MaxView !(Proxy k) (f a) !(FHMap f as)

class MaxViewSure k a l r k' a' z | k a l r -> k' a' z where
  maxViewSure :: Proxy k -> f a -> FHMap f l -> FHMap f r -> MaxView f k' a' z

instance MaxViewSure k a l 'T k a l where
  maxViewSure k a l FHTip = MaxView k a l

instance ( MaxViewSure rk ra rl rr k' a' r'
         , BalanceL k a l r' z
         )
        => MaxViewSure k a l ('B rk ra rl rr) k' a' z where
  maxViewSure k a l (FHBin rk ra rl rr) =
    let MaxView mk ma r' = maxViewSure rk ra rl rr
    in MaxView mk ma (balanceL k a l r')



type Delete = DeleteI 'IfExists

-- | Delete a key and its value from the map.
--   When the key is not a member of the map, a type error is thrown.
delete :: Delete k m d z => Proxy k -> FHMap f m -> FHMap f z
delete k = snd . deleteI (Proxy :: Proxy 'IfExists) k



type Delete' = DeleteI 'EitherWay

-- | Delete a key and its value from the map.
--   When the key is not a member of the map, the original map is returned.
delete' :: Delete' k m d z => Proxy k -> FHMap f m -> FHMap f z
delete' k = snd . deleteI (Proxy :: Proxy 'EitherWay) k



data IfExists = IfExists
              | EitherWay

type family Exists (a :: IfExists) (b :: Bool) (s1 :: Symbol) (q :: k) (s2 :: Symbol) :: Constraint where
  Exists 'IfExists 'False s1 k s2 = TypeError ( 'Text s1
                                          ':<>: 'ShowType k
                                          ':<>: 'Text s2
                                              )
  Exists _         _      _  _ _  = ()

class DeleteI (i :: IfExists) k m (d :: Bool) z | i k m -> d z where
  deleteI :: Proxy i -> Proxy k -> FHMap f m -> (Proxy d, FHMap f z)

instance DeleteI i k 'T 'False 'T where
  deleteI _ _ FHTip = ((Proxy :: Proxy 'False), FHTip)

instance ( Exists i d "Key " q " is not in the map"
         , DeleteI' (CmpSymbol q k) i q ('B k a l r) d z
         )
        => DeleteI i q ('B k a l r) d z where
  deleteI = deleteI' (Proxy :: Proxy (CmpSymbol q k))

class DeleteI' o (i :: IfExists) k m (d :: Bool) z | o i k m -> d z where
  deleteI' :: Proxy o -> Proxy i -> Proxy k -> FHMap f m -> (Proxy d, FHMap f z)

instance ( DeleteI i q l d l'
         , BalanceR k a l' r z
         )
        => DeleteI' 'LT i q ('B k a l r) d z where
  deleteI' _ i q (FHBin k a l r) =
    let (d, l') = deleteI i q l
    in (d, balanceR k a l' r)

instance ( DeleteI i q r d r'
         , BalanceL k a l r' z
         )
        => DeleteI' 'GT i q ('B k a l r) d z where
  deleteI' _ i q (FHBin k a l r) =
    let (d, r') = deleteI i q r
    in (d, balanceL k a l r')

instance Glue l r z => DeleteI' 'EQ i q ('B k a l r) 'True z where
  deleteI' _ _ _ (FHBin _ _ l r) = (Proxy :: Proxy 'True, glue l r)



type Adjust = AdjustI 'IfExists

-- | Update a value at a specific key with the result of the provided function.
--   
--   When the key is not a member of the map, a type error is thrown.
adjust :: Adjust a b k m d z => (f a -> f b) -> Proxy k -> FHMap f m -> FHMap f z
adjust f k = snd . adjustI (Proxy :: Proxy 'IfExists) f k



type Adjust' = AdjustI 'EitherWay

-- | Update a value at a specific key with the result of the provided function.
--   
--   When the key is not a member of the map, the original map is returned.
adjust' :: Adjust' a b k m d z => (f a -> f b) -> Proxy k -> FHMap f m -> FHMap f z
adjust' f k = snd . adjustI (Proxy :: Proxy 'EitherWay) f k



class AdjustI (i :: IfExists) a b k m (d :: Bool) z | i k m b -> a d z where
  adjustI :: Proxy i -> (f a -> f b) -> Proxy k -> FHMap f m -> (Proxy d, FHMap f z)

instance AdjustI i Void b k 'T 'False 'T where
  adjustI _ _ _ FHTip = ((Proxy :: Proxy 'False), FHTip)

instance ( Exists i d "Key " q " is not in the map"
         , AdjustI' (CmpSymbol q k) i a b q ('B k c l r) d z
         )
        => AdjustI i a b q ('B k c l r) d z where
  adjustI = adjustI' (Proxy :: Proxy (CmpSymbol q k))

class AdjustI' o (i :: IfExists) a b k m (d :: Bool) z | o i k m b -> a d z where
  adjustI' :: Proxy o -> Proxy i -> (f a -> f b) -> Proxy k -> FHMap f m -> (Proxy d, FHMap f z)

instance AdjustI i a b q l d l' => AdjustI' 'LT i a b q ('B k c l r) d ('B k c l' r) where
  adjustI' _ i f q (FHBin k a l r) =
    let (d, l') = adjustI i f q l
    in (d, FHBin k a l' r)

instance AdjustI i a b q r d r' => AdjustI' 'GT i a b q ('B k c l r) d ('B k c l r') where
  adjustI' _ i f q (FHBin k a l r) =
    let (d, r') = adjustI i f q r
    in (d, FHBin k a l r')

instance AdjustI' 'EQ i a b q ('B k a l r) 'True ('B k b l r) where
  adjustI' _ _ f _ (FHBin k a l r) = (Proxy :: Proxy 'True, FHBin k (f a) l r)


-- | Same as 'difference'.
(\\) :: Difference a b z => FHMap f a -> FHMap f b -> FHMap f z
(\\) = difference

class Difference a b z | a b -> z where
  -- | Difference of two maps.
  --   Return elements of the first map not existing in the second map.
  difference :: FHMap f a -> FHMap f b -> FHMap f z

instance Difference 'T b 'T where
  difference FHTip _ = FHTip

instance Difference ('B ak aa al ar) 'T ('B ak aa al ar) where
  difference a FHTip = a

instance ( SplitS bk ('B ak aa al ar) l1 r1
         , Difference l1 bl l1l2
         , Difference r1 br r1r2
         , flag ~ (Length l1l2 + Length r1r2 == Length ('B ak aa al ar))
         , Difference' flag ('B ak aa al ar) ('B bk ba bl br) z
         )
        => Difference ('B ak aa al ar) ('B bk ba bl br) z where
  difference = difference' (Proxy :: Proxy flag)

class Difference' (o :: Bool) a b z | a b -> z where
  difference' :: Proxy o -> FHMap f a -> FHMap f b -> FHMap f z

instance Difference' 'True a b a where
  difference' _ a _ = a

instance ( SplitS bk ('B ak aa al ar) l1 r1
         , Difference l1 bl l1l2
         , Difference r1 br r1r2
         , Link2 l1l2 r1r2 z
         )
        => Difference' 'False ('B ak aa al ar) ('B bk ba bl br) z where
  difference' _ t1 (FHBin k _ l2 r2) =
    let (l1, r1) = split k t1
        !l1l2 = difference l1 l2
        !r1r2 = difference r1 r2
    in link2 l1l2 r1r2



class Intersection a b z | a b -> z where
  -- | Intersection of two maps.
  --   Return data in the first map for the keys existing in both maps.
  intersection :: FHMap f a -> FHMap f b -> FHMap f z

instance Intersection 'T b 'T where
  intersection FHTip _ = FHTip

instance Intersection ('B ak aa al ar) 'T ('B ak aa al ar) where
  intersection a FHTip = a

instance ( SplitMemberS ak ('B bk ba bl br) l2 flag r2
         , Intersection' flag ('B ak aa al ar) ('B bk ba bl br) z
         )
        => Intersection ('B ak aa al ar) ('B bk ba bl br) z where
  intersection = intersection' (Proxy :: Proxy flag)

class Intersection' (o :: Bool) a b z | a b -> z where
  intersection' :: Proxy o -> FHMap f a -> FHMap f b -> FHMap f z

instance ( SplitMemberS k t2 l2 mv r2
         , Intersection l1 l2 l1l2
         , Intersection r1 r2 r1r2
         , Link k a l1l2 r1r2 z
         )
       => Intersection' 'True ('B k a l1 r1) t2 z where
  intersection' _ (FHBin k a l1 r1) t2 =
    let (l2, _, r2) = splitMember k t2
        !l1l2 = intersection l1 l2
        !r1r2 = intersection r1 r2
    in link k a l1l2 r1r2

instance ( SplitMemberS k t2 l2 mv r2
         , Intersection l1 l2 l1l2
         , Intersection r1 r2 r1r2
         , Link2 l1l2 r1r2 z
         )
        => Intersection' 'False ('B k a l1 r1) t2 z where
  intersection' _ (FHBin k _ l1 r1) t2 =
    let (l2, _, r2) = splitMember k t2
        !l1l2 = intersection l1 l2
        !r1r2 = intersection r1 r2
    in link2 l1l2 r1r2
