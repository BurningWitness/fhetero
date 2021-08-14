{-# LANGUAGE BangPatterns
           , ConstraintKinds
           , DataKinds
           , FlexibleContexts
           , FlexibleInstances
           , FunctionalDependencies
           , GADTs
           , KindSignatures
           , PatternSynonyms
           , PolyKinds
           , Rank2Types
           , ScopedTypeVariables
           , TypeFamilies
           , TypeOperators
           , UndecidableInstances #-}

{-# OPTIONS_HADDOCK not-home #-}

module Data.FHList.Internal where

import           Data.FHFoldable
import           Data.FHFunctor
import           Data.FHTraversable
import           Data.Type.Eq
import           Data.Type.Length
import           Data.Type.Materialize
import           Data.Type.Ord

import           Data.Type.Bool
import           Data.Proxy
import           GHC.Generics
import           GHC.TypeLits as Lits
import           Prelude hiding ( map, concat, foldr, foldl, foldMap, replicate
                                , traverse, (++), (!!), take, drop, zip, zip3
                                )



infixr 4 :&>
-- | A heterogenous list where each element (stored in a list @as@) is wrapped into @f@.
data FHList (f :: k -> *) (as :: [k]) where
  (:&>) :: f a -> FHList f as -> FHList f (a ': as)
  FHZero :: FHList t '[]



instance GenericH f as => Generic (FHList f as) where
  type Rep (FHList f as) =
         D1 ('MetaData "FHList" "Data.FHList" "fhetero" 'False)
           ( RepH f as )

  from = M1 <$> fromH

  to = toH . unM1

class GenericH f as where
  type RepH f as :: * -> *

  fromH :: FHList f as -> RepH f as x

  toH :: RepH f as x -> FHList f as

instance GenericH f '[] where
  type RepH f '[] = C1 ('MetaCons "FHZero" 'PrefixI 'False)
                      U1

  fromH FHZero = M1 U1

  toH (M1 U1) = FHZero

instance GenericH f as => GenericH f (a ': as) where
  type RepH f (a ': as) = C1 ('MetaCons ":&>" ('InfixI 'RightAssociative 4) 'False)
                            ( S1 ('MetaSel 'Nothing
                                             'NoSourceUnpackedness
                                             'NoSourceStrictness
                                             'DecidedLazy)
                                    (Rec0 (f a))
                              :*:
                              RepH f as
                            )

  fromH (a :&> as) = M1 $ M1 (K1 a) :*: fromH as

  toH (M1 (M1 (K1 a) :*: as)) = a :&> toH as




-- | Matches a 'FHList' of two elements.
pattern (:&>:) :: t a -> t b -> FHList t '[a, b]
pattern (:&>:) a b = a :&> b :&> FHZero



instance ShowFH 'True f as => Show (FHList f as) where
  showsPrec d as = showParen (d > 10) $
                       showString "fromList ["
                     . showFH (Proxy :: Proxy 'True) as
                     . showChar ']'

class ShowFH b f a where
  showFH :: Proxy b -> FHList f a -> ShowS

instance ShowFH b f '[] where
  showFH _ FHZero = id

instance (Show (f a), ShowFH 'False f as) => ShowFH 'True f (a:as) where
  showFH _ (a :&> b) = shows a . showFH (Proxy :: Proxy 'False) b

instance (Show (f a), ShowFH 'False f as) => ShowFH 'False f (a:as) where
  showFH _ (a :&> b) = showChar ',' . shows a . showFH (Proxy :: Proxy 'False) b




instance Map p as => FHFunctor FHList p as where
  fhfmap = map

class Map p as where
  map :: Proxy p -> (forall a. p a => f a -> g a) -> FHList f as -> FHList g as

instance Map p '[] where
  map _ _ FHZero = FHZero

instance (p a, Map p as) => Map p (a : as) where
  map p f (a :&> as) = f a :&> map p f as



instance Fold p as => FHFoldable FHList p as where
  fhfoldMap = foldMap
  fhfoldl = foldl
  fhfoldl' = foldl'
  fhfoldr = foldr
  fhfoldr' = foldr'

class Fold p as where
  foldMap :: Monoid m => Proxy p -> (forall a. p a => f a -> m) -> FHList f as -> m

  foldr :: Proxy p -> (forall a. p a => f a -> b -> b) -> b -> FHList f as -> b

  foldr' :: Proxy p -> (forall a. p a => f a -> b -> b) -> b -> FHList f as -> b

  foldl :: Proxy p -> (forall a. p a => b -> f a -> b) -> b -> FHList f as -> b

  foldl' :: Proxy p -> (forall a. p a => b -> f a -> b) -> b -> FHList f as -> b

  zip
    :: Proxy p
    -> (forall a. p a => f a -> g a -> h a)
    -> FHList f as
    -> FHList g as
    -> FHList h as

  zip3
    :: Proxy p
    -> (forall a. p a => f a -> g a -> h a -> i a)
    -> FHList f as
    -> FHList g as
    -> FHList h as
    -> FHList i as

  zip4
    :: Proxy p
    -> (forall a. p a => f a -> g a -> h a -> i a -> j a)
    -> FHList f as
    -> FHList g as
    -> FHList h as
    -> FHList i as
    -> FHList j as

  zip5
    :: Proxy p
    -> (forall a. p a => f a -> g a -> h a -> i a -> j a -> l a)
    -> FHList f as
    -> FHList g as
    -> FHList h as
    -> FHList i as
    -> FHList j as
    -> FHList l as

  zip6
    :: Proxy p
    -> (forall a. p a => f a -> g a -> h a -> i a -> j a -> l a -> m a)
    -> FHList f as
    -> FHList g as
    -> FHList h as
    -> FHList i as
    -> FHList j as
    -> FHList l as
    -> FHList m as

  zip7
    :: Proxy p
    -> (forall a. p a => f a -> g a -> h a -> i a -> j a -> l a -> m a -> n a)
    -> FHList f as
    -> FHList g as
    -> FHList h as
    -> FHList i as
    -> FHList j as
    -> FHList l as
    -> FHList m as
    -> FHList n as



instance Fold p '[] where
  foldMap _ _ FHZero = mempty

  foldr _ _ t FHZero = t

  foldr' _ _ t FHZero = t

  foldl _ _ t FHZero = t

  foldl' _ _ !t FHZero = t

  zip _ _ FHZero FHZero = FHZero

  zip3 _ _ FHZero FHZero FHZero = FHZero

  zip4 _ _ FHZero FHZero FHZero FHZero = FHZero

  zip5 _ _ FHZero FHZero FHZero FHZero FHZero = FHZero

  zip6 _ _ FHZero FHZero FHZero FHZero FHZero FHZero = FHZero

  zip7 _ _ FHZero FHZero FHZero FHZero FHZero FHZero FHZero = FHZero

instance (p a, Fold p as) => Fold p (a : as) where
  foldMap p f (a :&> as) = f a <> foldMap p f as

  foldr p f t (a :&> as) = f a $ foldr p f t as

  foldr' p f t (a :&> as) = f a $! foldr' p f t as

  foldl p f t (a :&> as) = foldl p f (f t a) as

  foldl' p f t (a :&> as) = foldl' p f (id $! f t a) as

  zip p f (a :&> as) (b :&> bs) = f a b :&> zip p f as bs

  zip3 p f (a :&> as) (b :&> bs) (c :&> cs) = f a b c :&> zip3 p f as bs cs

  zip4 p f (a :&> as) (b :&> bs) (c :&> cs) (d :&> ds) = f a b c d :&> zip4 p f as bs cs ds

  zip5 p f (a :&> as) (b :&> bs) (c :&> cs) (d :&> ds) (e :&> es) =
    f a b c d e :&> zip5 p f as bs cs ds es

  zip6 p f (a :&> as) (b :&> bs) (c :&> cs) (d :&> ds) (e :&> es) (g :&> gs) =
    f a b c d e g :&> zip6 p f as bs cs ds es gs

  zip7 p f (a :&> as) (b :&> bs) (c :&> cs) (d :&> ds) (e :&> es) (g :&> gs) (h :&> hs) =
    f a b c d e g h :&> zip7 p f as bs cs ds es gs hs



instance Traverse p as => FHTraversable FHList p as where
  fhtraverse = traverse

class Traverse p as where
  traverse
    :: Applicative m
    => Proxy p
    -> (forall a. p a => f a -> m (g a))
    -> FHList f as
    -> m (FHList g as)

  mapAccumL :: Proxy p -> (forall a. p a => b -> f a -> (b, g a)) -> b -> FHList f as -> (b, FHList g as)

  mapAccumR :: Proxy p -> (forall a. p a => b -> f a -> (b, g a)) -> b -> FHList f as -> (b, FHList g as)

instance Traverse p '[] where
  traverse _ _ FHZero = pure FHZero

  mapAccumL _ _ acc FHZero = (acc, FHZero)

  mapAccumR _ _ acc FHZero = (acc, FHZero)

instance (p a, Traverse p as) => Traverse p (a ': as) where
  traverse p f (a :&> as) = (:&>) <$> f a <*> traverse p f as

  mapAccumL p f acc (a :&> as) = let (acc', a') = f acc a
                                     (acc'', as') = mapAccumL p f acc' as
                                 in (acc'', a' :&> as')

  mapAccumR p f acc (a :&> as) = let (acc', as') = mapAccumR p f acc as
                                     (acc'', a') = f acc' a
                                 in (acc'', a' :&> as')



empty :: FHList f '[]
empty = FHZero



singleton :: f a -> FHList f '[a]
singleton = (:&> FHZero)



null :: FHList f as -> Bool
null FHZero = True
null _      = False



-- | Length of the list is known at compilation time.
length :: KnownNat (Length as) => FHList f as -> Integer
length (_ :: FHList f as) = natVal (Proxy :: Proxy (Length as))



class At n as a | n as -> a where
  (!!) :: FHList f as -> Proxy n -> f a

instance (flag ~ (n == 0), At' flag n as a) => At n as a where
  (!!) = flip $ at' (Proxy :: Proxy flag)

class At' flag n as a | n as -> a where
  at' :: Proxy flag -> Proxy n -> FHList f as -> f a

instance At' 'True 0 (a ': as) a where
  at' _ _ (a :&> _) = a

instance At (n - 1) as b => At' 'False n (a ': as) b where
  at' _ _ (_ :&> as) = as !! (Proxy :: Proxy (n - 1))



class Take n as bs | n as -> bs where
  take :: Proxy n -> FHList f as -> FHList f bs

instance (flag ~ (n == 0), Take' flag n as bs) => Take n as bs where
  take = take' (Proxy :: Proxy flag)

class Take' flag n as bs | flag n as -> bs where
  take' :: Proxy flag -> Proxy n -> FHList f as -> FHList f bs

instance Take' 'True 0 as '[] where
  take' _ _ _ = FHZero

instance Take (n - 1) as bs => Take' 'False n (a ': as) (a ': bs) where
  take' _ _ (a :&> as) = a :&> take (Proxy :: Proxy (n - 1)) as



class Drop n as bs | n as -> bs where
  drop :: Proxy n -> FHList f as -> FHList f bs

instance (flag ~ (n == 0), Drop' flag n as bs) => Drop n as bs where
  drop = drop' (Proxy :: Proxy flag)

class Drop' flag n as bs | flag n as -> bs where
  drop' :: Proxy flag -> Proxy n -> FHList f as -> FHList f bs

instance Drop' 'True 0 as as where
  drop' _ _ as = as

instance Drop' 'False n '[] '[] where
  drop' _ _ FHZero = FHZero

instance Drop (n - 1) as bs => Drop' 'False n (a ': as) bs where
  drop' _ _ (_ :&> as) = drop (Proxy :: Proxy (n - 1)) as



elem :: Elem a as bool => Proxy a -> FHList f as -> Bool
elem a as = materialize $ elem' a as

class Materialize Bool bool => Elem a as bool | a as -> bool where
  elem' :: Proxy a -> FHList f as -> Proxy bool

instance Elem a '[] 'False where
  elem' _ FHZero = Proxy

instance ( Elem a as next
         , flag ~ (a == b || next)
         , Materialize Bool flag
         )
      => Elem a (b ': as) flag where
  elem' _ _ = Proxy



-- | Helper typeclass that along with providing the looked up value also returns
--   the entire remaining list.
class Retrieve a as bs | a as -> bs where
  retrieve :: Proxy a -> FHList f as -> (f a, FHList f bs)

instance Lits.TypeError ('ShowType a ':<>: 'Lits.Text " is not an element of the type list")
      => Retrieve a '[] '[] where
  retrieve = undefined -- Fails at compilation time, value doesn't matter

instance ( flag ~ (a == b)
         , Retrieve' flag a (b ': as) bs
         )
        => Retrieve a (b:as) bs where
  retrieve = retrieve' (Proxy :: Proxy flag)

class Retrieve' flag a as bs | flag a as -> bs where
  retrieve' :: Proxy flag -> Proxy a -> FHList f as -> (f a, FHList f bs)

instance Retrieve' 'True a (a ': as) as where
  retrieve' _ _ (a :&> as) = (a, as)

instance Retrieve a as bs => Retrieve' 'False a (b ': as) (b ': bs) where
  retrieve' _ p (a :&> as) =
    let (b, bs) = retrieve p as
    in (b, a :&> bs)

lookup :: Retrieve a as bs => Proxy a -> FHList f as -> f a
lookup p = fst . retrieve p

delete :: Retrieve a as bs => Proxy a -> FHList f as -> FHList f bs
delete p = snd . retrieve p



-- | Helper typeclass that drops all occurences of the value from the list.
class Remove a as bs | a as -> bs where
  remove :: Proxy a -> FHList f as -> FHList f bs

instance Remove a '[] '[] where
  remove _ FHZero = FHZero

instance ( flag ~ (a == b)
         , Remove' flag a (b ': as) bs
         )
        => Remove a (b:as) bs where
  remove = remove' (Proxy :: Proxy flag)

class Remove' flag a as bs | flag a as -> bs where
  remove' :: Proxy flag -> Proxy a -> FHList f as -> FHList f bs

instance Remove a as bs => Remove' 'True a (a ': as) bs where
  remove' _ p (_ :&> as) = remove p as

instance Remove a as bs => Remove' 'False a (b ': as) (b ': bs) where
  remove' _ p (a :&> as) = a :&> remove p as



class Nub as bs | as -> bs where
  nub :: FHList f as -> FHList f bs

instance Nub '[] '[] where
  nub FHZero = FHZero

instance Remove a as bs => Nub (a ': as) (a ': bs) where
  nub (a :&> as) = a :&> remove (Proxy :: Proxy a) as



class Concat as bs cs | as bs -> cs where
  (++) :: FHList f as -> FHList f bs -> FHList f cs

instance Concat as '[] as where
  (++) as FHZero = as

instance Concat '[] bs bs where
  (++) FHZero bs = bs

instance Concat as (b ': bs) cs => Concat (a ': as) (b ': bs) (a ': cs) where
  (++) (a :&> as) bs = a :&> as ++ bs



class Replicate n a as | n a -> as where
  replicate :: Proxy n -> f a -> FHList f as

instance (flag ~ (n < 1), Replicate' flag n a as) => Replicate n a as where
  replicate = replicate' (Proxy :: Proxy flag)

class Replicate' flag n a as | flag n a -> as where
  replicate' :: Proxy flag -> Proxy n -> f a -> FHList f as

instance Replicate' 'True n a '[] where
  replicate' _ _ _ = FHZero

instance Replicate (n - 1) a as => Replicate' 'False n a (a ': as) where
  replicate' _ _ a = a :&> replicate (Proxy :: Proxy (n - 1)) a
