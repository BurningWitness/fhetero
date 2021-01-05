{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.FHList.Internal where

import           Data.Type.Bool
import           Data.Type.Extra

import           Data.List (intercalate)
import           Data.Proxy
import           GHC.Generics
import           GHC.TypeLits as Lits
import           Prelude hiding ( map, concat, foldr, foldl, foldMap
                                , traverse, (++), (!!), take, drop
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



instance ShowFH f a => Show (FHList f a) where
  show = ("fromList " <>) . (\a -> "[" <> a <> "]") . intercalate "," . showFH

class ShowFH f a where
  showFH :: FHList f a -> [String]

instance ShowFH f '[] where
  showFH FHZero = []

instance (Show (f a), ShowFH f as) => ShowFH f (a:as) where
  showFH (a :&> b) = show a : showFH b




class Map p as where
  map :: Proxy p -> (forall a. p a => f a -> g a) -> FHList f as -> FHList g as

instance Map p '[] where
  map _ _ FHZero = FHZero

instance (p a, Map p as) => Map p (a : as) where
  map p f (a :&> as) = f a :&> map p f as



class Fold p as where
  foldMap :: Monoid m => Proxy p -> (forall a. p a => f a -> m) -> FHList f as -> m

  foldr :: Proxy p -> (forall a. p a => f a -> b -> b) -> b -> FHList f as -> b

  foldr' :: Proxy p -> (forall a. p a => f a -> b -> b) -> b -> FHList f as -> b

  foldl :: Proxy p -> (forall a. p a => b -> f a -> b) -> b -> FHList f as -> b

  foldl' :: Proxy p -> (forall a. p a => b -> f a -> b) -> b -> FHList f as -> b

instance Fold p '[] where
  foldMap _ _ FHZero = mempty

  foldr _ _ t FHZero = t

  foldr' _ _ !t FHZero = t

  foldl _ _ t FHZero = t

  foldl' _ _ !t FHZero = t

instance (p a, Fold p as) => Fold p (a : as) where
  foldMap p f (a :&> as) = f a <> foldMap p f as

  foldr p f t (a :&> as) = f a $ foldr p f t as

  foldr' p f t (a :&> as) = f a $ foldr' p f t as

  foldl p f t (a :&> as) = foldl p f (f t a) as

  foldl' p f t (a :&> as) = foldl' p f (f t a) as



class Traverse p as where
  traverse
    :: Applicative m
    => Proxy p
    -> (forall a. p a => f a -> m (g a))
    -> FHList f as
    -> m (FHList g as)

instance Traverse p '[] where
  traverse _ _ FHZero = pure FHZero

instance (p a, Traverse p as) => Traverse p (a ': as) where
  traverse p f (a :&> as) = (:&>) <$> f a <*> traverse p f as



foldrM
  :: (Fold p as, Monad m)
  => Proxy p
  -> (forall a. p a => f a -> b -> m b)
  -> b
  -> FHList f as
  -> m b
foldrM p f z0 xs = foldl p (\k x z -> f x z >>= k) return xs z0

foldlM
  :: (Fold p as, Monad m)
  => Proxy p
  -> (forall a. p a => b -> f a -> m b)
  -> b
  -> FHList f as
  -> m b
foldlM p f z0 xs = foldr p (\x k z -> f z x >>= k) return xs z0

traverse_
  :: (Fold p as, Applicative m)
  => Proxy p
  -> (forall a. p a => f a -> m ())
  -> FHList f as
  -> m ()
traverse_ p f = foldr p (\x -> (f x *>)) $ pure ()

for_
  :: (Fold p as, Applicative m)
  => Proxy p
  -> FHList f as
  -> (forall a. p a => f a -> m ())
  -> m ()
for_ p f d = traverse_ p d f



empty :: FHList f '[]
empty = FHZero



singleton :: f a -> FHList f '[a]
singleton = (:&> FHZero)



null :: FHList f as -> Bool
null FHZero = True
null _      = False



-- | Length of the list is known at compilation time.
length :: KnownNat (Length1 [] as) => FHList f as -> Integer
length (_ :: FHList f as) = natVal (Proxy :: Proxy (Length1 [] as))



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
