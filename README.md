# fhetero

Dirty ports of containers'
[lazy Maps](https://hackage.haskell.org/package/containers/docs/Data-Map-Lazy.html)
and good old Haskell [lists](https://hackage.haskell.org/package/base/docs/Data-List.html)
into the realm of type-level data structures.

Type-level lists are represented as `FHList (f :: k -> *) (as :: [k])`, where each __type__
(or kind) inside a type-level list is wrapped into `f`;

Type-level maps are represented as `FHMap (f :: k -> *) (as :: M k)`, where each __type__
(or kind) inside a type-level map is wrapped into `f`;

Implementation-wise everything is just a straight up copy from modules linked above,
except elevated into type classes with functional dependencies. No inlining or RULES ported
and a lot of code in `Data.FHMap.Internal` looks like absolute sin.

Concerning the GHC versions: this can definitely be backwired to support GHC 8.4,
but that requires adding `TypeInType` pragma and `Data.Kind` import which also conflicts
with the `Nat` multiplication operator `(*)` from `GHC.TypeLits`. If you need it, extend it.
