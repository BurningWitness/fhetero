# fhetero

Heterogenous data structures that wrap elements into @f@.

Currently offering:

  * `FHList (f :: k -> *) (as :: [k])`: dirty port of Haskell
    [lists](https://hackage.haskell.org/package/base/docs/Data-List.html);

  * `FHMap (f :: v -> *) (as :: M k)`: dirty port of containers'
    [lazy Maps](https://hackage.haskell.org/package/containers/docs/Data-Map-Lazy.html).
    `k` here can be any `TypeOrd`-comparable kind, not just `Symbol`;

  * `OneOf (f :: v -> *) (as :: M ())`: upgraded `Either` that works on any number
    of elements;

  * `FHDecision (f :: v -> *) (as :: Dec)`: non-monadic decision trees, supporting
    chaining through `(>>)`, data-level (through `OneOf`) and type-level choices.

Concerning the GHC versions: this can definitely be backwired to support GHC 8.4,
but that requires adding `TypeInType` pragma and `Data.Kind` import which also conflicts
with the `Nat` multiplication operator `(*)` from `GHC.TypeLits`.
If you need it, extend it.
