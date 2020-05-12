# generic-constructors

I've gotten used to the idea that adding a constructor to a type should break
pattern matches on a value of that type - an incomplete pattern match error.
However, there isn't something like this for the opposite scenario -
constructing values of that type.

For example, given this code, where `genFoo` potentially lives in a different
module than `Foo`:

```haskell
module MyData

import Hedgehog.Gen (Gen)

data Foo = Bar | Baz

...

module MyData.Gen

genFoo :: Gen Foo
genFoo = element [Bar, Baz]
```

Consider that a new constructor `Qux` is added to `Foo`. Strictly speaking,
there really shouldn't be an error anywhere, but there's an implicit assumption
that `genFoo` should generate *all* sorts of `Foo`s, in particular, it should
generate `Qux` values. In a large codebase, it's surprisingly easy to forget
a change like this, and code depending on `genFoo` will never see a `Qux`,
so the tests will miss a lot of branches.

If, instead, we had used `constructors` to define `genFoo`:

```haskell
module MyData

import Hedgehog.Gen (Gen)
import GHC.Generics

data Foo = Bar | Baz  
  deriving (Generic)

...

module MyData.Gen

genFoo :: Gen Foo
genFoo = constructors @Foo \bar baz ->
  element [bar, baz]
```

when we decide to add `Qux`:

```diff
- data Foo = Bar | Baz
+ data Foo = Bar | Baz | Qux
```

the type of `constructors @Foo` will have changed, so the compiler will
remind us to add an entry for `Qux` to `genFoo`.
