# generic-constructors

I've gotten used to the idea that adding a constructor to a type should break
pattern matches on a value of that type - an incomplete pattern match error.
However, there isn't something like this for the opposite scenario -
constructing values of that type.

For example, given this code, where `genFoo` potentially lives in a different
module than `Foo`:

```haskell
module MyData

data Foo = Bar Bool Double | Baz Int

...

module MyData.Gen

import           Hedgehog       (Gen)
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

genFoo :: Gen Foo
genFoo = Gen.choice @Gen
  [ Bar <$> Gen.bool_ <*> Gen.double (Range.linearFrac 0.0 1.0)
  , Baz <$> Gen.int (Range.linear (-100) 100)
  ]
```

Consider that a new constructor `Qux` is added to `Foo`. Strictly speaking,
there really shouldn't be an error anywhere, but there's an implicit assumption
that `genFoo` should generate *all* sorts of `Foo`s; in particular, it should
generate `Qux` values. In a large codebase, it's surprisingly easy to forget
a change like this, and code depending on `genFoo` will never see a `Qux`,
so the tests will miss a lot of branches.

If, instead, we had used `constructors` to define `genFoo`:

```haskell
module MyData

data Foo = Bar Bool Double | Baz Int
  deriving (Generic)

...

module MyData.Gen

import Constructors (constructors)

import           Hedgehog       (Gen)
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

genFoo :: Gen Foo
genFoo = constructors @Foo \bar baz ->
  Gen.choice @Gen
    [ bar <$> Gen.bool_ <*> Gen.double (Range.linearFrac 0.0 1.0)
    , baz <$> Gen.int (Range.linear (-100) 100)
    ]
```

when we decide to add `Qux`:

```diff
- data Foo = Bar Bool Double | Baz Int
+ data Foo = Bar Bool Double | Baz Int | Qux Char
```

the type of `constructors @Foo` will have changed, so the compiler will
remind us to add an entry for `Qux` to `genFoo`.

## Tagged

It's very likely that when reading the above you thought "what if I
accidentally transpose two constructors?" After all, the argument to
`constructors` can call the constructors by any name.
`Constructors.Tagged` offers a similar interface to `Constructors`, but
unlike the plain functions provided by `Constructors.constructors`,
`Constructors.Tagged.constructors` tags each of the functions with the name
of the constructor. Continuing with the previous example, we can write:

```haskell
module MyData.Gen

import qualified Constructors.Tagged as Tagged (constructors)
import           Data.Tagged         (untag)

import           Hedgehog       (Gen)
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

genFoo :: Gen Foo
genFoo = Tagged.constructors @Foo
  \(untag @"Bar" -> bar) ->
  \(untag @"Baz" -> baz) ->
    Gen.choice @Gen
      [ bar <$> Gen.bool_ <*> Gen.double (Range.linearFrac 0.0 1.0)
      , baz <$> Gen.int (Range.linear (-100) 100)
      ]
```

This way, when the eventual addition of `Qux` comes along,

```diff
- data Foo = Bar Bool Double | Baz Int
+ data Foo = Bar Bool Double | Baz Int | Qux Char
```

the compiler will give a better error message:

```
<loc>: error:
    • Couldn't match type ‘Gen Foo’
                     with ‘Tagged "Qux" (Char -> Foo)
                           -> Gen Foo’
      Expected type: Constructors.Tagged.Q (Rep Foo) Foo (Gen Foo)
        Actual type: Tagged "Bar" (Bool -> Double -> Foo)
                     -> Tagged "Baz" (Int -> Foo)
                     -> Gen Foo
...
```

So it will be clear that the `Qux` constructor is what's missing.

# Notes

I had to guide the compiler a bit with the type application on `Gen.choice`;
otherwise the error message wouldn't be quite as clear due to its overloading.
