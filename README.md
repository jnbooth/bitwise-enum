# bitwise-enum

Efficient sets over bounded enumerations, using bitwise operations based on [containers](https://hackage.haskell.org/package/containers-0.6.0.1/docs/src/Data.IntSet.Internal.html) and [EdisonCore](https://hackage.haskell.org/package/EdisonCore-1.3.2.1/docs/src/Data-Edison-Coll-EnumSet.html). All operations apart from folds are constant-time. In many cases, the compiler may use constant folding to optimize `EnumSet`s away entirely. For example, in the following code:

```hs
import Data.Enum.Set as E

data Foo = A | B | C | D | E | F | G | H deriving (Bounded, Enum, Eq, Ord)

instance E.AsEnumSet Foo

addFoos :: E.EnumSet Foo -> E.EnumSet Foo
addFoos = E.delete A . E.insert B

bar :: E.EnumSet Foo
bar = addFoos $ E.fromFoldable [A, C, E]

barHasA :: Bool
barHasA = E.member A bar
```

With  -O  or  -O2 ,  `bar`  will compile to  `GHC.Types.W# 22##`  and  `barHasA`  will compile to  `GHC.Types.False`.

By default, `Word`s are used as the representation. Other representations may be chosen in the class instance:

```hs
{-# LANGUAGE TypeFamilies #-}

import Data.Enum.Set as E
import Data.Word (Word64)

data Foo = A | B | C | D | E | F | G | H deriving (Bounded, Enum, Eq, Ord, Show)

instance E.AsEnumSet Foo where
    type EnumSetRep Foo = Word64
```
