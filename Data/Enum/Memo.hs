{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE UnicodeSyntax       #-}

-- | Immutable lazy tables of functions over bounded enumerations.
-- Function calls are stored as thunks and not evaluated until accessed.
--
-- In order to be used as a key, an enumeration type should start at 0 and be
-- total over its range. In other words:
--
-- > [0 .. fromEnum maxBound] == map fromEnum [minBound .. maxBound]
--
-- __This property is not automatically checked.__
-- Derived instances of @Bounded@ and @Enum@ always obey this condition.
--
-- The underlying representation is a 'Vector' rather than a search tree.
-- This provides O(1) lookup but means that the range of keys should not be
-- very large, as in the case of an @Int@-like type.
module Data.Enum.Memo (memoize) where

import Prelude hiding (lookup)

import qualified Data.Vector as Vector

memoize :: ∀ k v. (Bounded k, Enum k) => (k -> v) -> k -> v
memoize f =
    case Vector.generate (1 + fromEnum @k maxBound) $ f . toEnum of
        memo -> Vector.unsafeIndex memo . fromEnum
