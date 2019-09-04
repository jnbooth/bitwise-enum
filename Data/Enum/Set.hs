-- | Efficient sets over bounded enumerations, using bitwise operations
-- based on [EdisonCore](https://hackage.haskell.org/package/EdisonCore-1.3.2.1/docs/src/Data-Edison-Coll-EnumSet.html) and [containers](https://hackage.haskell.org/package/containers-0.6.0.1/docs/src/Data.IntSet.Internal.html).
-- For type @EnumSet W A@, @W@ should be a 'Word'-like type that implements
-- 'Bits' and 'Num', and @A@ should be a type that implements 'Eq' and 'Enum'
-- equivalently and is a bijection to 'Int'.
--
-- @EnumSet W A@ will not store any @A@ with a 'fromEnum' less than 0 or greater
-- than the number of bits in @W@, so it is best for @A@ to be a type that
-- derives 'Eq' and 'Enum' and for @W@ to have at least as many bits as the
-- number of constructors of @A@.
--
-- Depending on the number of constructors, one of 'Data.Word.Word8',
-- 'Data.Word.Word16', 'Data.Word.Word32', or 'Data.Word.Word64' is a good
-- default representation type. For enumerations with many constructors, there
-- is also [Word128](https://hackage.haskell.org/package/wide-word-0.1.0.8/docs/Data-WideWord-Word128.html)
-- from the [wide-word](https://hackage.haskell.org/package/wide-word-0.1.0.8/)
-- package.
--
-- "Data.Enum.Set.Class" provides an alternate type alias that moves the
-- underlying representation to an associated type token,
-- so that e.g. @EnumSet Word16 MyEnum@ may be replaced with @EnumSet MyEnum@,
-- and reexports this module with adjusted type signatures.
module Data.Enum.Set
  ( -- * Set type
    EnumSet

    -- * Construction
  , empty
  , singleton
  , fromFoldable

  -- * Insertion
  , insert

  -- * Deletion
  , delete

  -- * Query
  , member
  , notMember
  , null
  , size
  , isSubsetOf

  -- * Combine
  , union
  , difference
  , (\\)
  , symmetricDifference
  , intersection

  -- * Filter
  , filter
  , partition

  -- * Map
  , map

  -- * Folds
  , foldl, foldl', foldr, foldr'
  , foldl1, foldl1', foldr1, foldr1'

  -- ** Special folds
  , foldMap
  , traverse
  , any
  , all

   -- * Min/Max
  , minimum
  , maximum
  , deleteMin
  , deleteMax
  , minView
  , maxView

  -- * Conversion
  , toList
  ) where

import qualified GHC.Exts
import qualified Data.Foldable as F

import Prelude hiding (all, any, filter, foldl, foldl1, foldMap, foldr, foldr1, map, maximum, minimum, null, traverse)

import Control.Applicative (liftA2)
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON(..))
import Data.Bits
import Data.Data (Data)
import Data.Monoid (Monoid(..))
import Foreign.Storable (Storable)
import GHC.Exts (IsList(Item), build)
import Text.Read

import qualified Data.Containers
import           Data.Containers (SetContainer, IsSet)
import qualified Data.MonoTraversable
import           Data.MonoTraversable (Element, GrowingAppend, MonoFoldable, MonoFunctor, MonoPointed, MonoTraversable)

{--------------------------------------------------------------------
  Set type
--------------------------------------------------------------------}

-- | A set of values @a@ with representation @word@,
-- implemented as bitwise operations.
newtype EnumSet word a = EnumSet word deriving (Eq, Ord, Data, Storable, NFData)

instance Bits w => Semigroup (EnumSet w a) where
    (<>) = union
    {-# INLINE (<>) #-}

instance Bits w => Monoid (EnumSet w a) where
    mempty = empty
    {-# INLINE mempty #-}

instance (Bits w, Enum a) => MonoPointed (EnumSet w a) where
    opoint = singleton
    {-# INLINE opoint #-}

instance (FiniteBits w, Num w, Enum a) => IsList (EnumSet w a) where
    type Item (EnumSet w a) = a
    fromList = fromFoldable
    {-# INLINE fromList #-}
    toList = toList
    {-# INLINE toList #-}

instance (FiniteBits w, Num w, Enum a, ToJSON a) => ToJSON (EnumSet w a) where
    toJSON = toJSON . toList
    {-# INLINE toJSON #-}
    toEncoding = toEncoding . toList
    {-# INLINE toEncoding #-}

type instance Element (EnumSet w a) = a

instance (FiniteBits w, Num w, Enum a) => MonoFunctor (EnumSet w a) where
    omap = map
    {-# INLINE omap #-}

instance (FiniteBits w, Num w, Enum a) => MonoFoldable (EnumSet w a) where
    ofoldMap = foldMap
    {-# INLINE ofoldMap #-}
    ofoldr = foldr
    {-# INLINE ofoldr #-}
    ofoldl' = foldl'
    {-# INLINE ofoldl' #-}
    ofoldr1Ex = foldr1
    {-# INLINE ofoldr1Ex #-}
    ofoldl1Ex' = foldl1'
    {-# INLINE ofoldl1Ex' #-}
    otoList = toList
    {-# INLINE otoList #-}
    oall = all
    {-# INLINE oall #-}
    oany = any
    {-# INLINE oany #-}
    onull = null
    {-# INLINE onull #-}
    olength64 (EnumSet !w) = bitcount 0 w
    {-# INLINE olength64 #-}
    headEx = minimum
    {-# INLINE headEx #-}
    lastEx = maximum
    {-# INLINE lastEx #-}
    oelem = member
    {-# INLINE oelem #-}
    onotElem x = not . member x
    {-# INLINE onotElem #-}

instance (FiniteBits w, Num w, Enum a) => GrowingAppend (EnumSet w a)

instance (FiniteBits w, Num w, Enum a) => MonoTraversable (EnumSet w a) where
    otraverse = traverse
    {-# INLINE otraverse #-}

instance (FiniteBits w, Num w, Eq a, Enum a) => SetContainer (EnumSet w a) where
    type ContainerKey (EnumSet w a) = a
    member = member
    {-# INLINE member #-}
    notMember = notMember
    {-# INLINE notMember #-}
    union = union
    {-# INLINE union #-}
    difference = difference
    {-# INLINE difference #-}
    intersection = intersection
    {-# INLINE intersection #-}
    keys = toList
    {-# INLINE keys #-}

instance (FiniteBits w, Num w, Eq a, Enum a) => IsSet (EnumSet w a) where
    insertSet = insert
    {-# INLINE insertSet #-}
    deleteSet = delete
    {-# INLINE deleteSet #-}
    singletonSet = singleton
    {-# INLINE singletonSet #-}
    setFromList = fromFoldable
    {-# INLINE setFromList #-}
    setToList = toList
    {-# INLINE setToList #-}

instance (FiniteBits w, Num w, Enum x, Show x) => Show (EnumSet w x) where
    showsPrec p xs = showParen (p > 10) $
        showString "fromList " . shows (toList xs)

instance (Bits w, Num w, Enum x, Read x) => Read (EnumSet w x) where
    readPrec = parens $ prec 10 do
        Ident "fromList" <- lexP
        xs :: [x] <- readPrec
        return $ fromFoldable xs
    readListPrec = readListPrecDefault

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}

-- | /O(1)/. The empty set.
empty :: ∀ w a. Bits w
      => EnumSet w a
empty = EnumSet zeroBits
{-# INLINE empty #-}

-- | /O(1)/. A set of one element.
singleton :: ∀ w a. (Bits w, Enum a)
          => a -> EnumSet w a
singleton = EnumSet . bit . fromEnum
{-# INLINE singleton #-}

-- | /O(n)/. Create a set from a finite foldable data structure.
fromFoldable :: ∀ f w a. (Foldable f, Bits w, Enum a)
             => f a -> EnumSet w a
fromFoldable = EnumSet . F.foldl' (flip $ (.|.) . bit . fromEnum) zeroBits

{--------------------------------------------------------------------
  Insertion
--------------------------------------------------------------------}

-- | /O(1)/. Add a value to the set.
insert :: ∀ w a. (Bits w, Enum a)
       => a -> EnumSet w a -> EnumSet w a
insert !x (EnumSet w) = EnumSet . setBit w $ fromEnum x

{--------------------------------------------------------------------
  Deletion
--------------------------------------------------------------------}

-- | /O(1)/. Delete a value in the set.
delete :: ∀ w a. (Bits w, Enum a)
       => a -> EnumSet w a -> EnumSet w a
delete !x (EnumSet w) = EnumSet . clearBit w $ fromEnum x

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}

-- | /O(1)/. Is the value a member of the set?
member :: ∀ w a. (Bits w, Enum a)
       => a -> EnumSet w a -> Bool
member !x (EnumSet w) = testBit w $ fromEnum x

-- | /O(1)/. Is the value not in the set?
notMember :: ∀ w a. (Bits w, Enum a)
          => a -> EnumSet w a -> Bool
notMember !x = not . member x

-- | /O(1)/. Is this the empty set?
null :: ∀ w a. Bits w
     => EnumSet w a -> Bool
null (EnumSet w) = zeroBits == w
{-# INLINE null #-}

-- | /O(1)/. The number of elements in the set.
size :: ∀ w a. (Bits w, Num w)
     => EnumSet w a -> Int
size (EnumSet !w) = bitcount 0 w

-- | /O(1)/. Is this a subset?
-- @(s1 `isSubsetOf` s2)@ tells whether @s1@ is a subset of @s2@.
isSubsetOf :: ∀ w a. (Bits w)
           => EnumSet w a -> EnumSet w a -> Bool
isSubsetOf (EnumSet x) (EnumSet y) = x .|. y == y
{-# INLINE isSubsetOf #-}

{--------------------------------------------------------------------
  Combine
--------------------------------------------------------------------}

-- | /O(1)/. The union of two sets.
union :: ∀ w a. Bits w
      => EnumSet w a -> EnumSet w a -> EnumSet w a
union (EnumSet x) (EnumSet y) = EnumSet $ x .|. y
{-# INLINE union #-}

-- | /O(1)/. Difference between two sets.
difference :: ∀ w a. Bits w
           => EnumSet w a -> EnumSet w a -> EnumSet w a
difference (EnumSet x) (EnumSet y) = EnumSet $ (x .|. y) `xor` y
{-# INLINE difference #-}

-- | /O(1)/. See 'difference'.
(\\) :: ∀ w a. Bits w
     => EnumSet w a -> EnumSet w a -> EnumSet w a
(\\) = difference
infixl 9 \\
{-# INLINE (\\) #-}

-- | /O(1)/. Elements which are in either set, but not both.
symmetricDifference :: ∀ w a. Bits w
                    => EnumSet w a -> EnumSet w a -> EnumSet w a
symmetricDifference (EnumSet x) (EnumSet y) = EnumSet $ x `xor` y
{-# INLINE symmetricDifference #-}

-- | /O(1)/. The intersection of two sets.
intersection :: ∀ w a. Bits w
             => EnumSet w a -> EnumSet w a -> EnumSet w a
intersection (EnumSet x) (EnumSet y) = EnumSet $ x .&. y
{-# INLINE intersection #-}

{--------------------------------------------------------------------
  Filter
--------------------------------------------------------------------}

-- | /O(n)/. Filter all elements that satisfy some predicate.
filter :: ∀ w a. (FiniteBits w, Num w, Enum a)
       => (a -> Bool) -> EnumSet w a -> EnumSet w a
filter p (EnumSet w) = EnumSet $ foldlBits' f 0 w
    where
      f z i
        | p $ toEnum i = setBit z i
        | otherwise    = z
      {-# INLINE f #-}

-- | /O(n)/. Partition the set according to some predicate.
-- The first set contains all elements that satisfy the predicate,
-- the second all elements that fail the predicate.
partition :: ∀ w a. (FiniteBits w, Num w, Enum a)
          => (a -> Bool) -> EnumSet w a -> (EnumSet w a, EnumSet w a)
partition p (EnumSet w) = (EnumSet yay, EnumSet nay)
    where
      (yay, nay) = foldlBits' f (0, 0) w
      f (x, y) i
          | p $ toEnum i = (setBit x i, y)
          | otherwise    = (x, setBit y i)
      {-# INLINE f #-}

{--------------------------------------------------------------------
  Map
--------------------------------------------------------------------}

-- | /O(n)/.
-- @'map' f s@ is the set obtained by applying @f@ to each element of @s@.
--
-- It's worth noting that the size of the result may be smaller if,
-- for some @(x,y)@, @x \/= y && f x == f y@.
map :: ∀ w a b. (FiniteBits w, Num w, Enum a, Enum b)
    => (a -> b) -> EnumSet w a -> EnumSet w b
map f0 (EnumSet w) = EnumSet $ foldlBits' f 0 w
    where
      f z i = setBit z $ fromEnum $ f0 (toEnum i)
      {-# INLINE f #-}

{--------------------------------------------------------------------
  Folds
--------------------------------------------------------------------}

-- | /O(n)/. Left fold.
foldl :: ∀ w a b. (FiniteBits w, Num w, Enum a)
      => (b -> a -> b) -> b -> EnumSet w a -> b
foldl f z (EnumSet w) = foldlBits ((. toEnum) . f) z w
{-# INLINE foldl #-}

-- | /O(n)/. Left fold with strict accumulator.
foldl' :: ∀ w a b. (FiniteBits w, Num w, Enum a)
       => (b -> a -> b) -> b -> EnumSet w a -> b
foldl' f z (EnumSet w) = foldlBits' ((. toEnum) . f) z w
{-# INLINE foldl' #-}

-- | /O(n)/. Right fold.
foldr :: ∀ w a b. (FiniteBits w, Num w, Enum a)
      => (a -> b -> b) -> b -> EnumSet w a -> b
foldr f z (EnumSet w) = foldrBits (f . toEnum) z w
{-# INLINE foldr #-}

-- | /O(n)/. Right fold with strict accumulator.
foldr' :: ∀ w a b. (FiniteBits w, Num w,  Enum a)
       => (a -> b -> b) -> b -> EnumSet w a -> b
foldr' f z (EnumSet w) = foldrBits' (f . toEnum) z w
{-# INLINE foldr' #-}

-- | /O(n)/. Left fold on non-empty sets.
foldl1 :: ∀ w a. (FiniteBits w, Num w, Enum a)
       => (a -> a -> a) -> EnumSet w a -> a
foldl1 f = fold1Aux lsb $ foldlBits ((. toEnum) . f)
{-# INLINE foldl1 #-}

-- | /O(n)/. Left fold on non-empty sets with strict accumulator.
foldl1' :: ∀ w a. (FiniteBits w, Num w, Enum a)
        => (a -> a -> a) -> EnumSet w a -> a
foldl1' f = fold1Aux lsb $ foldlBits' ((.toEnum) . f)
{-# INLINE foldl1' #-}

-- | /O(n)/. Right fold on non-empty sets.
foldr1 :: ∀ w a. (FiniteBits w, Num w, Enum a)
       => (a -> a -> a) -> EnumSet w a -> a
foldr1 f = fold1Aux msb $ foldrBits (f . toEnum)
{-# INLINE foldr1 #-}

-- | /O(n)/. Right fold on non-empty sets with strict accumulator.
foldr1' :: ∀ w a. (FiniteBits w, Num w, Enum a)
        => (a -> a -> a) -> EnumSet w a -> a
foldr1' f = fold1Aux msb $ foldrBits' (f . toEnum)
{-# INLINE foldr1' #-}

-- | /O(n)/. Map each element of the structure to a monoid, and combine the
-- results.
foldMap :: ∀ m w a. (Monoid m, FiniteBits w, Num w, Enum a)
        => (a -> m) -> EnumSet w a -> m
foldMap f (EnumSet w) = foldrBits (mappend . f . toEnum) mempty w
{-# INLINE foldMap #-}

traverse :: ∀ f w a. (Applicative f, FiniteBits w, Num w, Enum a)
         => (a -> f a) -> EnumSet w a -> f (EnumSet w a)
traverse f (EnumSet w) = EnumSet <$>
                         foldrBits
                         (liftA2 (flip setBit) . fmap fromEnum . f . toEnum)
                         (pure zeroBits)
                         w
{-# INLINE traverse #-}

-- | /O(n)/. Check if all elements satisfy some predicate.
all :: ∀ w a. (FiniteBits w, Num w, Enum a)
    => (a -> Bool) -> EnumSet w a -> Bool
all p (EnumSet w) = let lb = lsb w in go lb (w `unsafeShiftR` lb)
  where
    go !_ 0 = True
    go bi n
      | n `testBit` 0 && not (p $ toEnum bi) = False
      | otherwise = go (bi + 1) (n `unsafeShiftR` 1)

-- | /O(n)/. Check if any element satisfies some predicate.
any :: ∀ w a. (FiniteBits w, Num w, Enum a)
    => (a -> Bool) -> EnumSet w a -> Bool
any p (EnumSet w) = let lb = lsb w in go lb (w `unsafeShiftR` lb)
  where
    go !_ 0 = False
    go bi n
      | n `testBit` 0 && p (toEnum bi) = True
      | otherwise = go (bi + 1) (n `unsafeShiftR` 1)

{--------------------------------------------------------------------
  Min/Max
--------------------------------------------------------------------}

-- | /O(1)/. The minimal element of a non-empty set.
minimum :: ∀ w a. (FiniteBits w, Num w, Enum a)
        => EnumSet w a -> a
minimum (EnumSet 0) = error "empty EnumSet"
minimum (EnumSet w) = toEnum $ lsb w

-- | /O(1)/. The maximal element of a non-empty set.
maximum :: ∀ w a. (FiniteBits w, Num w, Enum a)
        => EnumSet w a -> a
maximum (EnumSet 0) = error "empty EnumSet"
maximum (EnumSet w) = toEnum $ msb w

-- | /O(1)/. Delete the minimal element.
deleteMin :: ∀ w a. (FiniteBits w, Num w)
          => EnumSet w a -> EnumSet w a
deleteMin (EnumSet 0) = EnumSet 0
deleteMin (EnumSet w) = EnumSet $ clearBit w $ lsb w

-- | /O(1)/. Delete the maximal element.
deleteMax :: ∀ w a. (FiniteBits w, Num w)
          => EnumSet w a -> EnumSet w a
deleteMax (EnumSet 0) = EnumSet 0
deleteMax (EnumSet w) = EnumSet $ clearBit w $ msb w

-- | /O(1)/. Retrieves the minimal element of the set,
-- and the set stripped of that element,
-- or Nothing if passed an empty set.
minView :: ∀ w a. (FiniteBits w, Num w, Enum a)
        => EnumSet w a -> Maybe (a, EnumSet w a)
minView (EnumSet 0) = Nothing
minView (EnumSet w) = let i = lsb w in Just (toEnum i, EnumSet $ clearBit w i)

-- | /O(1)/. Retrieves the maximal element of the set,
-- and the set stripped of that element,
-- or Nothing if passed an empty set.
maxView :: ∀ w a. (FiniteBits w, Num w, Enum a)
        => EnumSet w a -> Maybe (a, EnumSet w a)
maxView (EnumSet 0) = Nothing
maxView (EnumSet w) = let i = msb w in Just (toEnum i, EnumSet $ clearBit w i)

{--------------------------------------------------------------------
  Conversion
--------------------------------------------------------------------}

-- | /O(n)/. Convert the set to a list of values.
toList :: ∀ w a. (FiniteBits w, Num w, Enum a)
       => EnumSet w a -> [a]
toList (EnumSet w) = build \c n -> foldrBits (c . toEnum) n w
{-# INLINE toList #-}

{--------------------------------------------------------------------
  Utility functions
--------------------------------------------------------------------}

bitcount :: ∀ i w. (Bits w, Num w, Num i) => i -> w -> i
bitcount a 0 = a
bitcount !a x = bitcount (a+1) (x .&. (x - 1))

lsb :: ∀ w. (FiniteBits w, Num w) => w -> Int
lsb n0 = go 0 n0 $ finiteBitSize n0 `quot` 2
  where
    go b n 1 = case n .&. 1 of
        0 -> 1 + b
        _ -> b
    go b n i = case n .&. (bit i - 1) of
        0 -> go (i + b) (n `unsafeShiftR` i) (i `quot` 2)
        _ -> go b       n                    (i `quot` 2)
{-# INLINE lsb #-}

msb :: ∀ w. (FiniteBits w, Num w) => w -> Int
msb n0 = go 0 n0 $ finiteBitSize n0 `quot` 2
  where
    go b n 1 = case n .&. 2 of
        0 -> b
        _ -> 1 + b
    go b n i = case n .&. (bit (i * 2) - bit i) of
        0 -> go b       n                    (i `quot` 2)
        _ -> go (i + b) (n `unsafeShiftR` i) (i `quot` 2)
{-# INLINE msb #-}

foldlBits :: ∀ w a. (FiniteBits w, Num w) => (a -> Int -> a) -> a -> w -> a
foldlBits  f z w = let lb = lsb w in go lb z (w `unsafeShiftR` lb)
  where
    go !_ acc 0 = acc
    go bi acc n
      | n `testBit` 0 = go (bi + 1) (f acc bi) (n `unsafeShiftR` 1)
      | otherwise     = go (bi + 1)    acc     (n `unsafeShiftR` 1)
{-# INLINE foldlBits #-}

foldlBits' :: ∀ w a. (FiniteBits w, Num w) => (a -> Int -> a) -> a -> w -> a
foldlBits' f z w = let lb = lsb w in go lb z (w `unsafeShiftR` lb)
  where
    go !_ !acc 0 = acc
    go bi acc n
      | n `testBit` 0 = go (bi + 1) (f acc bi) (n `unsafeShiftR` 1)
      | otherwise     = go (bi + 1)    acc     (n `unsafeShiftR` 1)
{-# INLINE foldlBits' #-}

foldrBits :: ∀ w a. (FiniteBits w, Num w) => (Int -> a -> a) -> a -> w -> a
foldrBits f z w = let lb = lsb w in go lb (w `unsafeShiftR` lb)
  where
    go !_ 0 = z
    go bi n
      | n `testBit` 0 = f bi (go (bi + 1) (n `unsafeShiftR` 1))
      | otherwise     =       go (bi + 1) (n `unsafeShiftR` 1)
{-# INLINE foldrBits #-}

foldrBits' :: ∀ w a. (FiniteBits w, Num w) => (Int -> a -> a) -> a -> w -> a
foldrBits' f z w = let lb = lsb w in go lb (w `unsafeShiftR` lb)
  where
    go !_ 0 = z
    go bi n
      | n `testBit` 0 = f bi $! go (bi + 1) (n `unsafeShiftR` 1)
      | otherwise     =         go (bi + 1) (n `unsafeShiftR` 1)
{-# INLINE foldrBits' #-}

fold1Aux :: ∀ w a. (Bits w, Num w, Enum a)
         => (w -> Int) -> (a -> w -> a) -> EnumSet w a -> a
fold1Aux _      _ (EnumSet 0) = error "empty EnumSet"
fold1Aux getBit f (EnumSet w) = f (toEnum gotBit) (clearBit w gotBit)
  where
    gotBit = getBit w
{-# INLINE fold1Aux #-}
