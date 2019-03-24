{-# language BangPatterns #-}
{-# language DerivingStrategies #-}
{-# language GeneralizedNewtypeDeriving #-}

module BitArray.Mutable
  ( MutableBitArray(..)
  , bitset
  , flipBits
  , count
  , new
  , size
  , elementWise
  , map
  , elementWiseAnd
  , elementWiseOr
  , elementWiseXor
  , fromListN
  , fromList
  ) where

import BitArray.Internal
import Foreign.C.Types (CBool(..))
import Control.Monad.Primitive
import Data.Primitive.PrimArray
import Data.Primitive.UnliftedArray
import Prelude hiding (and,or,map)

-- | The number of bits in the mutable bit array.
size :: MutableBitArray s -> Int
size (MutableBitArray arr) = sizeofMutablePrimArray arr
{-# inline size #-}

map' :: PrimMonad m => (CBool -> CBool) -> MutableBitArray (PrimState m) -> m ()
map' f (MutableBitArray arr) = do
  let go !ix = if ix < sz
        then do
          v <- readPrimArray arr ix
          writePrimArray arr ix (f v)
          go (ix + 1)
        else pure ()
  go 0
  where
    !sz = sizeofMutablePrimArray arr
{-# inline map' #-}

--read :: PrimMonad m => MutableBitArray (PrimState m) -> Int -> m Bool
--read = do

-- | Map a bit function over a mutable bit array.
map :: PrimMonad m => (Bool -> Bool) -> MutableBitArray (PrimState m) -> m ()
map f = map' (\x -> b2c (f (c2b x)))
{-# inline map #-}

-- | Given the length of the list of bits, create a mutable bit array.
--
--   This is more efficient than 'fromList' because it does not need to traverse the
--   list to get its length.
--
-- /Note/: calls 'error' if the length given is incorrect.
fromListN :: PrimMonad m => Int -> [Bool] -> m (MutableBitArray (PrimState m))
fromListN len vs = do
  marr <- newPrimArray len
  let go [] !ix = if ix == len
        then pure ()
        else error "MutableBitArray.fromListN: list length less than specified size"
      go (a : as) !ix = if ix < len
        then do
          writePrimArray marr ix (b2c a)
          go as (ix + 1)
        else error "MutableBitArray.fromListN: list length greater than specified size"
  go vs 0
  pure (MutableBitArray marr)
{-# inlineable fromListN #-}

-- | Turn a list of bits into a mutable bit array.
fromList :: PrimMonad m => [Bool] -> m (MutableBitArray (PrimState m))
fromList xs = fromListN (length xs) xs
{-# inlineable fromList #-}

newtype MutableBitArray s = MutableBitArray (MutablePrimArray s CBool)
  deriving newtype (PrimUnlifted)

-- | Create a new mutable bit array of the given size.
new :: (PrimMonad m)
  => Int
  -> m (MutableBitArray (PrimState m))
new !sz = do
  marr <- newPrimArray sz
  pure (MutableBitArray marr)
{-# inline new #-}

-- | Set a range of bits to the same value.
bitset :: (PrimMonad m)
  => MutableBitArray (PrimState m) -- ^ bit array
  -> Int -- ^ starting index
  -> Int -- ^ ending index
  -> Bool -- ^ value to set
  -> m ()
bitset (MutableBitArray arr) !startIx !endIx b = setPrimArray arr startIx (endIx - startIx + 1) (b2c b)
{-# inline bitset #-}

-- | Count the number of ones in the bit array.
count :: (PrimMonad m)
  => MutableBitArray (PrimState m)
  -> m Int
count (MutableBitArray arr) = do
  let go !ix !acc = if ix < sz
        then do
          v <- readPrimArray arr ix
          if v == true
            then go (ix + 1) (acc + 1)
            else go (ix + 1) acc
        else pure acc
  go 0 0
  where
    !sz = sizeofMutablePrimArray arr

-- | Flip every bit in the array.
flipBits :: (PrimMonad m)
  => MutableBitArray (PrimState m)
  -> m ()
flipBits = map' flipBit'
{-# inline flipBits #-}

-- | Given a bitwise operation and two mutable bit arrays, produce a mutable bit array
--   containing the result of this operation to the both of them.
elementWise :: (PrimMonad m)
  => (Bool -> Bool -> Bool)
  -> MutableBitArray (PrimState m)
  -> MutableBitArray (PrimState m)
  -> m (MutableBitArray (PrimState m))
elementWise f b1 b2 = elementWise' (\x y -> b2c (f (c2b x) (c2b y))) b1 b2 
{-# inlineable elementWise #-}

-- | Given a bitwise operation and two mutable bit arrays, produce a mutable bit array
--   containing the result of this operation to the both of them.
elementWise' :: (PrimMonad m)
  => (CBool -> CBool -> CBool)
  -> MutableBitArray (PrimState m)
  -> MutableBitArray (PrimState m)
  -> m (MutableBitArray (PrimState m))
elementWise' f (MutableBitArray arr1) (MutableBitArray arr2) = do
  marr <- newPrimArray sz
  let go !ix = if ix < sz
        then do
          v1 <- readPrimArray arr1 ix
          v2 <- readPrimArray arr2 ix
          writePrimArray marr ix (f v1 v2)
          go (ix + 1) 
        else pure ()
  go 0
  pure (MutableBitArray marr)
  where
    !sz = min sz1 sz2
    !sz1 = sizeofMutablePrimArray arr1
    !sz2 = sizeofMutablePrimArray arr2
{-# inlineable elementWise' #-}

elementWiseXor :: PrimMonad m => MutableBitArray (PrimState m) -> MutableBitArray (PrimState m) -> m (MutableBitArray (PrimState m))
elementWiseXor = elementWise' xor

elementWiseAnd :: PrimMonad m => MutableBitArray (PrimState m) -> MutableBitArray (PrimState m) -> m (MutableBitArray (PrimState m))
elementWiseAnd = elementWise' and

elementWiseOr :: PrimMonad m => MutableBitArray (PrimState m) -> MutableBitArray (PrimState m) -> m (MutableBitArray (PrimState m))
elementWiseOr = elementWise' or
