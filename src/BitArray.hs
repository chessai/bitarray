{-# language BangPatterns #-}
{-# language DerivingStrategies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language UndecidableInstances #-}

module BitArray
  ( BitArray(..)
  ) where

import qualified Data.Bits as Bits
import qualified BitArray.Mutable as BM
import BitArray.Mutable (MutableBitArray(..))
import BitArray.Internal
import Foreign.C.Types (CBool(..))
import Control.Monad.Primitive
import Data.Primitive.PrimArray
import Data.Primitive.UnliftedArray
import Prelude hiding (and,or)
import Control.Monad.ST

newtype BitArray = BitArray (PrimArray CBool)

size :: BitArray -> Int
size (BitArray arr) = sizeofPrimArray arr
{-# inline size #-}

index' :: BitArray -> Int -> CBool
index' (BitArray arr) x = indexPrimArray arr x
{-# inline index' #-}

index :: BitArray -> Int -> Bool
index b x = c2b (index' b x)
{-# inline index #-}

unsafeFreeze :: PrimMonad m => MutableBitArray (PrimState m) -> m BitArray
unsafeFreeze (MutableBitArray marr) = unsafeFreezePrimArray marr >>= \x -> pure (BitArray x)
{-# inline unsafeFreeze #-}

fromListN :: Int -> [Bool] -> BitArray
fromListN len vs = runST $ BM.fromListN len vs >>= unsafeFreeze
{-# inline fromListN #-}

fromList :: [Bool] -> BitArray
fromList vs = runST $ BM.fromList vs >>= unsafeFreeze
{-# inline fromList #-}

foldr :: (Bool -> b -> b) -> b -> BitArray -> b
foldr f z arr = go (size arr - 1)
  where
    go !ix = if ix > 0
      then f (index arr ix) (go (ix - 1))
      else z

foldr' :: (Bool -> b -> b) -> b -> BitArray -> b
foldr' f z0 arr = go (size arr - 1) z0
  where
    go !ix !acc = if ix < 0
      then acc
      else go (ix - 1) (f (index arr ix) acc)

foldl :: (b -> Bool -> b) -> b -> BitArray -> b
foldl f z arr = go 0
  where
    !sz = size arr
    go !ix = if ix < sz
      then f (go (ix + 1)) (index arr ix)
      else z

foldl' :: (b -> Bool -> b) -> b -> BitArray -> b
foldl' f z0 arr = go 0 z0
  where
    !sz = size arr
    go !ix !acc = if ix < sz
      then go (ix + 1) (f acc (index arr ix))
      else acc
  
