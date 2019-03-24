module Main (main) where

import qualified BitArray

main :: IO ()
main = do
  let len = 1000
  arr <- BitArray.new len
  BitArray.bitset arr 0 (len - 1) False
