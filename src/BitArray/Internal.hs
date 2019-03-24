{-# language LambdaCase #-}

module BitArray.Internal
  ( and,or,xor
  , b2c,c2b
  , flipBit'
  , true,false
  ) where

import Foreign.C.Types (CBool(..))
import Prelude hiding (and,or,map)
import qualified Data.Bits as Bits

flipBit' :: CBool -> CBool
flipBit' = \case { CBool 0 -> true; CBool _ -> false }
{-# inline flipBit' #-}

and :: CBool -> CBool -> CBool
and = (Bits..&.)
{-# inline and #-}

or :: CBool -> CBool -> CBool
or = (Bits..|.)
{-# inline or #-}

xor :: CBool -> CBool -> CBool
xor = Bits.xor
{-# inline xor #-}

b2c :: Bool -> CBool
b2c = \case { True -> true; False -> false }
{-# inline b2c #-}

c2b :: CBool -> Bool
c2b (CBool x) = if x == 0 then False else True
{-# inline c2b #-}

true,false :: CBool
true = CBool 1
{-# inline true #-}
false = CBool 0
{-# inline false #-}
