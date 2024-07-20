{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Bitwise
  ( broadcastBS,
    selectBS,
  )
where

{-
import Control.Category ((.))
import Control.Monad (unless)
import Data.Bits
  ( complement,
    unsafeShiftL,
    unsafeShiftR,
    xor,
    (.&.),
    (.|.),
  )
import Data.Bool (Bool, otherwise, (||))
import Data.Eq ((/=), (==))
import Data.Foldable (for_)
import Data.Function (($))
import Data.Int (Int)
import Data.Ord (min, (<), (<=), (>), (>=))
import Data.Word (Word8)
import Foreign.Marshal.Utils (copyBytes, fillBytes)
import Foreign.Ptr (castPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import GHC.Err (error)
import GHC.Num (Integer, abs, (*), (+), (-))
import GHC.Real (, quotRem, rem)
-}

import PlutusTx.Builtins
import PlutusTx.Prelude

-- Given a rank and a desired value, return the first index of that value, or -1
-- if none exists.
selectBS :: Integer -> Bool -> BuiltinByteString -> Integer
selectBS ix val bs
  | ix < 0 = traceError "Negative rank requested"
  | ix >= byteLen = traceError "Impossibly high rank requested"
  | otherwise = go 0 0
  where
    byteLen :: Integer
    byteLen = lengthOfByteString bs * 8
    go :: Integer -> Integer -> Integer
    go currIx seen
      | currIx == byteLen = -1 -- We found nothing suitable
      | otherwise =
          let bitAt = readBit bs currIx
           in if bitAt == val
                then
                  if seen == ix
                    then currIx
                    else go (currIx + 1) (seen + 1)
                else go (currIx + 1) seen

-- Given a length and a byte, create a BuiltinByteString consisting of that many copies
-- of that byte.
broadcastBS :: Integer -> Integer -> BuiltinByteString
broadcastBS len w8
  | w8 < 0 || w8 > 255 = traceError "not a byte"
  | otherwise = replicateByte len w8
