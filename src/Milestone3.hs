{-# LANGUAGE MultiWayIf #-}

module Milestone3 (nqueens) where

import PlutusTx.Builtins
  ( andByteString,
    findFirstSetBit,
    replicateByte,
    shiftByteString,
    writeBits,
  )
import PlutusTx.Prelude

-- Based on Qiu, Zongyan (February 2002). "Bit-vector encoding of n-queen problem". ACM SIGPLAN Notices. 37 (2): 68–70
-- For simplicity, this only accepts multiples of 8 for the dimension (so 8, 16,
-- 24, etc): in all other cases it will return an empty list. Results are (row,
-- column) pairs.
{-# INLINE nqueens #-}
nqueens :: Integer -> [(Integer, Integer)]
nqueens dim
  | dim < 8 = []
  | dim `remainder` 8 /= 0 = []
  | otherwise =
      let down = replicateByte bytesNeeded 0xFF
          left = replicateByte bytesNeeded 0xFF
          right = replicateByte bytesNeeded 0xFF
       in go 0 0 down left right
  where
    bytesNeeded :: Integer
    bytesNeeded = dim `quotient` 8
    lastRow :: Integer
    lastRow = dim - 1
    lastPosition :: Integer
    lastPosition = dim - 1
    go ::
      Integer ->
      Integer ->
      BuiltinByteString ->
      BuiltinByteString ->
      BuiltinByteString ->
      [(Integer, Integer)]
    go selectIx row down left right
      | selectIx == dim = []
      | otherwise =
          let opts = andByteString False down . andByteString False left $ right
              available = selectByteString selectIx opts
           in if
                | available == (-1) -> []
                | row == lastRow -> [(row, available)]
                | otherwise ->
                    let newDown = writeBit down available False
                        newLeft = writeBit (shiftByteString (writeBit left available False) 1) 0 True
                        newRight = writeBit (shiftByteString (writeBit right available False) (-1)) lastPosition True
                        newRow = row + 1
                     in case go 0 newRow newDown newLeft newRight of
                          [] -> go (selectIx + 1) row down left right
                          next -> (row, available) : next

-- Helpers

{-# INLINE selectByteString #-}
selectByteString :: Integer -> BuiltinByteString -> Integer
selectByteString which bs =
  let ffs = findFirstSetBit bs
   in if which <= 0
        then ffs
        else selectByteString (which - 1) (shiftByteString bs . negate $ ffs + 1)

{-# INLINE writeBit #-}
writeBit :: BuiltinByteString -> Integer -> Bool -> BuiltinByteString
writeBit bs i b = writeBits bs . toBuiltin @[(Integer, Bool)] $ [(i, b)]
