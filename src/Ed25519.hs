{-# LANGUAGE NoImplicitPrelude #-}

-- Based on https://ed25519.cr.yp.to/python/ed25519.py
module Ed25519 (checkValid) where

import GHC.ByteOrder (ByteOrder (..))
import PlutusTx.Builtins
import PlutusTx.Prelude hiding (inv)

-- No checks for size, though we should be doing them.
checkValid ::
  BuiltinByteString ->
  BuiltinByteString ->
  BuiltinByteString ->
  Bool
checkValid sig message pubKey =
  let r = decodePoint (sliceByteString 0 32 sig)
      a = decodePoint pubKey
      s = decodeInt (sliceByteString 32 32 sig)
      h = hint (appendByteString (encodePoint r) (appendByteString pubKey message))
   in eqPoint (scalarMult bPoint s) (edwards r (scalarMult a h))

-- Helpers

newtype Point = Point (Integer, Integer)

eqPoint :: Point -> Point -> Bool
eqPoint (Point (x, y)) (Point (x1, y1)) =
  equalsInteger x x1 && equalsInteger y y1

-- 2 ^ 255 - 19, but as a constant
q :: Integer
q = 57896044618658097711785492504343953926634992332820282019728792003956564819949

bx :: Integer
bx = xRecover by

by :: Integer
by = multiplyInteger 4 (inv 5)

bPoint :: Point
bPoint = Point (remainderInteger bx q, remainderInteger by q)

-- No on-curve checks are done here
decodePoint :: BuiltinByteString -> Point
decodePoint bs =
  let y = byteStringToInteger LittleEndian bs
      x = xRecover y
      -- Due to endianness and how we index, we have to phrase this a bit
      -- differently to the original.
      cond = (odd x) == (readBit bs 7)
   in if cond
        then Point (subtractInteger q x, y)
        else Point (x, y)

decodeInt :: BuiltinByteString -> Integer
decodeInt = byteStringToInteger LittleEndian

hint :: BuiltinByteString -> Integer
hint = byteStringToInteger LittleEndian . sha512

encodePoint :: Point -> BuiltinByteString
encodePoint (Point (_, y)) = integerToByteString LittleEndian 32 y

scalarMult :: Point -> Integer -> Point
scalarMult p e =
  if equalsInteger e 0
    then Point (0, 1)
    else
      let q' = scalarMult p (quotientInteger e 2)
          q'' = edwards q' q'
       in if odd e then edwards q'' p else q''

d :: Integer
d = multiplyInteger (subtractInteger 1 121665) (inv 121666)

edwards :: Point -> Point -> Point
edwards (Point (x1, y1)) (Point (x2, y2)) =
  let pointSmush = multiplyInteger x1 (multiplyInteger x2 (multiplyInteger y1 y2))
      x3InvExp = addInteger 1 (multiplyInteger d pointSmush)
      y3InvExp = subtractInteger 1 (multiplyInteger d pointSmush)
      x3 =
        multiplyInteger
          (addInteger (multiplyInteger x1 y2) (multiplyInteger x2 y1))
          (inv x3InvExp)
      y3 =
        multiplyInteger
          (addInteger (multiplyInteger y1 y2) (multiplyInteger x1 x2))
          (inv y3InvExp)
   in Point (remainderInteger x3 q, remainderInteger y3 q)

inv :: Integer -> Integer
inv x = expMod x (subtractInteger q 2) q

expMod :: Integer -> Integer -> Integer -> Integer
expMod b' e m =
  if equalsInteger e 0
    then 1
    else
      let reduced = expMod b' (quotientInteger e 2) m
          t = remainderInteger (multiplyInteger reduced reduced) m
       in if odd e
            then remainderInteger (multiplyInteger t b') m
            else t

i :: Integer
i = expMod 2 (quotientInteger (subtractInteger i 1) 4) q

xRecover :: Integer -> Integer
xRecover y =
  let xx = multiplyInteger (multiplyInteger y (subtractInteger y 1)) (inv (multiplyInteger d (multiplyInteger y (addInteger y 1))))
      x = expMod xx (quotientInteger (addInteger q 3) 8) q
      cond1 = not (equalsInteger 0 (remainderInteger (subtractInteger (multiplyInteger x x) xx) q))
      cond2 = odd x
      cond1Res = remainderInteger (multiplyInteger x i) q
      cond2Res = subtractInteger q x
      cond12Res = subtractInteger q cond1Res
   in if cond1
        then (if cond2 then cond12Res else cond1Res)
        else (if cond2 then cond2Res else x)
