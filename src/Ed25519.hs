{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Based on https://ed25519.cr.yp.to/python/ed25519.py
module Ed25519 where

import Crypto.Sign.Ed25519 (PublicKey (..), Signature (..))
import Crypto.Sign.Ed25519 qualified as Reference
import Data.ByteString qualified as BS
import Data.Foldable (Foldable (foldl'))
import Debug.Trace (traceM)
import Debug.Trace qualified as Debug
import GHC.ByteOrder (ByteOrder (..))
import PlutusTx.Builtins
import PlutusTx.Builtins.Internal (BuiltinByteString (..))
import PlutusTx.Prelude hiding (inv)
import SHA512V2 (sha512)
import Prelude qualified as P

-- No checks for size, though we should be doing them.
checkValid ::
  BuiltinByteString ->
  BuiltinByteString ->
  BuiltinByteString ->
  Bool
checkValid sig message pubKey =
  let !r = decodePoint (sliceByteString 0 32 sig)
      !a = decodePoint pubKey
      !s = decodeInt (sliceByteString 32 32 sig)
      !h = hint (appendByteString (encodePoint r) (appendByteString pubKey message))
   in eqPoint (scalarMult bPoint s) (edwards r (scalarMult a h))

-- Helpers

newtype Point = Point (Integer, Integer) deriving stock (P.Show)

eqPoint :: Point -> Point -> Bool
eqPoint p1@(Point (x, y)) p2@(Point (x1, y1)) =
  Debug.trace msg
    $ equalsInteger x x1
    && equalsInteger y y1
  where
    msg = "eqPoint:\n\np1:\n" <> P.show p1 <> "\n\np2:\n" <> P.show p2

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
  Debug.trace "decodePoint"
    $ let y = byteStringToInteger LittleEndian bs
          x = xRecover y
          -- Due to endianness and how we index, we have to phrase this a bit
          -- differently to the original.
          cond = odd x == readBit bs 7
       in if cond
            then Point (subtractInteger q x, y)
            else Point (x, y)

decodeInt :: BuiltinByteString -> Integer
decodeInt bibs = Debug.trace ("decodeInt:\n" <> P.show bibs) $ byteStringToInteger LittleEndian bibs

hint :: BuiltinByteString -> Integer
hint bibs = Debug.trace ("hint:\n" <> P.show bibs) $ byteStringToInteger LittleEndian . sha512 $ bibs

encodePoint :: Point -> BuiltinByteString
encodePoint (Point (_, y)) = Debug.trace "encodePoint" $ integerToByteString LittleEndian 32 y

scalarMult :: Point -> Integer -> Point
scalarMult p e =
  Debug.trace "scalarMult"
    $ if equalsInteger e 0
      then Point (0, 1)
      else
        let q' = scalarMult p (quotientInteger e 2)
            q'' = edwards q' q'
         in if odd e then edwards q'' p else q''

d :: Integer
d = multiplyInteger (subtractInteger 1 121665) (inv 121666)

edwards :: Point -> Point -> Point
edwards (Point (x1, y1)) (Point (x2, y2)) =
  Debug.trace "edwards"
    $ let pointSmush = multiplyInteger x1 (multiplyInteger x2 (multiplyInteger y1 y2))
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
inv x = Debug.trace ("inv: " <> P.show x) expMod x (subtractInteger q 2) q

div :: Integer -> Integer -> Integer
div x y = fst $ divMod x y

mod :: Integer -> Integer -> Integer
mod x y = snd $ divMod x y

expMod :: Integer -> Integer -> Integer -> Integer
expMod x 0 p = 1
expMod x 1 p = x `mod` p
expMod !x !y !p
  | odd y =
      let !res = expMod x (y `div` 2) p `mod` p
       in ((x `mod` p) * (res `mod` p * res `mod` p)) `mod` p
  | otherwise =
      let !x' = expMod x (y `div` 2) p `mod` p
       in (x' `mod` p * x' `mod` p) `mod` p

i :: Integer
i = expMod 2 (quotientInteger (subtractInteger q 1) 4) q

xRecover :: Integer -> Integer
xRecover y =
  Debug.trace "xRecover"
    $ let xx = multiplyInteger (multiplyInteger y (subtractInteger y 1)) (inv (multiplyInteger d (multiplyInteger y (addInteger y 1))))
          x = Debug.trace msg $ expMod xx (quotientInteger (addInteger q 3) 8) q
          cond1 = not (equalsInteger 0 (remainderInteger (subtractInteger (multiplyInteger x x) xx) q))
          cond2 = odd x
          cond1Res = remainderInteger (multiplyInteger x i) q
          cond2Res = subtractInteger q x
          cond12Res = subtractInteger q cond1Res

          msg = "xRecover(expMod args)\n\nx: " <> P.show xx <> "\n\ny: " <> P.show (quotientInteger (addInteger q 3) 8) <> "\n\np: " <> P.show q <> "\n" <> replicate 20 '-' <> "\n\n"
       in Debug.trace msg
            $ if cond1
              then (if cond2 then cond12Res else cond1Res)
              else (if cond2 then cond2Res else x)

simpleTest :: P.IO ()
simpleTest = do
  traceM "A"
  (refPK@(PublicKey pk), refSK) <- Reference.createKeypair
  let rawMsg :: BS.ByteString
      !rawMsg = "helloworld"
  traceM "B"
  let !signedRawRef@(Signature signedRaw) = Reference.dsign refSK rawMsg
  traceM "C"
  let !verifyRaw = Reference.dverify refPK rawMsg signedRawRef
  traceM "D"
  let !verifyPlutus = checkValid (BuiltinByteString signedRaw) (BuiltinByteString rawMsg) (BuiltinByteString pk)
  traceM "E"
  traceM $ "REFERENCE IMPLEMENTATION VERIFICATION: " <> P.show verifyRaw
  traceM $ "PLUTUS IMPLEMENTATION VERIFICATION: " <> P.show verifyPlutus
