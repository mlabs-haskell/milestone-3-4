{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Based on https://ed25519.cr.yp.to/python/ed25519.py
module Ed25519 where

import Crypto.Sign.Ed25519 (PublicKey (..), SecretKey (..), Signature (..))
import Crypto.Sign.Ed25519 qualified as Reference
import Data.ByteString qualified as BS
import Debug.Trace (traceM)
import Debug.Trace qualified as Debug
import GHC.ByteOrder (ByteOrder (..))
import PlutusTx.Builtins
  ( BuiltinByteString,
    Integer,
    appendByteString,
    byteStringToInteger,
    equalsInteger,
    integerToByteString,
    lengthOfByteString,
    multiplyInteger,
    readBit,
    remainderInteger,
    sliceByteString,
    subtractInteger,
  )
import PlutusTx.Builtins.Internal (BuiltinByteString (..))
import PlutusTx.Prelude
  ( AdditiveGroup ((-)),
    AdditiveSemigroup ((+)),
    Bool,
    Eq ((==)),
    MultiplicativeSemigroup ((*)),
    Semigroup ((<>)),
    divMod,
    fst,
    not,
    odd,
    otherwise,
    snd,
    ($),
    (&&),
    (.),
    (/=),
  )
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
      msg =
        "checkValid\n\n"
          <> "sig len: "
          <> P.show (lengthOfByteString sig)
          <> "\n\nmsg len: "
          <> P.show (lengthOfByteString message)
          <> "\n\npk len: "
          <> P.show (lengthOfByteString pubKey)
   in Debug.trace msg $ eqPoint (scalarMult bPoint s) (edwards r (scalarMult a h))

-- Helpers

newtype Point = Point (Integer, Integer) deriving stock (P.Show)

eqPoint :: Point -> Point -> Bool
eqPoint p1@(Point (x, y)) p2@(Point (x1, y1)) =
  equalsInteger x x1
    && equalsInteger y y1

-- 2 ^ 255 - 19, but as a constant
q :: Integer
q = 57896044618658097711785492504343953926634992332820282019728792003956564819949

bx :: Integer
bx = xRecover by

by :: Integer
by = 4 * inv 5

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
decodeInt = byteStringToInteger LittleEndian

hint :: BuiltinByteString -> Integer
hint = byteStringToInteger BigEndian . sha512

encodePoint :: Point -> BuiltinByteString
encodePoint (Point (_, y)) = integerToByteString LittleEndian 32 y

scalarMult :: Point -> Integer -> Point
scalarMult p e =
  if equalsInteger e 0
    then Point (0, 1)
    else
      let q' = scalarMult p (e `div` 2)
          q'' = edwards q' q'
       in if odd e then edwards q'' p else q''

d :: Integer
d = multiplyInteger (-121665) (inv 121666)

edwards :: Point -> Point -> Point
edwards (Point (x1, y1)) (Point (x2, y2)) = Point (x3 `mod` q, y3 `mod` q)
  where
    x3 = (x1 * y2 + x2 * y1) * inv (1 + d * x1 * x2 * y1 * y2)
    y3 = (y1 * y2 + x1 * x2) * inv (1 - d * x1 * x2 * y1 * y2)

inv :: Integer -> Integer
inv x = expMod x (q - 2) q

div :: Integer -> Integer -> Integer
div x y = fst $ divMod x y

mod :: Integer -> Integer -> Integer
mod x y = snd $ divMod x y

expMod :: Integer -> Integer -> Integer -> Integer
expMod _ 0 _ = 1
expMod x 1 p = x `mod` p
expMod !x !y !p
  | odd y =
      let !res = expMod x (y `div` 2) p `mod` p
       in ((x `mod` p) * (res `mod` p * res `mod` p)) `mod` p
  | otherwise =
      let !x' = expMod x (y `div` 2) p `mod` p
       in (x' `mod` p * x' `mod` p) `mod` p

i :: Integer
i = expMod 2 ((q - 1) `div` 4) q

l :: Integer
l = 2 P.^ 252 + 27742317777372353535851937790883648493

sign :: BuiltinByteString -> BuiltinByteString -> BuiltinByteString -> BuiltinByteString
sign msg privKey pubKey = encodePoint r' <> integerToByteString LittleEndian 32 s
  where
    b = 256
    h = sha512 privKey
    a = byteStringToInteger BigEndian (sliceByteString 3 (b - 4) h)
    r = hint (sliceByteString 32 32 h <> msg)
    r' = scalarMult bPoint r
    s = (r + hint (encodePoint r' <> pubKey <> msg) * a) `mod` l

xRecover :: Integer -> Integer
xRecover y
  | cond1 && not cond2 = xA
  | cond1 && cond2 = xAB
  | not cond1 && cond2 = xB
  | otherwise = x
  where
    xx = (y * (y - 1)) * inv (d * y * (y + 1))
    x = expMod xx ((q + 3) `div` 8) q
    xA = (x * i) `mod` q
    xB = q - x
    xAB = q - xA
    cond1 = (x * x - xx) `mod` q /= 0 -- x here is always the input x
    cond2 = if cond1 then odd xA else odd x

simpleTest :: P.IO ()
simpleTest = do
  (refPK@(PublicKey pk), refSK@(SecretKey sk)) <- Reference.createKeypair
  let rawMsg :: BS.ByteString
      !rawMsg = "helloworld"
      !signedRawRef@(Signature signedRaw) = Reference.dsign refSK rawMsg
      !signedPlutus = sign (BuiltinByteString rawMsg) (BuiltinByteString sk) (BuiltinByteString pk)
      !verifyRaw = Reference.dverify refPK rawMsg signedRawRef
      !verifyPlutus = checkValid (BuiltinByteString signedRaw) (BuiltinByteString rawMsg) (BuiltinByteString pk)
  traceM $ "REFERENCE SIGNATURE: " <> P.show signedRaw
  traceM $ "PLUTUS SIGNATURES: " <> P.show signedPlutus

  traceM $ "REFERENCE IMPLEMENTATION VERIFICATION: " <> P.show verifyRaw
  traceM $ "PLUTUS IMPLEMENTATION VERIFICATION: " <> P.show verifyPlutus
