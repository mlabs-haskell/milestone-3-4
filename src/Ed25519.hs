{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Based on https://ed25519.cr.yp.to/python/ed25519.py
module Ed25519 where

import Control.Monad (unless)
import Crypto.Sign.Ed25519 (PublicKey (..), SecretKey (..), Signature (..))
import Crypto.Sign.Ed25519 qualified as Reference
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as S8
import Data.Char (intToDigit)
import Data.Word qualified as P
import Debug.Trace (traceM)
import Debug.Trace qualified as Debug
import GHC.ByteOrder (ByteOrder (..))
import Numeric (showHex, showIntAtBase)
import PlutusTx.Builtins
  ( BuiltinByteString,
    Integer,
    appendByteString,
    byteStringToInteger,
    divideInteger,
    equalsInteger,
    integerToByteString,
    lengthOfByteString,
    modInteger,
    multiplyInteger,
    readBit,
    sliceByteString,
    toBuiltin,
    writeBits,
  )
import PlutusTx.Builtins.Internal (BuiltinBool, BuiltinByteString (..), BuiltinList (..), BuiltinPair (..))
import PlutusTx.List (replicate)
import PlutusTx.Prelude
  ( AdditiveGroup ((-)),
    AdditiveSemigroup ((+)),
    Bool (..),
    Eq ((==)),
    MultiplicativeSemigroup ((*)),
    Semigroup ((<>)),
    divMod,
    fst,
    not,
    odd,
    otherwise,
    product,
    snd,
    ($),
    (&&),
    (.),
    (/=),
  )
import SHA512V2 (doTrace, prettify, sha512)
import Prelude qualified as P

showHex' :: BuiltinByteString -> [P.Char]
showHex' (BuiltinByteString bi) = P.concatMap (\x -> pad $ showHex x []) (BS.unpack bi)
  where
    pad [x] = ['0', x]
    pad s = s

-- No checks for size, though we should be doing them.
checkValid ::
  BuiltinByteString ->
  BuiltinByteString ->
  BuiltinByteString ->
  Bool
checkValid sig message pubKey =
  let sliceR = (sliceByteString 0 32 sig)
      !r = decodePoint (sliceByteString 0 32 sig)
      !a = decodePoint pubKey
      !s = decodeInt (sliceByteString 32 32 sig)
      hintArg = (encodePoint r <> pubKey <> message)
      !h = hint hintArg
      msg =
        prettify
          [ "checkValid",
            "sig len: " <> P.show (lengthOfByteString sig),
            "nmsg len: " <> P.show (lengthOfByteString message),
            "pk len: " <> P.show (lengthOfByteString pubKey),
            "hint " <> showHex' hintArg <> " = " <> P.show h,
            "sig: " <> P.show (showHex' sig),
            "msg: " <> P.show (showHex' message),
            "pubKey: " <> P.show (showHex' pubKey),
            "r: " <> P.show r,
            "a: " <> P.show a,
            "s: " <> P.show s,
            "sliceR: " <> showHex' sliceR
          ]
   in doTrace msg $ eqPoint (scalarMult bPoint s) (edwards r (scalarMult a h))

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
bPoint = Point (bx `mod` q, by `mod` q)

setBit :: Integer -> BuiltinByteString -> BuiltinByteString
setBit ix bs = writeBits bs arg
  where
    arg :: BuiltinList (BuiltinPair Integer BuiltinBool)
    arg = toBuiltin [(ix, True)]

clearBit :: Integer -> BuiltinByteString -> BuiltinByteString
clearBit ix bs = writeBits bs arg
  where
    arg :: BuiltinList (BuiltinPair Integer BuiltinBool)
    arg = toBuiltin [(ix, False)]

decodePoint :: BuiltinByteString -> Point
decodePoint bs
  | odd x /= x_0 = Point (q - x, yInt)
  | otherwise = Point (x, yInt)
  where
    x_0 = readBit bs 7
    x = xRecover yInt
    yInt = decodeInt $ clearBit 7 bs

decodeInt :: BuiltinByteString -> Integer
decodeInt = byteStringToInteger LittleEndian

hint :: BuiltinByteString -> Integer
hint bs = doTrace msg result
  where
    msg =
      prettify
        [ "hint",
          "input: " <> showBin bs,
          "result: " <> P.show result
        ]
    result = byteStringToInteger LittleEndian . sha512 $ bs

encodePoint :: Point -> BuiltinByteString
encodePoint (Point (x, y)) = result
  where
    zero :: Integer
    zero = 7

    wbArg :: BuiltinList (BuiltinPair Integer BuiltinBool)
    wbArg = toBuiltin [(zero, xLSBVal)]
    result = writeBits yBS wbArg
    yBS = integerToByteString LittleEndian 32 y
    xBS = integerToByteString LittleEndian 32 x
    xLSBVal = readBit xBS 248

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

(**) :: Integer -> Integer -> Integer
a ** b = product $ replicate b a

infixr 8 **

edwards :: Point -> Point -> Point
edwards (Point (x1, y1)) (Point (x2, y2)) = Point (x3 `mod` q, y3 `mod` q)
  where
    x3 = (x1 * y2 + x2 * y1) * inv (1 + d * x1 * x2 * y1 * y2)
    y3 = (y1 * y2 + x1 * x2) * inv (1 - d * x1 * x2 * y1 * y2)

inv :: Integer -> Integer
inv x = expMod x (q - 2) q

div :: Integer -> Integer -> Integer
div = divideInteger

mod :: Integer -> Integer -> Integer
mod = modInteger

expMod :: Integer -> Integer -> Integer -> Integer
expMod b' e m =
  if (e == 0)
    then 1
    else
      let reduced = expMod b' (e `div` 2) m
          t = (reduced * reduced) `mod` m
       in if odd e
            then (t * b') `mod` m
            else t

i :: Integer
i = expMod 2 ((q - 1) `div` 4) q

l :: Integer
l = 2 P.^ 252 + 27742317777372353535851937790883648493

xRecover :: Integer -> Integer
xRecover y =
  doTrace msg
    $ if
      | cond1 && not cond2 -> xA
      | cond1 && cond2 -> xAB
      | not cond1 && cond2 -> xB
      | otherwise -> x
  where
    msg =
      prettify
        [ "xRecover",
          "y: " <> P.show y,
          "xx: " <> P.show xx,
          "x: " <> P.show x
        ]
    xx = ((y *) y - 1) * inv ((d * y *) y + 1)
    x = expMod xx ((q + 3) `div` 8) q
    xA = (x *) i `mod` q
    xB = q - x
    xAB = q - xA
    cond1 = (x * x - xx) `mod` q /= 0 -- x here is always the input x
    cond2 = if cond1 then odd xA else odd x

{- Utilities -}

samplePoint = Point (0x00000045, 0x00000065)

printBSBin :: BuiltinByteString -> P.IO ()
printBSBin = P.putStrLn . showBin

showBin :: BuiltinByteString -> P.String
showBin (BuiltinByteString bi) = "\n" P.<> P.concatMap go (BS.unpack bi) <> "\n\n"
  where
    go :: P.Word8 -> P.String
    go w =
      let w' = showIntAtBase 2 intToDigit w ""
          paddingSize = 8 P.- P.length w'
          padding = P.replicate paddingSize '0'
       in padding <> w' <> "\n"

simpleTest :: P.IO ()
simpleTest = do
  (refPK@(PublicKey pk), refSK@(SecretKey sk)) <- Reference.createKeypair
  let rawMsg :: BS.ByteString
      !rawMsg = "helloworld"
      !signedRawRef@(Signature signedRaw) = Reference.dsign refSK rawMsg
      !verifyRaw = Reference.dverify refPK rawMsg signedRawRef
      !verifyPlutus = checkValid (BuiltinByteString signedRaw) (BuiltinByteString rawMsg) (BuiltinByteString pk)

  traceM $ "REFERENCE IMPLEMENTATION VERIFICATION: " <> P.show verifyRaw
  traceM $ "PLUTUS IMPLEMENTATION VERIFICATION: " <> P.show verifyPlutus

  if verifyRaw == verifyPlutus
    then P.pure ()
    else P.error "error: ed25519 test fail"
