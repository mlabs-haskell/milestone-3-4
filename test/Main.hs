module Main (main) where

import Control.Monad (replicateM)
import Crypto.Hash.SHA512 qualified as Reference
import Data.ByteString qualified as BS
import Data.Word (Word8)
import Ed25519
import PlutusTx.Builtins.Internal (BuiltinByteString (..))
import SHA512V2 (prettify, sha512)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, generate)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import Prelude (IO, Int, pure, replicate, show, ($), (<>), (==))

main :: IO ()
main =
  defaultMain $
    testGroup
      "milestone-3"
      [ testGroup "ed25519_Tests" (replicate 5000 ed25519Test),
        testGroup "SHA512_Tests" (replicate 5000 sha512Test)
      ]

ed25519Test :: TestTree
ed25519Test = testCase "ed25519" simpleTest

sha512Test :: TestTree
sha512Test = testCase "SHA512" $ do
  inp <- generate arbitraryData
  mkSha512Test inp

arbitraryData :: Gen BS.ByteString
arbitraryData = do
  len <- arbitrary @Int
  rawBytes <- replicateM len (arbitrary @Word8)
  pure $ BS.pack rawBytes

mkSha512Test :: BS.ByteString -> Assertion
mkSha512Test str = do
  let testBS :: BS.ByteString
      testBS = str

      testBS_BI :: BuiltinByteString
      testBS_BI = BuiltinByteString testBS

      resultRef = Reference.hash testBS
      resultPlutus@(BuiltinByteString resultPlutusBS) = sha512 testBS_BI

      msg =
        prettify
          [ "testSha512\n\n",
            "Test String: " <> show testBS <> "\n\n",
            "Reference Result:\n" <> show resultRef <> "\n\n",
            "Plutus Result:\n" <> show resultPlutus <> "\n" <> replicate 20 '-' <> "\n"
          ]
  assertBool "results match" (resultRef == resultPlutusBS)
