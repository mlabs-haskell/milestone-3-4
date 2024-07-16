module Main (main) where

import Milestone3 (nqueens)
import PlutusTx.Prelude
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Prelude qualified as HaskellPrelude

main :: HaskellPrelude.IO ()
main =
  defaultMain
    . testGroup "nqueens"
    $ [ testCase "basic" $ assertEqual "" [(0, 0), (1, 4), (2, 7), (3, 5), (4, 2), (5, 6), (6, 1), (7, 3)] (nqueens 8)
      ]
