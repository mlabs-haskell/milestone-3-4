module Main (main) where

import MyLib (mkData)
import Prelude qualified as HaskellPrelude

main :: HaskellPrelude.IO ()
main = do
  HaskellPrelude.putStrLn
    ( "Basic PlutusTx check: "
        HaskellPrelude.<> HaskellPrelude.show mkData
    )
  HaskellPrelude.putStrLn "Test suite not yet implemented."
