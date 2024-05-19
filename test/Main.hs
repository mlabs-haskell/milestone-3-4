module Main (main) where

import MyLib (mkData)

main :: IO ()
main = do
  putStrLn $ "Basic PlutusTx check: " <> show mkData
  putStrLn "Test suite not yet implemented."
