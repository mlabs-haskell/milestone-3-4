module Main (main) where

import MyLib (mkData)

main :: IO ()
main = do
  putStrLn $ "Basic PlutusTx check: " <> show mkData
  error "Test suite not yet implemented."
