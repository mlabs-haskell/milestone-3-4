module MyLib (mkData) where

import PlutusTx (Data, toData)
import PlutusTx.Prelude

mkData :: Data
mkData = toData (Just 1 :: Maybe Integer)
