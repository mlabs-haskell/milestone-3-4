module MyLib (mkData) where

import qualified PlutusTx as PlutusTx

mkData :: PlutusTx.Data
mkData = PlutusTx.toData (Just 1 :: Maybe Integer)
