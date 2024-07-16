{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Compile (nqueensCompiled) where

import Milestone3 (nqueens)
import PlutusTx.Code (CompiledCode)
import PlutusTx.Prelude
import PlutusTx.TH (compile)

nqueensCompiled :: CompiledCode (Integer -> [(Integer, Integer)])
nqueensCompiled = $$(compile [||nqueens||])
