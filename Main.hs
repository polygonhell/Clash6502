{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import CLaSH.Prelude
import CLaSH.Sized.Unsigned
import Language.Haskell.TH
import CLaSH.Promoted.Nat

import SevenSeg
import Cpu



-- declare d65536
$(decLiteralD 65536)


{-# ANN topEntity
  (TopEntity
    { t_name = "main"
    , t_inputs = []
    , t_outputs = ["SS_ANODES", "SS_SEGS"]
    , t_extraIn = [ ("CLOCK_32", 1)
                  ]
    , t_extraOut = []
    , t_clocks   = [ (clockWizard "clkwiz50"
                             "CLOCK_32(0)"
                             "'0'") 
                   ]
}) #-}  



topEntity :: Signal (BitVector 4, BitVector 8)
topEntity = ss where 
  ss = sevenSegA (prPC <$> topEntity')


topEntity' :: Signal CpuProbes
topEntity' = probes where
  (out, probes) = unbundle $ cpuA $ (CpuIn <$> din)
  adr = (resize . addr) <$> out :: Signal (Unsigned 9)
  din = romPow2 (replicate d512 (3 ::Unsigned 8)) adr
  -- din = blockRamPow2 (replicate d65536 (3 ::Unsigned 8)) adr adr (writeEn <$> out) (dataOut <$> out)
