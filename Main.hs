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

import Cpu


-- declare d4096
$(decLiteralD 4096)


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



topEntity :: Signal CpuProbes
topEntity = probes where
  (out, probes) = unbundle $ cpuA $ (CpuIn <$> din)
  adr = resize <$> (addr <$> out) :: Signal (Unsigned 12)
  din = blockRamPow2 (replicate d4096 (3 ::Unsigned 8)) adr adr (writeEn <$> out) (dataOut <$> out)
