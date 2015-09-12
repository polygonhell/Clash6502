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

import Types
import SevenSeg
-- import Cpu
import Cpu2
import qualified Data.List as L

-- declare d65536
$(decLiteralD 65536)
$(decLiteralD 65012)



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
  ss = sevenSegA (resize . prA <$> system)

ram64K :: Signal Addr -> Signal Bool -> Signal Byte -> Signal Byte
-- ram64K addr wrEn dataIn = blockRamPow2 testRAMContents addr addr wrEn dataIn
ram64K addr wrEn dataIn = unpack <$> blockRamFilePow2 "utils/6502_functional_test.bin" addr addr wrEn (pack <$> dataIn)





-- system :: Signal CpuProbes
-- system = probes where
--   (out, probes) = unbundle $ cpuA $ (CpuIn <$> din)
--   adr = (resize . addr) <$> out :: Signal (Unsigned 16)
--   din = ram64K adr (writeEn <$> out) (dataOut <$> out)

system :: Signal Probes
system = probes where
  (out, probes) = unbundle $ cpuM $ (CpuIn <$> din)
  adr = (resize . addr) <$> out :: Signal (Unsigned 16)
  din = ram64K adr (writeEn <$> out) (dOut <$> out)


-- Note we have to drop 1 because the initial state of dIn is undefined
runSystem = putStr $ unlines $ L.map (show) $L.drop 1 (sampleN 100000 system)


