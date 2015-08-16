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
import Cpu
import qualified Data.List as L

-- declare d65536
$(decLiteralD 65536)
$(decLiteralD 65016)



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
ram64K addr wrEn dataIn = blockRamPow2 testRAMContents addr addr wrEn dataIn

testRAMContents :: Vec 65536 Byte
testRAMContents = (0x1:>0x2:>0x3:>0x4:>0x05 :> Nil) ++ 
                  (replicate d507 0) ++ 
                  (
                   0xad:>0x00:>0x02 :>    -- lda $200                   
                   0x00 :> Nil) ++ 
                  (replicate d65016 (0xa9 ::Byte)) ++ (0x00 :> 0x02 :> 0x00 :> 0x00 :> Nil)



system :: Signal CpuProbes
system = probes where
  (out, probes) = unbundle $ cpuA $ (CpuIn <$> din)
  adr = (resize . addr) <$> out :: Signal (Unsigned 16)
  din = ram64K adr (writeEn <$> out) (dataOut <$> out)


-- Note we have to drop 1 because the initial state of dIn is undefined
runSystem = putStr $ unlines $ L.map (show) $L.drop 1 (sampleN 40 system)


