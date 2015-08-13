{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}


module SevenSeg (sevenSegA) where

import CLaSH.Prelude
import CLaSH.Sized.Unsigned
import qualified Data.List as L


digitToSeg :: Unsigned 4 -> BitVector 8
digitToSeg 0x0 = 0xc0
digitToSeg 0x1 = 0xf9
digitToSeg 0x2 = 0xa4
digitToSeg 0x3 = 0xb0
digitToSeg 0x4 = 0x99
digitToSeg 0x5 = 0x92
digitToSeg 0x6 = 0x82
digitToSeg 0x7 = 0xf8
digitToSeg 0x8 = 0x80
digitToSeg 0x9 = 0x90
digitToSeg 0xa = 0x88
digitToSeg 0xb = 0x83
digitToSeg 0xc = 0xc6
digitToSeg 0xd = 0xa1
digitToSeg 0xe = 0x86
digitToSeg 0xf = 0x8e
 
 
anode :: Unsigned 2 -> BitVector 4
anode 0 = 0x7
anode 1 = 0xb
anode 2 = 0xd
anode 3 = 0xe

sh :: Unsigned 2 -> Int
sh 0 = 0
sh 1 = 4
sh 2 = 8
sh 3 = 12

multiplex :: (Unsigned 2, Unsigned 16) -> Unsigned 16 -> ((Unsigned 2, Unsigned 16), (BitVector 4, BitVector 8))
multiplex (d, cnt) v = ((d', cnt+1), (an, leds)) where
  d' | (cnt == 0) = d+1
     | otherwise = d
  an = anode d
  leds = digitToSeg (resize (v `shiftR` (sh d)))

sevenSegA = multiplex `mealy` (0, 0)

