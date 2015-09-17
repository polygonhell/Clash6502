{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Alu2 where


import CLaSH.Prelude
import CLaSH.Sized.Unsigned

import qualified Data.List as L
import Text.Printf
import Debug.Trace


--
-- Note this is not the same as the ALU un a real 65XX
-- It incorporates the input inverter and the bcd Adjust
-- Both are separate entities on the real silicon
-- As a result Sub is an OP
-- Left Shift should still be performed as an add
--


data AluOp = AluADD
           | AluSUB -- Real 6502 Doesn't have this explicitly - adding it here allows us to not model the separate inverter
           | AluOR
           | AluAND
           | AluXOR
           | AluRSHIFT


type Byte = BitVector 8
type Nibble = BitVector 4


-- op aIn bIn cIn bcd (out, carry, overflow)
alu :: AluOp -> Byte -> Byte -> Bit -> Bit -> (Byte, Bit, Bit)
alu op aIn bIn cIn bcd = case op of
  AluOR -> (aIn .|. bIn,  0, 0)
  AluAND -> (aIn .&. bIn, 0, 0)
  AluXOR -> (aIn `xor` bIn, 0, 0)
  AluRSHIFT -> (pack (cIn +>> v), v !! 7, 0) where
    v = unpack aIn :: Vec 8 Bit
  AluSUB -> (res', cOut, vOut) where 
    (res, hc, cOut, vOut) = add aIn (complement bIn) cIn 0
    res' = if bcd == 1 then bcdAdjustSub res hc cOut else res
  AluADD -> (res', cOut, vOut) where 
    (res, hc, cOut, vOut) = add aIn bIn cIn bcd
    res' = if bcd == 1 then bcdAdjustAdd res hc cOut else res

bcdAdjustAdd :: Byte -> Bit -> Bit -> Byte
bcdAdjustAdd aIn hc c = high ++# low where
  (hIn, lIn) = split aIn :: (Nibble, Nibble)
  low = if hc == 1 then lIn + 6 else lIn
  high = if c == 1 then hIn + 6 else hIn

bcdAdjustSub :: Byte -> Bit -> Bit -> Byte
bcdAdjustSub aIn hc c = high ++# low where
  (hIn, lIn) = split aIn :: (Nibble, Nibble)
  low = if hc == 0 then lIn + 10 else lIn
  high = if c == 0 then hIn + 10 else hIn

-- aIn bIn cIn bcd -> (res, hcOut, cOut, vOut)
add :: Byte -> Byte -> Bit -> Bit -> (Byte, Bit, Bit, Bit)
add aIn bIn cIn bcd = (res, hc, cOut, vOut) where 
  -- Do Add in 2 halfs to get intermediate carries
  (aHi, aLo) = split aIn :: (Nibble, Nibble)
  (bHi, bLo) = split bIn :: (Nibble, Nibble)
  (rLo, c0, _) = adder cIn aLo bLo
  hc =  c0 .|. if (rLo >= 10) then bcd else 0
  (rHi, c1, vOut) = adder hc aHi bHi
  cOut = c1 .|. if (rHi >= 10) then bcd else 0
  res = rHi ++# rLo

adder :: Bit -> Nibble -> Nibble -> (Nibble, Bit, Bit)
adder cIn xV yV = (pack (reverse sum), cOut, vOut) where
  x = reverse $ unpack xV
  y = reverse $ unpack yV
  res = zipWith3 fullAdder (cIn +>> carries) x y
  (sum, carries) = unzip res
  cOut = carries !! 3
  vOut = cOut `xor` (carries !! 2)

fullAdder :: Bit -> Bit -> Bit -> (Bit, Bit)
fullAdder cIn x y = (s, cOut) where
  p = x `xor` y
  s = p `xor`cIn
  cOut = if p == low then y else cIn


testBCD :: Bool
testBCD = res where
  inp = [(i, j, c) | i <- [0..99], j <- [0..99], c <- [0,1]] :: [(Integer, Integer, Bit)]
  res = and res'
  res' = L.map doAndCheck inp
  doAndCheck :: (Integer, Integer, Bit) -> Bool
  doAndCheck (a, b, c) = r where
    a' = fromInteger ((a `div` 10) * 16 + (a `mod` 10)) :: Byte
    b' = fromInteger ((b `div` 10) * 16 + (b `mod` 10)) :: Byte
    (r0, _, _) = alu AluSUB a' b' c 1
    (rHi, rLo) = split(r0) :: (Nibble, Nibble)
    r1 = (toInteger rHi) * 10 + (toInteger rLo)
    -- Compute actual result
    r2' = a - b - if c == 0 then 1 else 0
    r2 = if r2' < 0 then r2' + 100 else r2'
    r = trace (if r1 /= r2 then printf "%d %d %s = %d -- %d" a b (show c) r1 r2 else "OK") $ r1 == r2








