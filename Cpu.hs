{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Cpu (cpuA, CpuIn(..), CpuOut(..), CpuProbes(..), State(..) ) where

import CLaSH.Prelude
import CLaSH.Sized.Unsigned
import Debug.Trace
import qualified Data.List as L
import Text.Printf

import Types
import Opcodes
import InstructionDecode


resetVector :: Addr
resetVector = 0xfffc


negFlag   = 0x80 :: Byte
ovFlag    = 0x40 :: Byte
decFlag   = 0x08 :: Byte
intFlag   = 0x04 :: Byte
zeroFlag  = 0x02 :: Byte
carryFlag = 0x01 :: Byte

zeroNegMask = complement(negFlag .|. zeroFlag)
zeroNegCarryOverflowMask = complement(negFlag .|. zeroFlag .|. ovFlag .|. carryFlag)


pRegString :: Byte -> String
pRegString v = res where
  c = if (v .&. carryFlag) /= 0 then "C" else "c" 
  z = if (v .&. zeroFlag) /= 0 then "Z" else "z" 
  o = if (v .&. ovFlag) /= 0 then "O" else "o" 
  n = if (v .&. negFlag) /= 0 then "N" else "n" 
  res = n L.++ o L.++ z L.++ c 


data CpuIn = CpuIn 
  { dataIn :: Byte
  } deriving (Show)

data CpuOut = CpuOut 
  { dataOut :: Byte
  , addr :: Addr
  , writeEn :: Bool
  } deriving (Show)

data State =  Halt
            | Init
            | FetchPCL
            | FetchPCH 
            | Fetch1
            | Fetch2
            | Read1
            deriving (Show)


data CpuProbes = CpuProbes
  { prState :: State
  , prPC :: Addr
  , prA :: Byte
  , prFlags :: Byte
  , prAddr :: Addr
  }

instance Show CpuProbes where
  show a = str where 
    -- str = "Hello"
    CpuProbes{..} = a
    str = (printf "%10s" (show prState)) L.++ " " L.++ (printf "%04x" (toInteger prPC)) L.++ " " L.++ (printf "%02x" (toInteger prA)) L.++ " " L.++ (pRegString prFlags) L.++ " " L.++ (show prAddr)



data CpuRegisters = CpuRegisters
  { aReg :: Byte
  , xReg :: Byte
  , yReg :: Byte
  , pReg :: Byte
  , spReg :: Byte
  , pcReg :: Addr
  -- requested address and dataOut
  , addrReg :: Addr
  -- , dbReg :: Byte
  -- current decoded instruction
  , decoded :: DecodedInst
  } deriving (Show)




initialProcessorRegisters :: CpuRegisters
initialProcessorRegisters = CpuRegisters 0xaa 0 0 0x02 0xfd 0x00ff 0 decodedNop



type CpuState = (State, CpuRegisters)
initialCpuState :: CpuState
initialCpuState = (Init, initialProcessorRegisters)


cpu :: CpuState -> CpuIn -> (CpuState, (CpuOut, CpuProbes))
-- Initial CPU State, just sets up the read address to the resetVector and initiates the read
cpu (Init, reg@CpuRegisters{..}) CpuIn{..} = ((st', reg'), (cpuOut, cpuProbes)) where
  st' = FetchPCL
  reg' = reg {addrReg = resetVector}
  cpuOut = CpuOut {dataOut = 0 , addr = resetVector, writeEn = False}
  cpuProbes = probes st' reg'

-- Two states to Fetch the 16 bit PC from memory
cpu (FetchPCL, reg@CpuRegisters{..}) CpuIn{..} = ((st', reg'), (cpuOut, cpuProbes)) where 
  st' = FetchPCH
  pc' = resize dataIn
  addr' = addrReg + 1
  reg' = reg {pcReg = pc', addrReg = addr'}
  cpuOut = CpuOut {dataOut = 0 , addr = addr', writeEn = False}
  cpuProbes = probes st' reg'

cpu (FetchPCH, reg@CpuRegisters{..}) CpuIn{..} = ((st', reg'), (cpuOut, cpuProbes)) where 
  st' = Fetch1
  pc' = (pcReg .&. 0xff) .|. ((resize dataIn) `shiftL` 8) 
  -- TODO RTS instruction requires incrementing the PC on return
  reg' = reg {pcReg = pc'}
  cpuOut = CpuOut {dataOut = 0 , addr = pc', writeEn = False}
  cpuProbes = probes st' reg'

-- Instruction Fetch
cpu (Fetch1, reg) CpuIn{..} = ((st', reg'), (cpuOut, cpuProbes)) where
  di@DecodedInst{..} = decode dataIn
  (reg', st', wrEn, dO) = run di reg
  cpuOut = CpuOut {dataOut = dO, addr = pcReg reg', writeEn = False}
  cpuProbes = probes st' reg'

cpu (Fetch2, reg) CpuIn{..} = ((st', reg'), (cpuOut, cpuProbes)) where
  DecodedInst{..} = decoded reg
  (reg', st', addr',  wrEn, dO) = run2 reg diAddrMode dataIn 
  cpuOut = CpuOut {dataOut = dO , addr = addr' , writeEn = wrEn}
  cpuProbes = probes st' reg'

-- read a 1 byte value
cpu (Read1, reg) CpuIn{..} = ((st', reg'), (cpuOut, cpuProbes)) where
  (reg', st', addr',  wrEn, dO) = run2 reg AddrImmediate dataIn
  cpuOut = CpuOut {dataOut = dO , addr = addr' , writeEn = wrEn}
  cpuProbes = probes st' reg'


cpu (Halt, reg@CpuRegisters{..}) CpuIn{..} = ((Halt, reg), (cpuOut, cpuProbes)) where
  cpuOut = CpuOut {dataOut = 0 , addr = addrReg, writeEn = False}
  cpuProbes = probes Halt reg


probes :: State -> CpuRegisters -> CpuProbes
probes st CpuRegisters{..} = CpuProbes st pcReg aReg pReg addrReg 

-- Deal with 1 byte instructions
run :: DecodedInst -> CpuRegisters -> (CpuRegisters, State, Bool, Byte)
run de@DecodedInst{..} reg@CpuRegisters{..} = (reg', st, wrEn, dOut) where 
  pc' = pcReg + 1
  (reg', st, wrEn, dOut) = case (diOpType, diAddrMode) of
    (OTInterrupt, _) -> (reg, Halt, False, 0)
    (_, _) -> (reg { decoded = de, pcReg = pc'}, Fetch2, False, 0)


-- Deal with 2 byte instructions
run2 :: CpuRegisters -> AddrMode -> Byte -> (CpuRegisters, State, Addr, Bool, Byte)
run2 reg@CpuRegisters{..} addrMode dIn = (reg', st, addr, wrEn, dOut) where 
  DecodedInst{..} = decoded
  pc' = pcReg + 1
  (reg', st, addr, wrEn, dOut) = 
    case (addrMode) of
      AddrZeroPage -> (reg, Read1, resize dIn, False, 0) 
      AddrImmediate -> case (diOpType) of
                         OTLoad -> ((load reg diReg dIn) {pcReg = pc'}, Fetch1, pc', False, 0)
                         OTAdc -> ((adc reg dIn) {pcReg = pc'}, Fetch1, pc', False, 0) 
                         _ -> (reg, Halt, pc', False, 0)



adc :: CpuRegisters -> Byte -> CpuRegisters
adc regs@CpuRegisters{..} v = regs' where
  cIn = pReg .&. carryFlag
  res9 = (resize cIn :: Unsigned 9) + (resize aReg :: Unsigned 9) + (resize v :: Unsigned 9)
  cOut = resize (res9 `shiftR` 8) :: Unsigned 8
  res = resize res9  :: Unsigned 8
  overflow = if (((aReg `xor` res) .&. (v `xor` res) .&. 0x80) == 0) then 0 else ovFlag
  flags = (pReg .&. zeroNegCarryOverflowMask) .|. cOut .|. overflow .|. (setZeroAndNeg res)
  regs' = regs {aReg = res, pReg = flags}

load :: CpuRegisters -> Reg -> Byte -> CpuRegisters
load regs@CpuRegisters{..} reg v = regs' where
  p' = (pReg .&. zeroNegMask) .|. (setZeroAndNeg v)
  regs' = case reg of
    RegA -> regs {aReg = v, pReg = p'}
    RegX -> regs {xReg = v, pReg = p'}
    RegY -> regs {yReg = v, pReg = p'} 

setZeroAndNeg :: Byte -> Byte
setZeroAndNeg 0 = zeroFlag
setZeroAndNeg a = a .&. 0x80 


cpuA = cpu `mealy` initialCpuState


