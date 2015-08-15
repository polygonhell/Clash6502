{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Cpu (cpuA, CpuIn(..), CpuOut(..), CpuProbes(..), State(..) ) where

import CLaSH.Prelude
import CLaSH.Sized.Unsigned
import Debug.Trace

import Types
import Opcodes
import InstructionDecode


resetVector :: Addr
resetVector = 0xfffc

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
            deriving (Show)


data CpuProbes = CpuProbes
  { prState :: State
  , prPC :: Addr
  , prA :: Byte
  , prAddr :: Addr
  } deriving (Show)


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
  (reg', st', wrEn, dO) = run2 reg dataIn 
  cpuOut = CpuOut {dataOut = dO , addr = pcReg reg', writeEn = wrEn}
  cpuProbes = probes st' reg'


cpu (Halt, reg@CpuRegisters{..}) CpuIn{..} = ((Halt, reg), (cpuOut, cpuProbes)) where
  cpuOut = CpuOut {dataOut = 0 , addr = addrReg, writeEn = False}
  cpuProbes = probes Halt reg


probes :: State -> CpuRegisters -> CpuProbes
probes st CpuRegisters{..} = CpuProbes st pcReg aReg addrReg

-- Deal with 1 byte instructions
run :: DecodedInst -> CpuRegisters -> (CpuRegisters, State, Bool, Byte)
run de@DecodedInst{..} reg@CpuRegisters{..} = (reg', st, wrEn, dOut) where 
  pc' = pcReg + 1
  (reg', st, wrEn, dOut) = case (diOpType, diAddrMode) of
    (OTInterrupt, _) -> (reg, Halt, False, 0)
    (_, _) -> (reg { decoded = de, pcReg = pc'}, Fetch2, False, 0)


-- Deal with 2 byte instructions
run2 :: CpuRegisters -> Byte -> (CpuRegisters, State, Bool, Byte)
run2 reg@CpuRegisters{..} dIn = (reg', st, wrEn, dOut) where 
  DecodedInst{..} = decoded
  pc' = pcReg + 1
  (reg', st, wrEn, dOut) = case (diOpType, diAddrMode) of
    (OTLoad, AddrImmediate) -> ((load reg diReg dIn) {pcReg = pc'}, Fetch1, False, 0)
    (_, _) -> (reg, Halt, False, 0)



load :: CpuRegisters -> Reg -> Byte -> CpuRegisters
load regs RegA v = regs {aReg = v}
load regs RegX v = regs {xReg = v}
load regs RegY v = regs {yReg = v}
load regs RegSP v = regs {spReg = v}


cpuA = cpu `mealy` initialCpuState


