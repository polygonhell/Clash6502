{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Cpu (cpuA, Byte, Addr, CpuIn(..), CpuOut(..), CpuProbes(..), State(..) ) where

import CLaSH.Prelude
import CLaSH.Sized.Unsigned


type Byte = Unsigned 8
type Addr = Unsigned 16

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
            deriving (Show)


data CpuProbes = CpuProbes
  { prState :: State
  , prPC :: Addr
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
  , dbReg :: Byte
  } deriving (Show)


initialProcessorRegisters :: CpuRegisters
initialProcessorRegisters = CpuRegisters 0xaa 0 0 0x02 0xfd 0x00ff 0 0



type CpuState = (State, CpuRegisters)
initialCpuState :: CpuState
initialCpuState = (Init, initialProcessorRegisters)


cpu :: CpuState -> CpuIn -> (CpuState, (CpuOut, CpuProbes))
-- Initial CPU State, just sets up the read address to the resetVector and initiates the read
cpu (Init, reg) cpuIn = ((st', reg), (cpuOut, cpuProbes)) where
  st' = FetchPCL
  cpuOut = CpuOut {dataOut = dbReg reg , addr = resetVector, writeEn = False}
  cpuProbes = CpuProbes st' (pcReg reg)

cpu (FetchPCL, reg) cpuIn = ((st', reg'), (cpuOut, cpuProbes)) where 
  st' = FetchPCH
  pcReg' = resize (dataIn cpuIn)
  addrReg' = (addrReg reg) + 1
  reg' = reg {pcReg = pcReg', addrReg = addrReg'}
  cpuOut = CpuOut {dataOut = dbReg reg , addr = addrReg', writeEn = False}
  cpuProbes = CpuProbes st' pcReg'

cpu (FetchPCH, reg) cpuIn = ((st', reg'), (cpuOut, cpuProbes)) where 
  st' = Fetch1
  pcReg' = ((pcReg reg).&. 0xff) .|. ((resize (dataIn cpuIn)) `shiftL` 8) 
  -- TODO RTS instruction requires incrementing the PC on return
  addrReg' = pcReg' 
  reg' = reg {pcReg = pcReg', addrReg = addrReg'}
  cpuOut = CpuOut {dataOut = dbReg reg , addr = addrReg', writeEn = False}
  cpuProbes = CpuProbes st' pcReg'

cpu (Fetch1, reg) cpuIn = ((st', reg), (cpuOut, cpuProbes)) where 
  st' = Fetch1
  cpuOut = CpuOut {dataOut = dbReg reg , addr = addrReg reg, writeEn = False}
  cpuProbes = CpuProbes st' (pcReg reg)


cpuA = cpu `mealy` initialCpuState


