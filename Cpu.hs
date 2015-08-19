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
zeroNegCarryMask = complement(negFlag .|. zeroFlag .|. carryFlag)
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
            | Fetch3
            | Write1
            | FetchAddr1
            deriving (Show)


data CpuProbes = CpuProbes
  { prStateIn :: State
  , prState :: State
  , prPC :: Addr
  , prA :: Byte
  , prX :: Byte
  , prY :: Byte
  , prFlags :: Byte
  , prAddr :: Addr 
  , prDIn :: Byte
  }

instance Show CpuProbes where
  show a = str where 
    -- str = "Hello"
    CpuProbes{..} = a
    str = (printf "%-8s" (show prStateIn)) L.++ " " L.++ 
          (printf "%-8s" (show prState)) L.++ " " L.++ 
          (printf "%04x" (toInteger prPC)) L.++ " " L.++ 
          (printf "%02x" (toInteger prA)) L.++ " " L.++ 
          (printf "%02x" (toInteger prX)) L.++ " " L.++ 
          (printf "%02x" (toInteger prY)) L.++ " " L.++ 
          (pRegString prFlags) L.++ " " L.++ 
          (printf "%04x" (toInteger prAddr)) L.++ " " L.++ 
          (printf "%02x" (toInteger prDIn))




data CpuRegisters = CpuRegisters
  { aReg :: Byte
  , xReg :: Byte
  , yReg :: Byte
  , pReg :: Byte
  , spReg :: Byte
  , pcReg :: Addr
  , addrReg :: Addr   -- Used for Indirect addressing 
  , decoded :: DecodedInst -- current decoded instruction
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
  cpuProbes = probes Init st' reg' dataIn

-- Two states to Fetch the 16 bit PC from memory
cpu (FetchPCL, reg@CpuRegisters{..}) CpuIn{..} = ((st', reg'), (cpuOut, cpuProbes)) where 
  st' = FetchPCH
  pc' = resize dataIn
  addr' = addrReg + 1
  reg' = reg {pcReg = pc'}
  cpuOut = CpuOut {dataOut = 0 , addr = addr', writeEn = False}
  cpuProbes = probes FetchPCL st' reg' dataIn

cpu (FetchPCH, reg@CpuRegisters{..}) CpuIn{..} = ((st', reg'), (cpuOut, cpuProbes)) where 
  st' = Fetch1
  pc' = (pcReg .&. 0xff) .|. ((resize dataIn) `shiftL` 8) 
  -- TODO RTS instruction requires incrementing the PC on return
  reg' = reg {pcReg = pc'}
  cpuOut = CpuOut {dataOut = 0 , addr = pc', writeEn = False}
  cpuProbes = probes FetchPCH st' reg' dataIn

-- Instruction Fetch
cpu (Fetch1, reg) CpuIn{..} = ((st', reg'), (cpuOut, cpuProbes)) where
  di@DecodedInst{..} = decode dataIn
  (reg', st', wrEn, dO) = run di reg
  cpuOut = CpuOut {dataOut = dO, addr = pcReg reg', writeEn = False}
  cpuProbes = probes Fetch1 st' reg' dataIn

cpu (Fetch2, reg) CpuIn{..} = ((st', reg'), (cpuOut, cpuProbes)) where
  DecodedInst{..} = decoded reg
  (reg', st', addr',  wrEn, dO) = run2 reg dataIn 
  cpuOut = CpuOut {dataOut = dO , addr = addr' , writeEn = wrEn}
  cpuProbes = probes Fetch2 st' reg' dataIn

-- Low High byte of a 2 byte address -- does not increase PC becasue it happens when the instruction is executed
cpu (Fetch3, reg@CpuRegisters{..}) CpuIn{..} = ((st', reg'), (cpuOut, cpuProbes)) where
  DecodedInst{..} = decoded
  (reg', st', addr',  wrEn, dO) = run3 reg dataIn
  cpuOut = CpuOut {dataOut = dO , addr = addr' , writeEn = wrEn}
  cpuProbes = probes Fetch3 st' reg dataIn

-- read a 1 byte value
-- cpu (Read1, reg) CpuIn{..} = ((st', reg'), (cpuOut, cpuProbes)) where
--   (reg', st', addr',  wrEn, dO) = run2 reg AddrImmediate dataIn
--   cpuOut = CpuOut {dataOut = dO , addr = addr' , writeEn = wrEn}
--   cpuProbes = probes Read1 st' reg' dataIn

-- write 1 byte value -- Really this just sets the address for the following Fetch
cpu (Write1, reg@CpuRegisters{..}) CpuIn{..} = ((st', reg), (cpuOut, cpuProbes)) where
  st' = Fetch1
  cpuOut = CpuOut {dataOut = 0 , addr = pcReg , writeEn = False}
  cpuProbes = probes Write1 st' reg dataIn

-- Fetch Addr from the address in addr
cpu (FetchAddr1, reg@CpuRegisters{..}) CpuIn{..} = ((st', reg'), (cpuOut, cpuProbes)) where
  st' = Fetch3
  addr' = resize dataIn
  reg' = reg { addrReg = addr' }
  cpuOut = CpuOut {dataOut = 0, addr = addrReg + 1 , writeEn = False} -- Fetch High Byte
  cpuProbes = probes Fetch3 st' reg' dataIn

cpu (Halt, reg@CpuRegisters{..}) CpuIn{..} = ((Halt, reg), (cpuOut, cpuProbes)) where
  cpuOut = CpuOut {dataOut = 0 , addr = pcReg, writeEn = False}
  cpuProbes = probes Halt Halt reg dataIn


probes :: State -> State -> CpuRegisters -> Byte -> CpuProbes
probes stIn st CpuRegisters{..} d = CpuProbes stIn st pcReg aReg xReg yReg pReg addrReg d

-- Deal with 1 byte instructions
run :: DecodedInst -> CpuRegisters -> (CpuRegisters, State, Bool, Byte)
run de@DecodedInst{..} reg@CpuRegisters{..} = (reg', st, wrEn, dOut) where 
  pc' = pcReg + 1
  (reg', st, wrEn, dOut) = case (diOpType, diAddrMode) of
    (OTAsl, AddrNone) -> (reg'' { aReg = v, pcReg = pc' }, Fetch1, False, 0) where
                         (v, reg'') = asl reg aReg
    (OTInterrupt, _) -> (reg, Halt, False, 0)
    (_, _) -> (reg { decoded = de, pcReg = pc'}, Fetch2, False, 0)


-- Deal with instructions that consume data
run2 :: CpuRegisters -> Byte -> (CpuRegisters, State, Addr, Bool, Byte)
run2 reg@CpuRegisters{..} dIn = (reg', st, addr, wrEn, dOut) where 
  DecodedInst{..} = decoded
  pc' = pcReg + 1
  zpAddr = computeAddrZP reg diAddrOffset dIn -- computed address for potential ZP access
  (reg', st, addr, wrEn, dOut) = 
    case (diAddrMode) of
      AddrZeroPage -> case (diOpType) of
                         OTStore -> (reg { pcReg = pc' }, Write1, zpAddr, True, regVal reg)
                         _ -> (reg { decoded = rewriteDecoded decoded, addrReg = zpAddr}, Fetch2, zpAddr, False, 0)  -- Note don't increase PC here because it runs back through Immediate once the fetch is complete
      AddrImmediate -> case (diOpType) of
                         OTLoad -> ((load reg diReg dIn) {pcReg = pc'}, Fetch1, pc', False, 0)
                         OTAdc -> ((adc reg dIn) {pcReg = pc'}, Fetch1, pc', False, 0) 
                         OTAnd -> ((andd reg dIn) {pcReg = pc'}, Fetch1, pc', False, 0)
                         -- **** Test THE ASL VARIANTS **** --
                         OTAsl -> (reg'' {pcReg = pc'}, Write1, addrReg, True, v) where
                                  (v, reg'') = asl reg dIn
                         _ -> (reg, Halt, pc', False, 0)
      AddrAbsolute -> (reg { pcReg = pc', addrReg = addr'}, Fetch3, pc', False, 0) where
                         addr' = resize dIn -- Low byte of the address
      AddrIndirect -> (reg { addrReg = zpAddr, decoded = rewriteDecoded decoded}, FetchAddr1, zpAddr, False, 0)     -- Indirect through the address - PC not updated              



run3 :: CpuRegisters -> Byte -> (CpuRegisters, State, Addr, Bool, Byte)
run3 reg@CpuRegisters{..} dIn = (reg', st, addr, wrEn, dOut) where 
  DecodedInst{..} = decoded
  pc' = pcReg + 1
  addr' = (addrReg .|. ((resize dIn :: Addr) `shiftL` 8))    -- Low read cleared out the upper byte
  addr'' = computeAddrAbs reg diAddrOffset addr'
  (reg', st, addr, wrEn, dOut) = 
    case (diAddrMode) of
      AddrIndirect -> (reg { addrReg = addr'', decoded = rewriteDecoded decoded }, FetchAddr1, addr'', False, 0)     -- Indirect through the address - PC not updated              
      _ -> case diOpType of
              OTStore -> (reg { pcReg = pc' }, Write1, addr'', True, regVal reg)
              _ ->  (reg { decoded = rewriteDecoded decoded, addrReg = addr'' }, Fetch2, addr'', False, 0)


rewriteDecoded :: DecodedInst -> DecodedInst
rewriteDecoded (DecodedInst AddrIndirect o OffsetPostAddY r) = DecodedInst AddrAbsolute  o  OffsetPreAddY  r
rewriteDecoded (DecodedInst AddrIndirect o OffsetPreAddX r) = DecodedInst AddrAbsolute o OffsetNone r
rewriteDecoded (DecodedInst AddrIndirect o OffsetPreAddY r) = DecodedInst AddrAbsolute o OffsetNone r
rewriteDecoded (DecodedInst AddrIndirect o off r) = DecodedInst AddrAbsolute o off r
-- We just run the instruction as if it had an Immediate argument after the byte has been fetched
rewriteDecoded (DecodedInst _ o _ r) = DecodedInst AddrImmediate o OffsetNone r




regVal :: CpuRegisters -> Byte
regVal CpuRegisters{..}  = v where
  DecodedInst{..} = decoded
  v = case diReg of
    RegA -> aReg
    RegX -> xReg
    RegY -> yReg



computeAddrZP :: CpuRegisters -> AddrOffset -> Byte -> Addr 
computeAddrZP reg@CpuRegisters{..} OffsetPreAddX dIn = resize (dIn + xReg)
computeAddrZP reg@CpuRegisters{..} OffsetPreAddY dIn = resize (dIn + yReg)
computeAddrZP _ _ dIn = resize dIn

-- TODO page boundary penalty
computeAddrAbs :: CpuRegisters -> AddrOffset -> Addr -> Addr 
computeAddrAbs reg@CpuRegisters{..} OffsetPreAddX addrIn = addrIn + (resize xReg)
computeAddrAbs reg@CpuRegisters{..} OffsetPreAddY addrIn = addrIn + (resize yReg)
computeAddrAbs _ _ addrIn = addrIn



adc :: CpuRegisters -> Byte -> CpuRegisters
adc regs@CpuRegisters{..} v = regs' where
  cIn = pReg .&. carryFlag
  res9 = (resize cIn :: Unsigned 9) + (resize aReg :: Unsigned 9) + (resize v :: Unsigned 9)
  cOut = resize (res9 `shiftR` 8) :: Unsigned 8
  res = resize res9  :: Unsigned 8
  overflow = if (((aReg `xor` res) .&. (v `xor` res) .&. 0x80) == 0) then 0 else ovFlag
  flags = (pReg .&. zeroNegCarryOverflowMask) .|. cOut .|. overflow .|. (setZeroAndNeg res)
  regs' = regs {aReg = res, pReg = flags}

andd :: CpuRegisters -> Byte -> CpuRegisters
andd regs@CpuRegisters{..} v = regs' where
  res = v .&. aReg
  flags = (pReg .&. zeroNegMask) .|. (setZeroAndNeg res)
  regs' = regs {aReg = res, pReg = flags}

-- Not even vaguely cycle accurate
asl :: CpuRegisters -> Byte -> (Byte, CpuRegisters)
asl regs@CpuRegisters{..} v = (res, regs') where
  res = v `shiftL` 1
  c = v `shiftR` 7
  flags = (pReg .&. zeroNegCarryMask) .|. (setZeroAndNeg res) .|. c
  regs' = regs {pReg = flags}




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


