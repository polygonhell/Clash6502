{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}


module Cpu2 (cpuM, CpuOut(..), CpuIn(..), Probes(..))where 

import CLaSH.Prelude
import CLaSH.Sized.Unsigned
import qualified Data.List as L
import Text.Printf

import Alu
import Types

data Probes = Probes
  { prStateIn :: State
  , prState :: State
  , prPC :: Addr
  , prA :: Byte
  , prX :: Byte
  , prY :: Byte
  , prFlags :: Byte

  , prAddr :: Addr 
  , prDIn :: Byte
  , prAddrOut :: Addr 
  , prDOut :: Byte
  , prWrEn :: Bool
  }

instance Show Probes where
  show a = str where 
    -- str = "Hello"
    Probes{..} = a
    str = (printf "%-8s" (show prStateIn)) L.++ " " L.++ 
          (printf "%-8s" (show prState)) L.++ " " L.++ 
          (printf "%04x" (toInteger prPC)) L.++ " " L.++ 
          (printf "%02x" (toInteger prA)) L.++ " " L.++ 
          (printf "%02x" (toInteger prX)) L.++ " " L.++ 
          (printf "%02x" (toInteger prY)) L.++ " " L.++ 
          (pRegString prFlags) L.++ " " L.++ 
          (printf "%04x" (toInteger prAddr)) L.++ " " L.++ 
          (printf "%02x" (toInteger prDIn)) L.++ " " L.++ 
          (printf "%04x" (toInteger prAddrOut)) L.++ " " L.++ 
          (printf "%02x" (toInteger prDOut)) L.++ " " L.++ 
          (show prWrEn) 


pRegString :: Byte -> String
pRegString v = res where
  c = if (v .&. carryFlag) /= 0 then "C" else "c" 
  z = if (v .&. zeroFlag) /= 0 then "Z" else "z" 
  d = if (v .&. decFlag) /= 0 then "D" else "d" 
  o = if (v .&. ovFlag) /= 0 then "O" else "o" 
  n = if (v .&. negFlag) /= 0 then "N" else "n" 
  res = n L.++ o L.++ d L.++ z L.++ c 



data CpuIn = CpuIn 
  { dIn :: Byte
  } deriving (Show)

data CpuOut = CpuOut 
  { dOut :: Byte
  , addr :: Addr
  , writeEn :: Bool
  } deriving (Show)


resetVec :: Addr
resetVec = 0xfffc


cpuM = cpu `mealy` initialState

{-# NOINLINE cpu #-}
cpu :: CpuState -> CpuIn -> (CpuState, (CpuOut, Probes))
cpu st@CpuState{..} CpuIn{..} = (st', (out, probes)) where
  state' = getState st'
  probes = Probes state state' rPC rA rX rY rFlags rAddr dIn (addr out) (dOut out) (writeEn out)

  (st', out) = case state of
    Halt -> (st, CpuOut 0 0 False)
    Init -> (st { state = WaitPCL, rAddr = resetVec }, CpuOut 0 resetVec False)
    WaitPCL -> (st { state = WaitPCH, rPC = resize dIn}, CpuOut 0 (rAddr + 1)  False)
    WaitPCH -> (st { state = FetchI, rPC = pc' }, CpuOut 0 pc' False) where
                 pc' = rPC + ((resize dIn :: Addr) `shiftL` 8)

    -- Read and decode the instruction - execute single byte instructions
    -- PC always advanced             
    FetchI -> (st'', CpuOut oByte addr wr) where
                pc' = rPC+1
                stp = decodeInstruction st dIn
                (st'', addr, oByte, wr) = case getAddrMode stp of
                  Implicit -> execWithData stp 0 0
                  _ -> (stp {rPC = pc', state = FetchL }, pc', 0, False)

    -- Low data byte ready
    -- Can execute anything that doesn't require indirection through that byte
    FetchL -> (st'', CpuOut oByte addr wr) where
                pc' = rPC + 1 
                cAddr = computeAddress st dIn
                (m, ao) = newAddrMode rAddrMode rAddrOp
                (st'', addr, oByte, wr)  
                  | canExecute rAluOp rAddrMode = execWithData st dIn cAddr
                  -- Some indirection required -- pc not incremented, until instruction executed
                  -- just read the byte from the supplied address and rerun this state as if it were Immediate
                  | rAddrMode == Zp = (st {state = FetchL, rAddrMode = Imm, rAddr = cAddr}, cAddr, 0, False)
                  -- Read the 16 bit address
                  | rAddrMode == ZpInd = (st {state = ReadAddr, rAddr = cAddr, rAddrMode = m, rAddrOp = ao}, cAddr, 0, False)                                           
                  -- Have to fetch a 3rd instructionByte -- Store low byte of final 16 bit value in addr
                  | otherwise = (st{state = FetchH, rAddr = resize dIn, rPC = pc'}, pc', 0, False)
                canExecute :: AluOp -> AddrMode -> Bool
                canExecute _ Imm = True
                canExecute _ Implicit = True
                canExecute a Zp = if writesToAddress a then True else False
                canExecute _ _ = False

    FetchH -> (st'', CpuOut oByte addr wr) where
                base = ((resize dIn) `shiftL` 8) .|. (rAddr)
                cAddr = computeAddress st{rAddr = base} dIn
                (m, ao) = newAddrMode rAddrMode rAddrOp
                (st'', addr, oByte, wr)  
                  | canExecute rAluOp rAddrMode = execWithData st dIn cAddr
                  -- Need to indirect Addr mode must be ABS or ABSInd at this point
                  | rAddrMode == Abs = (st {state = FetchL, rAddrMode = Imm, rAddr = cAddr}, cAddr, 0, False)
                  -- AbsInd only happens for JSR
                  | otherwise = (st {state = ReadAddr, rAddr = cAddr, rAddrMode = m, rAddrOp = ao}, cAddr, 0, False)

                canExecute :: AluOp -> AddrMode -> Bool
                canExecute _ Imm = True
                canExecute _ Implicit = True
                canExecute a Abs = if writesToAddress a then True else False
                canExecute _ _ = False

    ReadAddr -> (st'', CpuOut 0 addr' False) where
                  addr' = rAddr + 1
                  st'' = st{state = FetchH, rAddr = resize dIn}

    -- This is just a delay state for the write to occur on the bus before issuing an instruction read
    WriteByte -> (st'', CpuOut 0 rPC False) where
                  st'' = st{state = FetchI}


newAddrMode :: AddrMode -> AddrOp -> (AddrMode, AddrOp)
newAddrMode ZpInd AOPostAddY = (Abs, AOPreAddY)
newAddrMode _ _ = (Abs, AONone)



computeAddress :: CpuState -> Byte -> Addr
computeAddress st@CpuState{..} dIn = case rAddrMode of
  Zp -> resize $ addressCalc st dIn
  ZpInd -> resize $ addressCalc st dIn
  Abs -> addressCalc st rAddr
  AbsInd -> addressCalc st rAddr
  _ -> rAddr

addressCalc :: forall n . (KnownNat n) => CpuState -> Unsigned n -> Unsigned n
addressCalc CpuState{..} base = case rAddrOp of
  AOPreAddX -> base + (resize rX)
  AOPreAddY -> base + (resize rY)
  _ -> base



writesToAddress :: AluOp -> Bool
writesToAddress STA = True
writesToAddress STX = True
writesToAddress _ = False


getAddr :: CpuState -> Addr
getAddr st = rAddr st

getAddrMode :: CpuState -> AddrMode
getAddrMode st = rAddrMode st

getState :: CpuState -> State
getState st = state st


