{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Alu where

import CLaSH.Prelude
import CLaSH.Sized.Unsigned

import Types

data AddrMode = Zp
              | Abs
              | Imm
              | ZpInd
              | AbsInd
              | Implicit
              deriving (Show, Eq)

data AddrOp = AONone
            | AOPreAddX
            | AOPreAddY
            | AOPostAddY
            deriving (Show, Eq)


addrMode :: Unsigned 2 -> Unsigned 3 -> Unsigned 3-> (AddrMode, AddrOp)
addrMode 1 0 _ = (ZpInd, AOPreAddX)
addrMode 1 1 _ = (Zp, AONone)
addrMode 1 2 _ = (Imm, AONone)
addrMode 1 3 _ = (Abs, AONone)
addrMode 1 4 _ = (ZpInd, AOPostAddY)
addrMode 1 5 _ = (Zp, AOPreAddX)
addrMode 1 6 _ = (Abs, AOPreAddY)
addrMode 1 7 _ = (Abs, AOPreAddX)


data AluOp = ORA
           | AND
           | EOR
           | ADC
           | STA
           | LDA
           | CMP
           | SBC
           deriving (Show, Eq)

aluOp :: Unsigned 2 -> Unsigned 3 -> AluOp
aluOp 1 0 = ORA
aluOp 1 1 = AND
aluOp 1 2 = EOR
aluOp 1 3 = ADC
aluOp 1 4 = STA
aluOp 1 5 = LDA
aluOp 1 6 = CMP
aluOp 1 7 = SBC


decodeInstruction :: CpuState -> Byte -> CpuState
decodeInstruction st x = st' where
  o = aluOp dm opBits
  (m, ao) = addrMode dm addrBits opBits
  st' = st{rAluOp = o, rAddrMode = m, rAddrOp = ao}
  dm = resize x
  opBits = resize (x `shiftR` 5)
  addrBits = resize (x `shiftR` 2)



data State =  Halt
            | Init
            | WaitPCL
            | WaitPCH 
            | FetchI 
            | FetchL
            | FetchH
            | ReadAddr
            | WriteByte
            deriving (Show)


negFlag   = 0x80 :: Byte
ovFlag    = 0x40 :: Byte
decFlag   = 0x08 :: Byte
intFlag   = 0x04 :: Byte
zeroFlag  = 0x02 :: Byte
carryFlag = 0x01 :: Byte     

data CpuState = CpuState
  { state :: State
  , rA :: Byte
  , rX :: Byte    
  , rY :: Byte
  , rFlags :: Byte
  , rSp :: Byte
  , rPC :: Addr
  , rAddr :: Addr   -- Used for Indirect addressing 
  -- Need some thing for current addressing mode and pending adjustment
  , rAluOp :: AluOp
  , rAddrMode :: AddrMode
  , rAddrOp :: AddrOp
  } deriving (Show)

initialState = CpuState Init 0xaa 0x00 0x00 0x00 0xfc 0x0000 0x0000 ADC Imm AONone


execNoData :: CpuState -> (CpuState, Addr)
execNoData st@CpuState{..} = (st', addr) where
  (st', addr) = (st {state = Halt}, rPC)      -- TODO


execWithData :: CpuState -> Byte -> Addr -> (CpuState, Addr, Byte, Bool)
execWithData st@CpuState{..} v addrIn = (st', addr, oByte, wr) where
  pc' = rPC+1
  zn = setZN rFlags v
  (st', addr, oByte, wr) = case rAluOp of
    LDA -> (st {state = FetchI, rA = v, rFlags = zn, rPC= pc'}, pc', 0, False)
    _ -> (st {state = Halt}, rPC, 0, False) 

setZN :: Byte -> Byte -> Byte
setZN f v = (f .&. (complement (negFlag .|. zeroFlag))) .|. z .|. n where
  n = v .&. negFlag
  z = if v == 0 then zeroFlag else 0

