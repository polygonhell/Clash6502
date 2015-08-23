{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Alu where

import CLaSH.Prelude
import CLaSH.Sized.Unsigned
import Debug.Trace
import Text.Printf

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


addrMode :: Unsigned 2 -> Unsigned 3 -> Unsigned 3 -> (AddrMode, AddrOp)
addrMode 1 0 _ = (ZpInd, AOPreAddX)
addrMode 1 1 _ = (Zp, AONone)
addrMode 1 2 _ = (Imm, AONone)
addrMode 1 3 _ = (Abs, AONone)
addrMode 1 4 _ = (ZpInd, AOPostAddY)
addrMode 1 5 _ = (Zp, AOPreAddX)
addrMode 1 6 _ = (Abs, AOPreAddY)
addrMode 1 7 _ = (Abs, AOPreAddX)

addrMode 2 0 5 = (Imm, AONone)
addrMode 2 1 _ = (Zp, AONone)

addrMode 2 2 _ = (Implicit, AONone)

addrMode 2 3 4 = (Implicit, AONone) -- Not defined in this group
addrMode 2 3 _ = (Abs, AONone)
-- addrMode 2 4 _ = (Zp, AONone)
addrMode 2 5 _ = (Zp, AOPreAddX)
-- addrMode 2 6 _ = (Zp, AOPreAddX)
addrMode 2 7 _ = (Abs, AOPreAddX)

addrMode _ _ _ = (Implicit, AONone)


data AluOp = ORA
           | AND
           | EOR
           | ADC
           | STA
           | LDA
           | CMP
           | SBC
           | BIT
           | ASL
           | ROL
           | LSR
           | ROR
           | STX
           | LDX
           | DEC
           | INC
           | TXA
           | TAX
           | ILLEGAL
           deriving (Show, Eq)

aluOp :: Unsigned 2 -> Unsigned 3 -> Unsigned 3 -> AluOp
aluOp 1 _ 0 = ORA
aluOp 1 _ 1 = AND
aluOp 1 _ 2 = EOR
aluOp 1 _ 3 = ADC
aluOp 1 2 4 = BIT  -- Store Immediate is mising -- it's BIT # on 65C02
aluOp 1 _ 4 = STA
aluOp 1 _ 5 = LDA
aluOp 1 _ 6 = CMP
aluOp 1 _ 7 = SBC

aluOp 2 0 0 = ILLEGAL
aluOp 2 _ 0 = ASL
aluOp 2 0 1 = ILLEGAL
aluOp 2 _ 1 = ROL
aluOp 2 0 2 = ILLEGAL
aluOp 2 _ 2 = LSR
aluOp 2 0 3 = ILLEGAL
aluOp 2 _ 3 = ROR
aluOp 2 0 4 = ILLEGAL
aluOp 2 2 4 = TXA
aluOp 2 7 4 = ILLEGAL
aluOp 2 _ 4 = STX
aluOp 2 2 5 = TAX
aluOp 2 _ 5 = LDX
aluOp 2 0 6 = ILLEGAL
aluOp 2 _ 6 = DEC
aluOp 2 0 7 = ILLEGAL
aluOp 2 _ 7 = INC

aluOp _ _ _ = ILLEGAL


decodeInstruction :: CpuState -> Byte -> CpuState
decodeInstruction st x = st' where
  o = aluOp dm addrBits opBits
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
  -- The "decoded" instruction, is modified by the state machine as the address modes are "unwound"
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
  (st', addr, oByte, wr) = case rAluOp of
    STA -> (st {state = WriteByte, rPC = pc'}, addrIn, rA, True)
    LDA -> (st {state = FetchI, rA = v, rFlags = setZN rFlags v, rPC= pc'}, pc', 0, False)
    EOR -> logicOp st v xor 
    ORA -> logicOp st v (.|.)
    AND -> logicOp st v (.&.)
    ADC -> (st {state = FetchI, rA = v', rFlags = flags', rPC = pc'}, pc', 0, False) where
      (v', flags) = adc rFlags rA v
      flags' = setZN flags v'
    SBC -> (st {state = FetchI, rA = v', rFlags = flags', rPC = pc'}, pc', 0, False) where
      (v', flags) = sbc rFlags rA v
      flags' = setZN flags v'
    CMP -> (st {state = FetchI, rFlags = flags', rPC = pc'}, pc', 0, False) where
      flags' = cmp rFlags rA v



    -- _ -> trace (printf "Unsupported AluOp %s" (show rAluOp)) (st {state = Halt}, rPC, 0, False) 
    _ -> (st {state = Halt}, rPC, 0, False) 

logicOp :: CpuState -> Byte -> (Byte -> Byte -> Byte) -> (CpuState, Addr, Byte, Bool)
logicOp st@CpuState{..} v fn = (st {state = FetchI, rA = v', rFlags = flags, rPC = pc'}, pc', 0, False) where
    pc' = rPC+1
    v' = fn rA v 
    flags = setZN rFlags v'

setZN :: Byte -> Byte -> Byte
setZN f v = (f .&. (complement (negFlag .|. zeroFlag))) .|. z .|. n where
  n = v .&. negFlag
  z = if v == 0 then zeroFlag else 0

-- Do the add set the OV and C flags
adc :: Byte -> Byte -> Byte -> (Byte, Byte)
adc flags a b  = (v, flags') where
  (v, flags') = if (flags .&. decFlag == 0) then
    adcNorm flags a b
  else
    adcBCD flags a b

adcNorm :: Byte -> Byte -> Byte -> (Byte, Byte)
adcNorm flags a b = (res, flags') where
    cIn = flags .&. carryFlag
    res9 = (resize cIn :: Unsigned 9) + (resize a :: Unsigned 9) + (resize b :: Unsigned 9)
    res = resize res9
    cOut = resize (res9 `shiftR` 8) :: Unsigned 8
    overflow = if (((a `xor` res) .&. (b `xor` res) .&. 0x80) == 0) then 0 else ovFlag
    flags' = (flags .&. (complement (ovFlag .|. carryFlag))) .|. overflow .|. cOut

-- TODO need to test BCD implementation
adcBCD :: Byte -> Byte -> Byte -> (Byte, Byte)
adcBCD flags a b = (res, flags') where
    cIn = resize (flags .&. carryFlag) :: Unsigned 5
    lowO = (resize a :: Unsigned 5) + (resize b :: Unsigned 5) + cIn
    (lowCout, lowO') = if lowO > 9 then (1, lowO + 6) else (0, lowO) :: (Unsigned 5, Unsigned 5)

    highO = (resize (a `shiftR` 4) :: Unsigned 5) + (resize (b `shiftR` 4) :: Unsigned 5) + lowCout
    (highCout, highO') = if highO > 9 then (carryFlag, highO + 6) else (0, highO)
    res = ((resize highO' :: Unsigned 8) `shiftL` 4) .|. (resize lowO' :: Unsigned 8)
    -- Overflow not documented for the original 6502, but basically set as if for the last nibble calculation in standard ADC case
    highOO = (resize highO :: Unsigned 8) `shiftL` 4
    overflow = if ((a `xor` highOO) .&. (b `xor` highOO) .&. 0x80) == 0 then 0 else ovFlag
    flags' = (flags .&. (complement (ovFlag .|. carryFlag))) .|. overflow .|. highCout


-- Do the sub set the OV and C flags
sbc :: Byte -> Byte -> Byte -> (Byte, Byte)
sbc flags a b  = (v, flags') where
  (v, flags') = if (flags .&. decFlag == 0) then
    sbcNorm flags a b
  else
    sbcBCD flags a b

sbcNorm :: Byte -> Byte -> Byte -> (Byte, Byte)
sbcNorm flags a b = (res, flags') where
    cIn = (complement flags) .&. carryFlag -- SBC needs the inverted carry
    res9 = (resize a :: Unsigned 9) - (resize b :: Unsigned 9) - (resize cIn :: Unsigned 9)
    res = resize res9
    cOut = resize (res9 `shiftR` 8) :: Unsigned 8
    overflow = if (((a `xor` res) .&. (b `xor` res) .&. 0x80) == 0) then 0 else ovFlag
    flags' = (flags .&. (complement (ovFlag .|. carryFlag))) .|. overflow .|. cOut


-- TODO need to test BCD implementation
sbcBCD :: Byte -> Byte -> Byte -> (Byte, Byte)
sbcBCD flags a b = (res, flags') where
  cIn = resize ((complement flags) .&. carryFlag) :: Unsigned 5
  lowO = (resize a :: Unsigned 5) - (resize b :: Unsigned 5) - cIn
  (lowCout, lowO') = if lowO > 9 then (0, lowO - 6) else (1, lowO) :: (Unsigned 5, Unsigned 5) --  Inverted carry

  highO = (resize (a `shiftR` 4) :: Unsigned 5) - (resize (b `shiftR` 4) :: Unsigned 5) - lowCout
  (highCout, highO') = if highO > 9 then (carryFlag, highO - 6) else (0, highO) -- correct Carry
  res = ((resize highO' :: Unsigned 8) `shiftL` 4) .|. (resize lowO' :: Unsigned 8)
  -- Overflow not documented for the original 6502, but basically set as if for the last nibble calculation in standard SBC case
  highOO = (resize highO :: Unsigned 8) `shiftL` 4
  overflow = if ((a `xor` highOO) .&. (b `xor` highOO) .&. 0x80) == 0 then 0 else ovFlag
  flags' = (flags .&. (complement (ovFlag .|. carryFlag))) .|. overflow .|. highCout



cmp :: Byte -> Byte -> Byte -> Byte
cmp flags a b = flags' where
  t = a - b
  neg = t .&. 0x80
  c = if a >= b then carryFlag else 0
  z = if t == 0 then zeroFlag else 0
  flags' = (flags .&. (complement (carryFlag .|. negFlag .|. zeroFlag))) .|. neg .|. c .|. z










