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

{-# NOINLINE addrMode #-}
addrMode :: Unsigned 2 -> Unsigned 3 -> Unsigned 3 -> (AddrMode, AddrOp)
addrMode 0 0 _ = (Imm, AONone) 
addrMode 0 1 _ = (Zp, AONone) 
addrMode 0 3 3 = (AbsInd, AONone) -- JMP (ABS)
addrMode 0 3 _ = (Abs, AONone) 
addrMode 0 4 _ = (Imm, AONone)    -- Conditional Branch
addrMode 0 5 _ = (Zp, AOPreAddX) 
addrMode 0 7 _ = (Abs, AOPreAddX) 


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
           | JMP
           | STY
           | LDY
           | CPY
           | CPX
           | BCC
           | ILLEGAL
           deriving (Show, Eq)

{-# NOINLINE aluOp #-}
aluOp :: Unsigned 2 -> Unsigned 3 -> Unsigned 3 -> AluOp
aluOp 0 addrBits opBits = case addrBits of
  4 -> BCC    -- Conditional Branch is determined by the addressing mode op bits determine type
  _ -> case opBits of
         1 -> BIT
         2 -> JMP
         3 -> JMP
         4 -> STY
         5 -> LDY
         6 -> CPY
         7 -> CPX
         _ -> ILLEGAL

aluOp 1 addrBits opBits = case opBits of
  0 -> ORA
  1 -> AND
  2 -> EOR
  3 -> ADC
  4 -> case addrBits of
         2 -> BIT  -- Store Immediate is mising -- it's BIT # on 65C02
         _ -> STA
  5 -> LDA
  6 -> CMP
  7 -> SBC
  _ -> ILLEGAL
aluOp 2 addrBits opBits = case addrBits of
  0 -> case opBits of 
         5 -> LDX
         _ -> ILLEGAL
  _ -> case opBits of
         0 -> ASL
         1 -> ROL
         2 -> LSR
         3 -> ROR
         4 -> case addrBits of
                2 -> TXA
                7 -> ILLEGAL
                _ -> STX
         5 -> case addrBits of
                2 -> TAX
                _ -> LDX
         6 -> DEC
         7 -> INC
         _ -> ILLEGAL
aluOP _ _ _ = ILLEGAL




decodeInstruction :: CpuState -> Byte -> CpuState
decodeInstruction st x = st' where
  o = aluOp dm addrBits opBits
  (m, ao) = addrMode dm addrBits opBits
  st' = st{rAluOp = o, rAddrMode = m, rAddrOp = ao, rIBits = x}
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
  , rIBits :: Byte
  } deriving (Show)

initialState = CpuState Init 0xaa 0x00 0x00 0x00 0xfc 0x0000 0x0000 ADC Imm AONone 00


execNoData :: CpuState -> (CpuState, Addr)
execNoData st@CpuState{..} = (st', addr) where
  (st', addr) = (st {state = Halt}, rPC)      -- TODO


{-# NOINLINE execWithData #-}
execWithData :: CpuState -> Byte -> Addr -> (CpuState, Addr, Byte, Bool)
execWithData st@CpuState{..} v addrIn = (st', addr, oByte, wr) where
  pc' = rPC+1
  (st', addr, oByte, wr) = case rAluOp of
    STA -> (st {state = WriteByte, rPC = pc'}, addrIn, rA, True)
    STX -> (st {state = WriteByte, rPC = pc'}, addrIn, rX, True)
    STY -> (st {state = WriteByte, rPC = pc'}, addrIn, rY, True)

    LDA -> (st {state = FetchI, rA = v, rFlags = setZN rFlags v, rPC= pc'}, pc', 0, False)
    LDX -> (st {state = FetchI, rX = v, rFlags = setZN rFlags v, rPC= pc'}, pc', 0, False)
    LDY -> (st {state = FetchI, rY = v, rFlags = setZN rFlags v, rPC= pc'}, pc', 0, False)

    CMP -> (st {state = FetchI, rFlags = flags', rPC = pc'}, pc', 0, False) where
      flags' = cmp rFlags rA v
    CPX -> (st {state = FetchI, rFlags = flags', rPC = pc'}, pc', 0, False) where
      flags' = cmp rFlags rX v
    CPY -> (st {state = FetchI, rFlags = flags', rPC = pc'}, pc', 0, False) where
      flags' = cmp rFlags rY v

    EOR -> logicOp st v xor 
    ORA -> logicOp st v (.|.)
    AND -> logicOp st v (.&.)
    ADC -> (st {state = FetchI, rA = v', rFlags = flags', rPC = pc'}, pc', 0, False) where
      (v', flags) = adc rFlags rA v
      flags' = setZN flags v'
    SBC -> (st {state = FetchI, rA = v', rFlags = flags', rPC = pc'}, pc', 0, False) where
      (v', flags) = sbc rFlags rA v
      flags' = setZN flags v'

    ASL -> shiftOp st addrIn v (\x -> shiftL x 1) True
    ROL -> shiftOp st addrIn v rolFn True where
      rolFn x = (x `shiftL` 1) .|. (rFlags .&. carryFlag)  -- Shifts in from carry
    LSR -> shiftOp st addrIn v (\x -> shiftR x 1) False
    ROR -> shiftOp st addrIn v rolFn False where
      rolFn x = (x `shiftR` 1) .|. (rFlags `shiftL` 7)  -- Shifts in from carry


    TAX -> (st {state = FetchI, rX = rA, rFlags = setZN rFlags v, rPC= pc'}, pc', 0, False)
    TXA -> (st {state = FetchI, rA = rX, rFlags = setZN rFlags v, rPC= pc'}, pc', 0, False)
    DEC -> memOp st addrIn v (\x -> x-1)
    INC -> memOp st addrIn v (\x -> x+1)
    BIT -> (st {state = FetchI, rFlags = bitFlags rFlags rA v, rPC = pc'}, pc', 0, False)

    JMP -> (st {state = FetchI, rPC = addrIn}, addrIn, 0, False)
    BCC -> (st {state = FetchI, rPC = pc''}, pc'', 0, False) where
      pc'' = pc' + (bccOffset st v)


    _ -> trace (printf "Unsupported AluOp %s" (show rAluOp)) (st {state = Halt}, rPC, 0, False) 
    -- _ -> (st {state = Halt}, rPC, 0, False) 


bitFlags :: Byte -> Byte -> Byte -> Byte
bitFlags f a v = f' where
  t = a .&. v
  vF = a .&. 0x40
  f'' = setZN f t 
  f' = (f'' .&. (complement ovFlag)) .|. vF


bccOffset :: CpuState -> Byte -> Addr
bccOffset CpuState{..} v =  offset where 
  flagMask = case resize (rIBits `shiftR` 6) :: Unsigned 2 of
    0 -> negFlag
    1 -> ovFlag
    2 -> carryFlag
    3 -> zeroFlag
  compareTo = if (rIBits .&. 0x20) /= 0 then 0xff else 0x00 :: Byte
  offset = if (rFlags .&. flagMask) == (compareTo .&. flagMask) then resize v else 0 :: Addr




{-# NOINLINE memOp #-}
memOp :: CpuState -> Addr -> Byte -> (Byte -> Byte) -> (CpuState, Addr, Byte, Bool)
memOp st@CpuState{..} addrIn v fn = (st', addr, oByte, wr) where
  pc' = rPC+1
  (st', addr, oByte, wr) = case rAddrMode of
    Implicit -> (st {state = FetchI, rA = v', rFlags = setZN rFlags v', rPC = pc'}, pc', 0,  False) where
      v' = fn rA
    _ -> (st {state = WriteByte, rFlags = setZN rFlags v', rPC = pc'}, addrIn, v', True) where
      v' = fn v



{-# NOINLINE shiftOp #-}
shiftOp :: CpuState -> Addr -> Byte -> (Byte -> Byte) -> Bool -> (CpuState, Addr, Byte, Bool)
shiftOp st@CpuState{..} addrIn v fn leftShift = (st', addr, oByte, wr) where
  pc' = rPC+1
  (st', addr, oByte, wr) = case rAddrMode of
    Implicit -> (st {state = FetchI, rA = v', rFlags = flags', rPC = pc'}, pc', 0,  False) where
      (v', flags') = doShiftOp rFlags rA fn leftShift
    _ -> (st {state = WriteByte, rFlags = flags', rPC = pc'}, addrIn, v', True) where
      (v', flags') = doShiftOp rFlags v fn leftShift

{-# NOINLINE doShiftOp #-}
doShiftOp :: Byte -> Byte -> (Byte -> Byte) -> Bool -> (Byte, Byte)
doShiftOp f a fn leftShift = (v, flags) where
  v = fn a
  flags' = setZN f v
  carryBit = if leftShift then 0x80 else 0x01 :: Unsigned 8
  carry = if (a .&. carryBit) == 0 then 0 else carryFlag
  flags = (flags' .&. (complement carryFlag)) .|. carry


{-# NOINLINE logicOp #-}
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
{-# NOINLINE adcBCD #-}
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
{-# NOINLINE sbcBCD #-}
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









