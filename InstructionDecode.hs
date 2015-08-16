{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module InstructionDecode where

import CLaSH.Prelude
import CLaSH.Sized.Unsigned
import qualified Data.List as L
import Types
import Opcodes

import Debug.Trace


-- Addressing mode
data AddrMode = AddrNone
              | AddrImmediate
              | AddrZeroPage
              | AddrIndirect
              | AddrAbsolute
              deriving (Show, Eq)

-- Offset calculation Type
data AddrOffset = OffsetNone
                | OffsetPreAddX
                | OffsetPreAddY
                | OffsetPostAddY
                deriving (Show)

-- Offset calculation Type
data Reg = RegNone
         | RegA
         | RegX
         | RegY
         | RegSP
           deriving (Show)

-- Read or Write? -- only valid if AddrMode != AddrNone
data OpType = OTNone
            | OTLoad
            | OTInterrupt
            | OTStore
            | OTAdc

           deriving (Show)

data DecodedInst = DecodedInst
  { diAddrMode :: AddrMode
  , diOpType :: OpType
  , diAddrOffset :: AddrOffset
  , diReg :: Reg  
  } deriving (Show)


decodedNop = decode nop

decode :: Byte -> DecodedInst
decode $opBrk     = DecodedInst AddrNone OTInterrupt OffsetNone RegNone

decode $opNop     = DecodedInst AddrNone OTNone OffsetNone RegNone 

decode $opLda_Imm = DecodedInst AddrImmediate OTLoad OffsetNone RegA 
decode $opLda_ZP  = DecodedInst AddrZeroPage OTLoad OffsetNone RegA 
decode $opLda_ZP_X  = DecodedInst AddrZeroPage OTLoad OffsetPreAddX RegA 
decode $opLda_Abs = DecodedInst AddrAbsolute OTLoad OffsetNone RegA 
decode $opLda_Abs_X = DecodedInst AddrAbsolute OTLoad OffsetPreAddX RegA 
decode $opLda_Abs_Y = DecodedInst AddrAbsolute OTLoad OffsetPreAddY RegA 

decode $opLdx_Imm = DecodedInst AddrImmediate OTLoad OffsetNone RegX 
decode $opLdx_ZP  = DecodedInst AddrZeroPage OTLoad OffsetNone RegX 
decode $opLdx_ZP_Y  = DecodedInst AddrZeroPage OTLoad OffsetPreAddY RegX 
decode $opLdx_Abs = DecodedInst AddrAbsolute OTLoad OffsetNone RegX 
decode $opLdx_Abs_Y = DecodedInst AddrAbsolute OTLoad OffsetPreAddY RegX 

decode $opLdy_Imm = DecodedInst AddrImmediate OTLoad OffsetNone RegY 
decode $opLdy_ZP  = DecodedInst AddrZeroPage OTLoad OffsetNone RegY 
decode $opLdy_ZP_X  = DecodedInst AddrZeroPage OTLoad OffsetPreAddX RegY 
decode $opLdy_Abs = DecodedInst AddrAbsolute OTLoad OffsetNone RegY 
decode $opLdy_Abs_X = DecodedInst AddrAbsolute OTLoad OffsetPreAddX RegY 


decode $opAdc_Imm = DecodedInst AddrImmediate OTAdc OffsetNone RegA
decode $opAdc_ZP  = DecodedInst AddrZeroPage OTAdc OffsetNone RegA
decode $opAdc_ZP_X  = DecodedInst AddrZeroPage OTAdc OffsetPreAddX RegA
decode $opAdc_Abs = DecodedInst AddrAbsolute OTAdc OffsetNone RegA
decode $opAdc_Abs_X = DecodedInst AddrAbsolute OTAdc OffsetPreAddX RegA
decode $opAdc_Abs_Y = DecodedInst AddrAbsolute OTAdc OffsetPreAddY RegA

decode a = DecodedInst AddrNone OTInterrupt OffsetNone RegNone
--trace ("Missing decode for " L.++ (show a)) DecodedInst AddrNone OTInterrupt OffsetNone RegNone


