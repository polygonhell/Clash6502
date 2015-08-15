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
            | OTAdd

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

decode a = trace ("Missing decode for " L.++ (show a)) DecodedInst AddrNone OTInterrupt OffsetNone RegNone


