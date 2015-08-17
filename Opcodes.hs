{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Opcodes where

import CLaSH.Prelude
import CLaSH.Sized.Unsigned

import Types

nop = 0xea

opBrk     = [p| 0x00 |]

opLda_Imm = [p| 0xa9 |]
opLda_ZP  = [p| 0xa5 |]
opLda_ZP_X = [p| 0xb5 |]
opLda_Abs = [p| 0xad |]
opLda_Abs_X = [p| 0xbd |]
opLda_Abs_Y = [p| 0xb9 |]

opLdx_Imm = [p| 0xa2 |]
opLdx_ZP  = [p| 0xa6 |]
opLdx_ZP_Y  = [p| 0xb6 |]
opLdx_Abs  = [p| 0xae |]
opLdx_Abs_Y  = [p| 0xbe |]

opLdy_Imm = [p| 0xa0 |]
opLdy_ZP  = [p| 0xa4 |]
opLdy_ZP_X = [p| 0xb4 |]
opLdy_Abs = [p| 0xac |]
opLdy_Abs_X = [p| 0xbc |]


opSta_ZP = [p| 0x85 |]
opSta_Abs = [p| 0x8d |]


opAdc_Imm = [p| 0x69 |]
opAdc_ZP  = [p| 0x65 |]
opAdc_ZP_X  = [p| 0x75 |]
opAdc_Abs  = [p| 0x6d |]
opAdc_Abs_X  = [p| 0x7d |]
opAdc_Abs_Y  = [p| 0x79 |]

opNop     = [p| 0xea |]
