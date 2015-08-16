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

opLdx_Imm = [p| 0xa2 |]
opLdx_ZP  = [p| 0xa6 |]

opLdy_Imm = [p| 0xa0 |]
opLdy_ZP  = [p| 0xa4 |]


opAdc_Imm = [p| 0x69 |]
opAdc_ZP  = [p| 0x65 |]

opNop     = [p| 0xea |]
