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
opLdx_Imm = [p| 0xa2 |]
opLdy_Imm = [p| 0xa0 |]


opAdc_Imm = [p| 0x69 |]

opNop     = [p| 0xea |]