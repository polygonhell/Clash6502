{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Decode where

import CLaSH.Prelude
import CLaSH.Sized.Unsigned
import qualified Data.List as L
import Text.Printf