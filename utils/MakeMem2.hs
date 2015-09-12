import System.Environment

import Data.List
import Data.List.Split
import Text.Printf
import qualified Data.ByteString as B
import Data.Word
import System.IO




toBinStrings :: B.ByteString -> [String]
toBinStrings bytes = map (\x -> printf "%08b" x) (B.unpack bytes)



main = do
  [f, fo] <- getArgs
  h <- openFile f ReadMode
  contents <- B.hGetContents h
  writeFile fo $ unlines $ toBinStrings contents
