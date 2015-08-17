import System.Environment

import Data.List
import Data.List.Split
import Text.Printf


valuesFromString :: String -> [Int]
valuesFromString contents = bytes where
  words = filter (\x -> x /= "") $ splitOneOf " \n\t" contents
  bytes = (map (\x -> read ("0x" ++ x) :: Int) words)


vectors = [0x00, 0x02, 0x00, 0x00]
baseAddr = 0x200
romSize = 65536

romImage :: Int -> [Int] -> [Int]
romImage baseAddr imageBytes = preBytes ++ imageBytes ++ postBytes ++ vectors where 
  preBytes = (replicate baseAddr 0x00)
  bytesToPad = romSize - (length imageBytes) - baseAddr - (length vectors)
  postBytes = (replicate bytesToPad 0x00)

toBinStrings :: [Int] -> [String]
toBinStrings bytes = map (\x -> printf "%08b" x) bytes



main = do
  [f, fo] <- getArgs
  contents <- readFile f
  writeFile fo $ unlines $ toBinStrings $ romImage baseAddr (valuesFromString contents)
