module Main where

import qualified AOC25
import           System.Environment
import           System.Exit

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (a:_) = Just a

main = do
  print "Advent of Code"
  arg <- getArgs
  case safeHead arg of
    Nothing    -> putStrLn "Usage: AOC <day>" >>= const exitFailure
    Just "25"  -> AOC25.solution1 >>= print
    Just other -> putStrLn $ "No sulution for day " ++ show other
