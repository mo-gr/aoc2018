module Main where

import qualified AOC1
import qualified AOC2
import qualified AOC25
import           System.Environment
import           System.Exit

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (a:_) = Just a

format s1 s2 = do
  putStrLn "Solution 1:"
  s1 >>= print
  putStrLn "Solution 2:"
  s2 >>= print

main = do
  print "Advent of Code"
  arg <- getArgs
  case safeHead arg of
    Nothing    -> putStrLn "Usage: AOC <day>" >>= const exitFailure
    Just "1"   -> format AOC1.solution1 AOC1.solution2
    Just "2"   -> format AOC2.solution1 AOC2.solution2
    Just "25"  -> AOC25.solution1 >>= print
    Just other -> putStrLn $ "No sulution for day " ++ show other
