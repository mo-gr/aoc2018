module Main where

import qualified AOC1
import qualified AOC2
import qualified AOC25
import qualified AOC3
import qualified AOC4
import qualified AOC5
import qualified AOC6
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
  print "Advent of Code 2018"
  print "https://adventofcode.com/2018"
  arg <- getArgs
  case safeHead arg of
    Nothing    -> putStrLn "Usage: AOC <day>" >>= const exitFailure
    Just "1"   -> format AOC1.solution1 AOC1.solution2
    Just "2"   -> format AOC2.solution1 AOC2.solution2
    Just "3"   -> format AOC3.solution1 AOC3.solution2
    Just "4"   -> format AOC4.solution1 AOC4.solution2
    Just "5"   -> format AOC5.solution1 AOC5.solution2
    Just "6"   -> format AOC6.solution1 AOC6.solution2
    Just "25"  -> AOC25.solution1 >>= print
    Just other -> putStrLn $ "No sulution for day " ++ show other
