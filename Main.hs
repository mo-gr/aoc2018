module Main where

import qualified AOC1
import qualified AOC12
import qualified AOC14
import qualified AOC16
import qualified AOC18
import qualified AOC19
import qualified AOC2
import qualified AOC24
import qualified AOC25
import qualified AOC3
import qualified AOC4
import qualified AOC5
import qualified AOC6
import qualified AOC7
import qualified AOC8
import qualified AOC9
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
    Nothing -> putStrLn "Usage: AOC <day>" >>= const exitFailure
    Just "1" -> format AOC1.solution1 AOC1.solution2
    Just "2" -> format AOC2.solution1 AOC2.solution2
    Just "3" -> format AOC3.solution1 AOC3.solution2
    Just "4" -> format AOC4.solution1 AOC4.solution2
    Just "5" -> format AOC5.solution1 AOC5.solution2
    Just "6" -> format AOC6.solution1 AOC6.solution2
    Just "7" -> format AOC7.solution1 AOC7.solution2
    Just "8" -> format AOC8.solution1 AOC8.solution2
    Just "9" -> format AOC9.solution1 AOC9.solution2
    Just "10" ->
      putStrLn
        "This solution uses SpriteKit and thus requires Haskell for Mac (http://haskellformac.com). Just put the source and input in a new project and write 'solution1' or 'solution2' in the playground."
    Just "12" -> format AOC12.solution1 (pure "not solved")
    Just "14" -> format AOC14.solution1 (pure "not solved")
    Just "16" -> format AOC16.solution1 AOC16.solution2
    Just "18" -> format AOC18.solution1 (pure "not solved")
    Just "19" -> format AOC19.solution1 (pure "not solved")
    Just "24" -> format AOC24.solution1 AOC24.solution2
    Just "25" -> AOC25.solution1 >>= print
    Just other -> putStrLn $ "No sulution for day " ++ show other
