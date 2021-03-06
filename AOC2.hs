module AOC2 where

import           Data.List
import           Data.Maybe
import           Text.Parsec
import           Text.Parsec.ByteString (Parser, parseFromFile)

inputParser :: Parser [String]
inputParser = many (many1 letter <* skipMany space)

input = do
  x <- parseFromFile inputParser "AOC2.input"
  pure $
    case x of
      Left _  -> []
      Right x -> x

test = ["abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab"]

nSet :: Int -> String -> Bool
nSet _ [] = False
nSet n (x:xs) =
  case length $ filter (== x) xs of
    c
      | c == (n - 1) -> True
    _ -> nSet n $ filter (/= x) xs

isDouble :: String -> Bool
isDouble = nSet 2

isTriple :: String -> Bool
isTriple = nSet 3

checksum :: [String] -> Int
checksum input =
  let doubles = length $ filter isDouble input
      triples = length $ filter isTriple input
   in doubles * triples

-- 6225
solution1 = checksum <$> input

difference :: String -> String -> Int
difference = difference' 0
  where
    difference' d [] other = d + length other
    difference' d other [] = d + length other
    difference' d (x:xs) (y:ys) =
      if x == y
        then difference' d xs ys
        else difference' (d + 1) xs ys

leastDiff :: [String] -> Maybe (String, String)
leastDiff [] = Nothing
leastDiff (x:xs) =
  case filter (\(x', d) -> d == 1) $ fmap (\x' -> (x', difference x x')) xs of
    []        -> leastDiff xs
    (x', _):_ -> Just (x, x')

removeDiff :: (String, String) -> String
removeDiff (x, y) = map fst $ filter (uncurry (==)) $ zip x y

-- revtaubfniyhsgxdoajwkqilp
solution2 = do
  x <- leastDiff <$> input
  pure $ removeDiff $ fromMaybe undefined x
