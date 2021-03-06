module AOC8 where

import           Data.Functor
import           Data.List
import           Text.Parsec
import           Text.Parsec.ByteString (Parser, parseFromFile)

number :: Parser Int
number = read <$> many1 digit

--input = parseFromFile licenseFileParser "example.txt"
input = parseFromFile licenseFileParser "AOC8.input"

data Node = Node
  { metadata :: [Int]
  , children :: [Node]
  } deriving (Show)

emptyNode = Node [] []

headerParser :: Parser (Int, Int)
headerParser = do
  childCount <- number <* many space
  metadataCount <- number <* many space
  pure (childCount, metadataCount)

metaParser :: Parser Int
metaParser = number <* many space

nodeParser :: Parser Node
nodeParser = do
  (childCount, metaCount) <- headerParser <* many space
  children <- count childCount nodeParser <* many space
  metadata <- count metaCount metaParser <* many space
  pure $ Node metadata children

licenseFileParser :: Parser Node
licenseFileParser = nodeParser

sumOfMeta :: Node -> Int
sumOfMeta (Node m []) = sum m
sumOfMeta (Node m c)  = sum m + sum (sumOfMeta <$> c)

value :: Node -> Int
value (Node m []) = sum m
value (Node m c) = sum $ findReferenced <$> m
  where
    findReferenced :: Int -> Int
    findReferenced m =
      sum $ value . snd <$> filter ((== m) . fst) (zip [1 ..] c)

--36627
solution1 = do
  parseResult <- input
  license <-
    case parseResult of
      Left e  -> print e Data.Functor.$> emptyNode
      Right l -> pure l
  pure $ sumOfMeta license

--16695
solution2 = do
  parseResult <- input
  license <-
    case parseResult of
      Left e  -> print e Data.Functor.$> emptyNode
      Right l -> pure l
  pure $ value license
