{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module AOC1 where

import           Control.Monad.Reader
import           Data.Set
import           Text.Parsec            (digit, many1, parse, skipMany, space,
                                         string, (<|>))
import           Text.Parsec.ByteString (Parser, parseFromFile)
import           Text.Parsec.Token      (natural)

data Op
  = Plus Integer
  | Minus Integer
  deriving (Show)

newtype Frequency =
  Frequency Integer
  deriving (Show, Num, Eq, Ord)

number :: Parser Integer
number = read <$> many1 digit

parsePlus :: Parser Op
parsePlus = do
  _ <- string "+"
  Plus <$> number

parseMinus :: Parser Op
parseMinus = do
  _ <- string "-"
  Minus <$> number

parseOp = many1 ((parsePlus <|> parseMinus) <* skipMany space)

test = "+7 +7 -2 -7 -4"

foldOps :: Frequency -> [Op] -> Frequency
foldOps = Prelude.foldl apply

apply :: Frequency -> Op -> Frequency
apply f (Plus a)  = f + Frequency a
apply f (Minus a) = f - Frequency a

detectDoubleFreq :: Set Frequency -> Frequency -> [Op] -> Reader [Op] Frequency
detectDoubleFreq seen f [] = ask >>= detectDoubleFreq seen f
detectDoubleFreq seen f (op:ops) =
  let f' = foldOps f [op]
   in if member f' seen
        then pure f'
        else detectDoubleFreq (insert f' seen) f' ops

--561
solution1 :: IO Frequency
solution1 = do
  ops <- parseFromFile parseOp "AOC1.input"
  case ops of
    Right o -> pure $ foldOps (Frequency 0) o
    Left e  -> error $ show e

--563
solution2 :: IO Frequency
solution2 = do
  ops <- parseFromFile parseOp "AOC1.input"
  case ops of
    Right o -> pure $ runReader (detectDoubleFreq empty (Frequency 0) o) o
    Left e  -> error $ show e
