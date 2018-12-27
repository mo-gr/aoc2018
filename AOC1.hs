{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module AOC1 where

import Text.Parsec.ByteString (Parser, parseFromFile)  
import Text.Parsec ((<|>), string, parse, many1, digit, skipMany, space)
import Text.Parsec.Token (natural)
import Data.Set
import Control.Monad.Reader
  
data Op = Plus Integer | Minus Integer
  deriving (Show)

newtype Frequency = Frequency Integer deriving (Show, Num, Eq, Ord)

number :: Parser Integer
number = read <$> many1 digit

parsePlus :: Parser Op
parsePlus = do
  _ <- string "+"
  x <- number
  pure $ Plus x
  
parseMinus :: Parser Op
parseMinus = do
  _ <- string "-"
  x <- number
  pure $ Minus x
  
parseOp = many1 ((parsePlus <|> parseMinus) <* (skipMany space))

test = "+7 +7 -2 -7 -4"

foldOps :: Frequency -> [Op] -> Frequency
foldOps f [] = f
foldOps f (op:ops) = foldOps (apply f op) ops

apply :: Frequency -> Op -> Frequency
apply f (Plus a) = f + (Frequency a)
apply f (Minus a) = f - (Frequency a)

detectDoubleFreq :: Set Frequency -> Frequency -> [Op] -> Reader [Op] Frequency
detectDoubleFreq seen f [] = ask >>= detectDoubleFreq seen f
detectDoubleFreq seen f (op:ops) = 
  let f' = foldOps f [op] in
    case (member f' seen) of
      True -> pure f'
      False -> detectDoubleFreq (insert f' seen) f' ops

main = do
  ops <- parseFromFile parseOp "AOC1.input"
  f <- pure $ case ops of
         Right o -> runReader (detectDoubleFreq empty (Frequency 0) o) o
         Left _ -> Frequency 0
  pure f
  
