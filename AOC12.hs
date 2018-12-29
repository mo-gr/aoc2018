{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AOC12 where

import           Data.Functor
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Text.Parsec
import           Text.Parsec.ByteString (Parser, parseFromFile)

--input = fromRight mempty <$> parseFromFile (inputParser) "example.txt"
input = fromRight mempty <$> parseFromFile inputParser "AOC12.input"

fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _         = b

data PlantState
  = Alive
  | Dead
  deriving (Eq)

instance Show PlantState where
  show Alive = "#"
  show Dead  = "."

newtype Plants = Plants
  { unPlants :: [PlantState]
  } deriving (Eq, Monoid, Semigroup)

instance Show Plants where
  show (Plants state) = mconcat $ show <$> state

data Rule = Rule
  { constellation :: [PlantState]
  , result        :: PlantState
  } deriving (Show, Eq)

aliveParser :: Parser PlantState
aliveParser = string "#" Data.Functor.$> Alive

deadParser :: Parser PlantState
deadParser = string "." Data.Functor.$> Dead

plantState :: Parser PlantState
plantState = aliveParser <|> deadParser

plantParser :: Parser Plants
plantParser = do
  _ <- string "initial state:" <* many space
  Plants <$> many plantState

ruleParser :: Parser Rule
ruleParser = do
  constellation <- count 5 plantState <* many space
  _ <- string "=>" <* many space
  result <- plantState <* many space
  return $ Rule constellation result

inputParser :: Parser (Plants, [Rule])
inputParser = do
  initialState <- plantParser <* many space
  rules <- many1 ruleParser
  return (initialState, rules)

rightPad :: [PlantState] -> [PlantState]
rightPad p = p <> repeat Dead

tick :: [Rule] -> Plants -> Plants
tick rules (Plants plants) =
  Plants $
  take (length plants) $
  [Dead, Dead] <>
  (applyRules rules <$>
   zip5
     plants
     (drop 1 $ rightPad plants)
     (drop 2 $ rightPad plants)
     (drop 3 $ rightPad plants)
     (drop 4 $ rightPad plants))

applyRules ::
     [Rule]
  -> (PlantState, PlantState, PlantState, PlantState, PlantState)
  -> PlantState
applyRules rules (a, b, c, d, e) =
  maybe Dead result $ find ((== [a, b, c, d, e]) . constellation) rules

plantValue :: Plants -> Int
plantValue (Plants plants) =
  sum $
  fst <$> filter ((== Alive) . snd) (zip [(paddingCount * (-1)) ..] plants)

paddingCount = 20

generations = 20

--generations = 50000000000
--3494
solution1 = do
  (initial, rules) <- input
  let padding = Plants $ replicate paddingCount Dead
  let world = padding <> initial <> padding
  let live = iterate (tick rules) world
  --sequence $ putStrLn.show <$> zip [0..] (take 21 $ live)
  pure $ plantValue . head $ drop generations live
