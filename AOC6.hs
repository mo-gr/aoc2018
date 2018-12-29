{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AOC6 where

import           Data.List
import           Data.Maybe
import           Data.Set               (fromList, notMember)
import           Text.Parsec
import           Text.Parsec.ByteString (Parser, parseFromFile)

newtype X = X
  { unX :: Int
  } deriving (Show, Eq, Num, Ord, Enum)

newtype Y = Y
  { unY :: Int
  } deriving (Show, Eq, Num, Ord, Enum)

data Coord = Coord
  { x :: X
  , y :: Y
  } deriving (Eq)

instance Show Coord where
  show (Coord (X x) (Y y)) = show x ++ "x" ++ show y

instance Ord Coord where
  compare (Coord (X x) (Y y)) (Coord (X x') (Y y')) =
    mappend (compare x x') (compare y y')

number :: Parser Int
number = read <$> many1 digit

coordParser :: Parser Coord
coordParser = do
  x <- number <* string "," <* many space
  y <- number <* many space
  pure $ Coord (X x) (Y y)

fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _         = b

--input = fromRight mempty <$> parseFromFile (many coordParser) "example.txt"
input = fromRight mempty <$> parseFromFile (many coordParser) "AOC6.input"

distance :: Coord -> Coord -> Int
distance (Coord x y) (Coord x' y') = unX (abs (x - x')) + unY (abs (y - y'))

zero = pure $ Coord (X 0) (Y 0)

finity = do
  x <- maximum <$> (fmap . fmap) x input
  y <- maximum <$> (fmap . fmap) y input
  pure $ Coord x y

infinity = do
  zero' <- zero
  finity' <- finity
  world' <- fromList <$> world
  pure
    [ Coord x' y'
    | x' <- [(x zero' - 1) .. (x finity' + 1)]
    , y' <- [(y zero' - 1) .. (y finity' + 1)]
    , notMember (Coord x' y') world'
    ]

world = do
  zero' <- zero
  finity' <- finity
  pure
    [ Coord x' y'
    | x' <- [(x zero') .. (x finity')]
    , y' <- [(y zero') .. (y finity')]
    ]

findClosest :: [Coord] -> Coord -> Maybe Coord
findClosest candidates point =
  firstOrNothing $ sortOn snd $ zip candidates $ distance point <$> candidates
  where
    firstOrNothing ((_, x):((_, y):more))
      | x == y = Nothing
    firstOrNothing ((c, x):more) = Just c

--findDistSum :: [Coord] -> Coord -> Int
findDistSum candidates point = sum $ distance point <$> candidates
  where
    firstOrNothing ((_, x):((_, y):more))
      | x == y = Nothing
    firstOrNothing ((c, x):more) = Just c

--3969
solution1 = do
  coords <- input
  w <- world
  outside <- infinity
  let closest = fmap (findClosest coords) w
  let infinites = mapMaybe (findClosest coords) outside
  let finites = filter (`notElem` infinites) coords
  let areas = (\c -> length $ filter (== Just c) closest) <$> finites
  pure $ maximum areas

-- 42123
solution2 = do
  let threshold = 10000
  coords <- input
  w <- world
  let dSums = fmap (findDistSum coords) w
  pure $ length $ filter (< threshold) dSums
