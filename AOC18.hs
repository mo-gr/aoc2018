{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AOC18 where

import           Data.Foldable
import           Data.Functor
import           Data.Maybe
import           Data.Sequence          as S
import           Text.Parsec
import           Text.Parsec.ByteString (Parser, parseFromFile)

tileParser :: Parser Tile
tileParser =
  (string "." Data.Functor.$> Open) <|> (string "|" Data.Functor.$> Tree) <|>
  (string "#" Data.Functor.$> Lumber)

lineParser :: Parser (S.Seq Tile)
lineParser = fromList <$> (count worldSize tileParser <* many space)

worldParser :: Parser (World Tile)
worldParser = do
  world <- fromList <$> count worldSize lineParser
  pure $ World world

--input = fromRight makeOpenWorld <$> parseFromFile (worldParser) "example.txt"
input = fromRight makeOpenWorld <$> parseFromFile worldParser "AOC18.input"

fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _         = b

data Tile
  = Open
  | Tree
  | Lumber
  deriving (Eq)

instance Show Tile where
  show Open   = "."
  show Tree   = "|"
  show Lumber = "#"

newtype CoordX = CoordX
  { _x :: Int
  } deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

newtype CoordY = CoordY
  { _y :: Int
  } deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

data Point =
  Point CoordX
        CoordY
  deriving (Show, Eq)

newtype World a =
  World (S.Seq (S.Seq a))
  deriving (Eq, Show)

makeOpenWorld :: World Tile
makeOpenWorld = World $ S.replicate worldSize (S.replicate worldSize Open)

peek :: Point -> World a -> a
peek (Point x y) (World w) =
  let r = fromMaybe undefined (w !? fromIntegral y)
      t = fromMaybe undefined (r !? fromIntegral x)
   in t

evolve :: (Point -> World a -> a) -> World a -> World a
evolve rule world@(World w) =
  World $
  S.mapWithIndex
    (\rowIndex row ->
       S.mapWithIndex
         (\colIndex col ->
            rule (Point (CoordX colIndex) (CoordY rowIndex)) world)
         row)
    w

ruleSet :: Point -> World Tile -> Tile
ruleSet p w =
  let adjacentTiles :: S.Seq Tile
      adjacentTiles = (`peek` w) <$> adjacent p
   in case peek p w of
        Open ->
          if S.length (S.filter (== Tree) adjacentTiles) >= 3
            then Tree
            else Open
        Tree ->
          if S.length (S.filter (== Lumber) adjacentTiles) >= 3
            then Lumber
            else Tree
        Lumber ->
          if S.length (S.filter (== Lumber) adjacentTiles) >= 1 &&
             S.length (S.filter (== Tree) adjacentTiles) >= 1
            then Lumber
            else Open

adjacent :: Point -> S.Seq Point
adjacent (Point x y) =
  S.filter
    (\(Point x y) ->
       x < fromIntegral worldSize &&
       x >= 0 && y < fromIntegral worldSize && y >= 0) $
  fromList
    [ Point (x - 1) (y - 1)
    , Point x (y - 1)
    , Point (x + 1) (y - 1)
    , Point (x - 1) y
    , Point (x + 1) y
    , Point (x - 1) (y + 1)
    , Point x (y + 1)
    , Point (x + 1) (y + 1)
    ]

render :: World Tile -> IO ()
render (World w) =
  sequence_ $ putStrLn . (\line -> mconcat (show <$> toList line)) <$> w

worldToList :: World Tile -> [Tile]
worldToList (World r) = mconcat $ toList <$> toList r

value :: World Tile -> Int
value w =
  let treeCount = Prelude.length $ Prelude.filter (== Tree) (worldToList w)
      lumberCount = Prelude.length $ Prelude.filter (== Lumber) (worldToList w)
   in treeCount * lumberCount

worldSize :: Int
worldSize = 50

times :: Int -> (a -> a) -> a -> a
times n f a = foldl' (\a _ -> f a) a [0 .. (pred n)]

-- 558960
solution1 = do
  world <- input
  let target = times 10 (evolve ruleSet) world
  --render target
  pure $ value target

-- something here leaks like a sieve
solution2 = do
  world <- input
  let target = times 1000000000 (evolve ruleSet) world
  render target
  pure $ value target
