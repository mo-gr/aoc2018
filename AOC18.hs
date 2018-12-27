{-# LANGUAGE DataKinds,KindSignatures,GeneralizedNewtypeDeriving,DeriveFunctor,DeriveFoldable,DeriveTraversable,MultiParamTypeClasses,FunctionalDependencies,BangPatterns#-}
module AOC18 where

-- based on ideas from https://www.reddit.com/r/haskell/comments/a2vnni/a_better_comonad_for_cellular_automata/
  
import Data.Sequence as S
import Data.Foldable

import Text.Parsec.ByteString (Parser, parseFromFile)  
import Text.Parsec

tileParser :: Parser Tile
tileParser = (string "." *> return Open)
             <|> (string "|" *> return Tree)
             <|> (string "#" *> return Lumber)

lineParser :: Parser (S.Seq Tile)
lineParser = fromList <$> (count worldSize tileParser <* many space)

worldParser :: Parser (World Tile)
worldParser = do
  world <- fromList <$> count worldSize lineParser
  pure $ World world
  
--input = fromRight makeOpenWorld <$> parseFromFile (worldParser) "example.txt"
input = fromRight makeOpenWorld <$> parseFromFile (worldParser) "AOC18.input"

fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _         = b

data Tile = Open | Tree | Lumber 
  deriving (Eq)
  
instance Show Tile where
  show Open = "."
  show Tree = "|"
  show Lumber = "#"

data Axis = X | Y

newtype Coord (x :: Axis) = Coord { getCoord :: Int }
  deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

data Point = Point (Coord X) (Coord Y) 
  deriving (Show, Eq)

newtype World a = World (S.Seq (S.Seq a))
  deriving (Eq, Show, Functor, Foldable, Traversable)

makeOpenWorld :: World Tile
makeOpenWorld = World $ S.replicate worldSize (S.replicate worldSize Open)

class Functor c => CA p c | c -> p where
    peek :: p -> c a -> a
    evolve :: (p -> c a -> b) -> c a -> c b
    
instance CA Point World where
  peek (Point x y) (World w) = let t = S.index 
                                        (S.index w (fromIntegral y))
                                        (fromIntegral x)
                               in t `seq` t
  evolve rule world = 
    World $ S.fromFunction worldSize $ \row ->
               S.fromFunction worldSize $ \col ->
                 rule (Point (Coord col) (Coord row)) world
  
ruleSet :: Point -> World Tile -> Tile
ruleSet p w = let
  adjacentTiles :: S.Seq Tile
  adjacentTiles = (\a -> peek a w) <$> adjacent p
  in
    case peek p w of
      Open -> 
        if S.length (S.filter (==Tree) adjacentTiles) >= 3 then Tree else Open
      Tree -> 
        if S.length (S.filter (==Lumber) adjacentTiles) >= 3 then Lumber else Tree
      Lumber -> if
          S.length (S.filter (==Lumber) adjacentTiles) >= 1
          && S.length (S.filter (==Tree) adjacentTiles) >= 1
        then Lumber else Open

adjacent :: Point -> S.Seq Point
adjacent (Point x y) 
  = S.filter (\(Point x y) -> x < worldSize 
                           && x >= 0 
                           && y < worldSize
                           && y >= 0)
  $ fromList [Point (x-1) (y-1), Point x (y-1), Point (x+1) (y-1),
              Point (x-1) y, Point (x+1) y,
              Point (x-1) (y+1), Point x (y+1), Point (x+1) (y+1)]


w' = evolve ruleSet makeOpenWorld

render :: World Tile -> IO ()
render (World w) = do
  sequence $ putStrLn <$> (\line -> mconcat (show <$> toList line)) <$> w
  pure ()
  
value :: World Tile -> Int
value w = let
  treeCount = Prelude.length $ Prelude.filter (== Tree) (toList w)
  lumberCount = Prelude.length $ Prelude.filter (== Lumber) (toList w)
  in treeCount * lumberCount

worldSize = 50

times :: Int -> (a -> a) -> a -> a
times n f a = foldl' (\a _ -> f a) a [0..n]

-- 558960
solution1 = do
  world <- input
  let target = times 10 (evolve ruleSet) world
  render target
  pure $ value target
  
-- something here leaks like a sieve
solution2 = do
  world <- input
  let target = times 1000000000 (evolve ruleSet) world
  render target
  pure $ value target
  


