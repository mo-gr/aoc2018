module AOC25 where
  
import Text.Parsec.ByteString (Parser, parseFromFile)  
import Text.Parsec
import Data.List
import Control.Monad
import Data.Set as S
import Data.Monoid

type Coord = Int
data Star = Star {
  x :: Coord,
  y :: Coord,
  z :: Coord,
  t :: Coord
} deriving (Eq, Show, Ord)

--instance Show Star where
--  show s = show (t s)

zero = Star 0 0 0 0

--instance Ord Star where
--  compare s s' = compare (distance zero s) (distance zero s')

type Constellation = Set Star

distance :: Star -> Star -> Int
distance (Star x y z t)  (Star x' y' z' t') = 
  abs (x - x')
  + abs (y - y')
  + abs (z - z') 
  + abs (t - t')
  
number :: Parser Int
number = do
  s <- (char '-' >> return negate) <|> (optional (char '+') >> return id)
  s . read <$> many1 digit

input = parseFromFile (many star) "AOC25.input"
input' = parseFromFile (many star) "example.txt"

fromRight :: Show a => Either a b -> b
fromRight (Right b) = b
fromRight (Left e)  = error $ show e

star :: Parser Star
star = do
  x <- number <* string ","
  y <- number <* string ","
  z <- number <* string ","
  t <- number <* many space
  pure $ Star x y z t
  
assignConstellation :: [Constellation] -> Star -> [Constellation]
assignConstellation [] s = [ singleton s]
assignConstellation (c:cs) s = case find ((<=3).distance s) c of
  Just s' -> S.insert s c : cs
  Nothing -> c : (assignConstellation cs s)

untilStable :: Eq a => (a -> a) -> a -> a
untilStable f a = let a' = f a in
  if a == a' then a' else untilStable f a'

untilStable' :: (Eq a, Show a) => ([a] -> [a]) -> [a] -> IO [a]
untilStable' f a = let a' = f a in
  if a == a' then pure a' else do
    print $ length a'
    untilStable' f a'

filterSubSets :: Ord a => [Set a] -> [Set a]
filterSubSets [] = []
filterSubSets (a:aa) = case find (\a' -> (a `isSubsetOf` a')) aa of
  Nothing -> a : (filterSubSets aa)
  Just _ -> filterSubSets aa

mergeConstellations :: [Constellation] -> IO [Constellation]
mergeConstellations cs = untilStable' mergeConstellations' cs
mergeConstellations' :: [Constellation] -> [Constellation]
mergeConstellations' cs = sort . filterSubSets . reverse . filterSubSets $ nub $ mconcat [ if minimum (liftM2 distance (toList c') (toList c)) < 4 then ([S.union c c']) else [c,c']
                         | c <- cs
                         , c' <- cs
                         , c /= c'
                         , not (c `isSubsetOf` c')
                         ]

-- WRONG: 388 - too high
-- wrong 13
solution1 = do
  stars <- fromRight <$> input
  let cs = Data.List.foldl assignConstellation [] stars
  mcs <- mergeConstellations cs
  pure $ length mcs