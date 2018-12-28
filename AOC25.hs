module AOC25 where

import           Control.Monad
import           Data.List
import           Data.Monoid
import           Data.Set               as S
import           Text.Parsec
import           Text.Parsec.ByteString (Parser, parseFromFile)

type Coord = Int

data Star = Star
  { x :: Coord
  , y :: Coord
  , z :: Coord
  , t :: Coord
  } deriving (Eq, Ord, Show)

{-instance Show Star where
  show s = show (x s) ++ ":"
        ++ show (y s) ++ ":"
        ++ show (z s) ++ ":"
        ++ show (t s)
-}
zero = Star 0 0 0 0

--instance Ord Star where
--  compare s s' = compare (distance zero s) (distance zero s')
type Constellation = Set Star

unparse :: [Constellation] -> String
unparse [] = ""
unparse (c:cs) =
  unparse cs ++
  unlines
    (fmap
       (\s ->
          show (x s) ++
          "," ++ show (y s) ++ "," ++ show (z s) ++ "," ++ show (t s))
       (toList c))

distance :: Star -> Star -> Int
distance (Star x y z t) (Star x' y' z' t') =
  abs (x - x') + abs (y - y') + abs (z - z') + abs (t - t')

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
assignConstellation [] s = [singleton s]
assignConstellation (c:cs) s =
  case find ((<= 3) . distance s) c of
    Just s' -> S.insert s c : cs
    Nothing -> c : assignConstellation cs s

untilStable :: Eq a => (a -> a) -> a -> a
untilStable f a =
  let a' = f a
   in if a == a'
        then a'
        else untilStable f a'

untilStable' :: (Eq a, Show a) => ([a] -> [a]) -> [a] -> IO [a]
untilStable' f a =
  let a' = f a
   in if a == a'
        then pure a'
        else do
          print $ length a'
          untilStable' f a'

intersects s s' = not . S.null $ intersection s s'

filterSubSets :: Ord a => [Set a] -> [Set a]
filterSubSets a = filterSubSets' a a

filterSubSets' :: Ord a => [Set a] -> [Set a] -> [Set a]
filterSubSets' _ [] = []
filterSubSets' orig (a:aa) =
  case find (\a' -> (a `isProperSubsetOf` a')) orig of
    Nothing -> a : filterSubSets' orig aa
    Just _  -> filterSubSets' orig aa

mergeConstellations :: [Constellation] -> IO [Constellation]
mergeConstellations = untilStable' mergeConstellations'

mergeConstellations' :: [Constellation] -> [Constellation]
mergeConstellations' cs =
  sort . filterSubSets $
  toList $ fst (Prelude.foldl f (S.empty, S.empty) (liftM2 (,) cs cs))
  where
    f (acc, blacklist) (c, c') =
      if c == c'
        then (acc, blacklist)
        else if c `member` blacklist
               then (acc, blacklist)
               else if c' `member` blacklist
                      then (acc, blacklist)
                      else if (minimum (liftM2 distance (toList c) (toList c')) <
                               4) ||
                              (c `intersects` c')
                             then ( S.insert
                                      (S.union c c')
                                      (S.delete c' $ S.delete c acc)
                                  , S.insert c' $ S.insert c blacklist)
                             else (S.insert c acc, blacklist)

-- WRONG: 388 - too high
-- wrong 13
-- RIGHT: 350
solution1 = do
  stars <- fromRight <$> input
  let cs = Data.List.foldl assignConstellation [] stars
  mcs <- mergeConstellations cs
  print $ length cs
  pure $ length mcs
