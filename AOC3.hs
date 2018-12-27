module AOC3 where
  
import Text.Parsec.ByteString (Parser, parseFromFile)  
import Text.Parsec
import Data.List
import Data.Set as S (Set(), toList, fromList, empty, intersection, null)

fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _         = b

data Coord = Coord {
  x :: Int,
  y :: Int
} deriving (Show, Eq)

instance Ord Coord where
  compare (Coord x y) (Coord x' y') = mappend (compare x x') (compare y y')

data Claim = Claim {
  claimNumber :: Int,
  topLeft :: Coord,
  width :: Int,
  height :: Int,
  coords :: Set Coord
}

instance Eq Claim where
  (Claim x _ _ _ _) == (Claim y _ _ _ _) = x == y

instance Show Claim where
  show c = "Claim@" ++ (show.claimNumber $ c)


number :: Parser Int
number = read <$> many1 digit

claimParser :: Parser Claim
claimParser = do
  num <- string "#" *> number 
  _ <- space *> string "@" *> space
  l <- number
  _ <- string ","
  t <- number
  _ <- string ": "
  w <- number
  _ <- string "x"
  h <- number <* many space
  pure $ Claim num (Coord l t) w h (fromList [Coord x y | x <- [l .. (l + w - 1)], y <- [t .. ( t + h - 1)]])

input = parseFromFile (many claimParser) "AOC3.input"

intersecting :: Claim -> Claim -> Bool
intersecting c1 c2 = let
  c1coords = coords c1
  c2coords = coords c2
  in not . S.null $ S.intersection c1coords c2coords

filterIntersecting :: [Claim] -> [Claim] -> [Claim]
filterIntersecting _ [] = []
filterIntersecting orig (x:xs) = 
  case find (intersecting x) (filter (/= x) orig) of
    Nothing -> [x]
    Just x -> filterIntersecting orig xs

solution1 = do
  claims <- fromRight [] <$> input
  let allCoords = sort . mconcat $ toList.coords <$> claims
  pure $ length . (filter (>= 2)) . (fmap length) . group $ allCoords

solution2 = do
  claims <- fromRight [] <$> input
  claim <- pure $ filterIntersecting claims claims
  return $ claimNumber (head claim)
  
  
  


