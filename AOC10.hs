{-# LANGUAGE GeneralizedNewtypeDeriving,NamedFieldPuns,RecordWildCards #-}
module AOC10 where
  
import Text.Parsec.ByteString (Parser, parseFromFile)  
import Text.Parsec
import Data.List

import Graphics.SpriteKit

newtype X = X { unX :: Int } deriving (Eq, Num, Ord, Enum)
newtype Y = Y { unY :: Int } deriving (Eq, Num, Ord, Enum)

instance Show X where
  show x = show $ unX x
instance Show Y where
  show y = show $ unY y

data Coord = Coord {
  x :: X,
  y :: Y
} deriving (Eq)

data Velocity = Velocity {
  vx :: X,
  vy :: Y
} deriving (Eq)

type Star = (Coord, Velocity)

instance Show Coord where
  show (Coord (X x) (Y y)) = show x ++ "x" ++ show y

instance Show Velocity where
  show (Velocity (X x) (Y y)) = show x ++ "x" ++ show y

instance Ord Coord where
  compare (Coord (X x) (Y y)) (Coord (X x') (Y y'))= 
    mappend (compare x x') (compare y y')

number :: Parser Int
number = do
  s <- (char '-' >> return negate) <|> (optional (char '+') >> return id)
  s . read <$> many1 digit

velocityParser :: Parser Velocity
velocityParser = do
  _ <- string "velocity=<" <* many space
  x <- number <* string "," <* many space
  y <- number <* many space <* string ">" <* many space
  pure $ Velocity (X x) (Y y)

coordParser :: Parser Coord
coordParser = do 
  _ <- string "position=<" <* many space
  x <- number <* string "," <* many space
  y <- number <* many space <* string ">" <* many space
  pure $ Coord (X x) (Y y)
  
pointParser :: Parser (Coord, Velocity)
pointParser = do
  c <- coordParser
  v <- velocityParser
  pure (c,v)

fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _         = b

--input = fromRight mempty <$> parseFromFile (many pointParser) "example.txt"
input = fromRight mempty <$> parseFromFile (many pointParser) "AOC10.input"

tick :: [(Coord, Velocity)] -> [(Coord, Velocity)]
tick cvs = move <$> cvs
 where move (coord, velo) = 
          (Coord (x coord + vx velo)
                 (y coord + vy velo),
           velo)

--toPlot (time, cs) = sequence $ putStrLn <$> (\((Coord cx cy),_) -> --(show cx) ++ ", " ++ (show cy) ++ ", " ++ (show time)) <$> cs

toSprite :: Star -> Node nodeData
toSprite ((Coord (X x) (Y y)), _) = whitePixel (fromIntegral x) (fromIntegral (height - y))

-- ZNNRZJXP
-- 10418
solution1 = do
  start <- input
  let time = 10400
      skipSome = head . drop time $ iterate tick start
      zero = AOC10UserData skipSome time
  --sequence $ toPlot <$> sky
  pure $ skyScene zero
  
skyScene zeroData = (sceneWithSize (Size width height)) {
    sceneHandleEvent = Just advance,
    sceneUpdate = Just updateScene,
    sceneData = zeroData
  }

width = 500
height = 500
white = colorWithRGBA 1 1 1 1

whitePixel x y = (spriteNodeWithColorSize white (Size 1 1)) {nodePosition = Point x y}

data AOC10UserData = AOC10UserData {
 stars :: [Star],
 time :: Int
}

updateScene scene@Scene{sceneData = d} _ = scene {
  sceneChildren = (labelNodeWithText $ "   Time: " ++ show (time d)) 
      : (toSprite <$> (stars d))
}

advance :: EventHandler AOC10UserData
advance KeyEvent{ keyEventType = KeyDown } (AOC10UserData s t) = Just $ AOC10UserData (tick s) (succ t) 
advance _ _ = Nothing
