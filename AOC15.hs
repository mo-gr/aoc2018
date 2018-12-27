module AOC15 where
  
import Control.Applicative
import Data.List
import Data.Maybe

type HP = Int
type Coord = (Int, Int)
type Id = Int

data Tile = Wall
  | Empty
  | Goblin { hp :: HP, id:: Id }
  | Elf { hp :: HP, id:: Id }
  
instance Eq Tile where
  (Goblin _ g) == (Goblin _ g') = g == g'
  (Elf _ e) == (Elf _ e') = e == e'
  _ == _ = False
  
isGoblin (Goblin _ _) = True
isGobling _ = False
isElf (Elf _ _) = True
isElf _ = False

instance Show Tile where
  show Wall = "#"
  show Empty = "."
  show (Goblin _ _) = "G"
  show (Elf _ _) = "E"

type World = [[Tile]]

test :: World
test = [
  replicate 7 Wall,
  [Wall, Empty, Goblin 200 0, Empty, Empty, Empty, Wall],
  [Wall, Empty, Empty, Empty, Elf 200 0, Goblin 200 1, Wall],
  [Wall, Empty, Wall, Empty, Wall, Goblin 200 2, Wall],
  [Wall, Empty, Empty, Goblin 200 3, Wall, Elf 200 2, Wall],
  [Wall, Empty, Empty, Empty, Empty, Empty, Wall],
  replicate 7 Wall]
  
render :: World -> IO ()
render w = do
  sequence $ putStrLn <$> (\line -> unwords $ show <$> line) <$> w
  pure ()
  

turnCandidates :: World -> [Tile]
turnCandidates w = filter (liftA2 (||) isGoblin isElf) $ mconcat w

coord :: World -> Tile -> (Int, Int)
coord _ Empty = error "not supported"
coord _ Wall = error "not supported"
coord w unit = fromJust <$> (head $ filter (isJust.snd) $ (\(y, line) -> (y, findIndex (== unit) line)) <$> zip [0..] w)

tick :: World -> World
tick w = w