module AOC9 where
  
import Control.Monad.ST
import Data.STRef

data STCircleNode s a = STCircleNode {
  val :: a,
  prevNode :: STRef s (STCircleNode s a),
  nextNode :: STRef s (STCircleNode s a)
  }
  
instance Show a => Show (STCircleNode s a) where
  show c = show (val c)

singleCircle :: a -> ST s (STCircleNode s a)
singleCircle v = do 
  p <- newSTRef undefined
  n <- newSTRef undefined
  self <- pure $ STCircleNode v p n
  writeSTRef n self
  writeSTRef p self
  return self
  
appendNode :: a -> STCircleNode s a -> ST s (STCircleNode s a)
appendNode a thisNode = do
  newNode <- singleCircle a
  oldNextNode <- readSTRef (nextNode thisNode)
  writeSTRef (nextNode thisNode) newNode
  writeSTRef (prevNode oldNextNode) newNode
  writeSTRef (prevNode newNode) thisNode
  writeSTRef (nextNode newNode) oldNextNode
  pure newNode
  
removeNode :: STCircleNode s a -> ST s (a, STCircleNode s a)
removeNode node = do
  p <- readSTRef (prevNode node)
  n <- readSTRef (nextNode node)
  writeSTRef (nextNode p) n
  writeSTRef (prevNode n) p
  pure (val node, n)

traverseLeft :: Int -> STCircleNode s a -> ST s (STCircleNode s a)
traverseLeft 0 s = pure s
traverseLeft n s = do
  p <- readSTRef (prevNode s)
  traverseLeft (pred n) p

data Game = Game {
  playerCount :: Int,
  finalMarble :: Marble
}

data GameState s = GameState {
  currentMarble :: STCircleNode s Marble,
  remainingMarbles :: ![Marble],
  currentPlayer :: !Int,
  scores :: ![Score]
}

type Marble = Int
type Score = Int

initialState :: Game -> ST s (GameState s)
initialState g = do
  let firstMarble = head $ marblesInGame g
      rest = drop 1 $ marblesInGame g
      scores = replicate (playerCount g) 0
  initialCircle <- singleCircle firstMarble
  pure $ GameState initialCircle rest 0 scores

replaceIndex :: Int -> (Int -> Int) -> [Int] -> [Int]
replaceIndex n f l = let
  before = take n l
  it = head $ drop n l
  after = drop (succ n) l
  in before ++ [f it] ++ after

isMagic :: Marble -> Bool
isMagic m = 0 == (fromEnum m) `mod` 23

isFinished :: GameState s -> ST s Bool
isFinished (GameState _ [] _ _) = pure True
isFinished _ = pure False

gameRound :: GameState s -> ST s (GameState s)
gameRound g | isMagic (head $ remainingMarbles g) = do 
  let magicMarble = head $ remainingMarbles g
  priceMarble <- traverseLeft 7 (currentMarble g)
  (priceMarble, currentMarble') <- removeNode priceMarble
  let price = fromIntegral (priceMarble + magicMarble)
  pure $ g {
    remainingMarbles = drop 1 $ remainingMarbles g,
    currentPlayer = (succ $ currentPlayer g) `mod` (length $ scores g),
    currentMarble = currentMarble',
    scores = replaceIndex (currentPlayer g) (+ price) (scores g)
    }
gameRound g = do 
  let nextMarble = head $ remainingMarbles g
  successor <- readSTRef (nextNode (currentMarble g))
  currentMarble' <- appendNode nextMarble successor
  pure $ g {
    remainingMarbles = drop 1 $ remainingMarbles g,
    currentPlayer = (succ $ currentPlayer g) `mod` (length $ scores g),
    currentMarble = currentMarble'
    }

untilM :: Monad m => (a -> m Bool) -> (a -> m a) -> m a -> m a
untilM mPredicate mf ma = do
  a <- ma
  p <- mPredicate a
  if p then pure a else untilM mPredicate mf (mf a)

marblesInGame :: Game -> [Marble]
marblesInGame g = [0..(finalMarble g)]

input = Game 429 70901 -- 399645
legendaryInput = Game 429 (70901 * 100) -- 3352507536
test0 = Game 7 25 -- 32
test1 = Game 10 1618 -- 8317
test2 = Game 13 7999 -- 146373
test3 = Game 17 1104 -- 2764
test4 = Game 21 6111 -- 54718
test5 = Game 30 5807 -- 37305

play :: Game -> ST s Score
play g = do
  startState <- initialState g
  endState <- untilM isFinished gameRound (pure startState)
  pure $ 
    maximum (scores endState)
    
        
solution1 = do
  let res = runST $ play input
  pure res
  
solution2 = do
  let res = runST $ play legendaryInput
  pure res