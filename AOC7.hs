{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TupleSections #-}

module AOC7 where

import           Control.Monad
import           Control.Monad.Reader
import           Data.Either
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Set               as S (Set, difference, empty, fromList,
                                              isSubsetOf, map, member,
                                              notMember, null, toList, unions)
import qualified Data.Set               as S
import           Data.Tuple
import           Text.Parsec
import           Text.Parsec.ByteString (Parser, parseFromFile)

--input = fromRight mempty <$> parseFromFile (many instructionParser) "example.txt"
input = fromRight mempty <$> parseFromFile (many instructionParser) "AOC7.input"

instructionParser :: Parser Instruction
instructionParser = do
  cond <- string "Step " *> letter <* string " must be finished before step "
  step <- letter <* string " can begin." <* many space
  pure $ Instruction step cond

type Step = Char

type Precondition = Char

data Instruction = Instruction
  { _step :: Step
  , _cond :: Precondition
  } deriving (Eq)

instance Show Instruction where
  show (Instruction step cond) = cond : "->" ++ [step]

instance Ord Instruction where
  compare (Instruction step cond) (Instruction step' cond') = compare step step'

allSteps :: [Instruction] -> [Step]
allSteps is = nub . sort $ mappend (_step <$> is) (_cond <$> is)

allPreconditions :: [Instruction] -> Step -> S.Set Step
allPreconditions man s =
  case directPreconditions man s of
    []   -> mempty
    pres -> S.unions (S.fromList pres : (allPreconditions man <$> pres))

directPreconditions :: [Instruction] -> Step -> [Step]
directPreconditions man s = _cond <$> filter ((== s) . _step) man

directFollow :: [Instruction] -> Step -> [Step]
directFollow man s = _step <$> filter ((== s) . _cond) man

isPrecondition :: [Instruction] -> Step -> Step -> Bool
isPrecondition man s s' =
  let direct =
        isJust $ find (liftM2 (&&) ((== s) . _cond) ((== s') . _step)) man
      pres = directPreconditions man s'
   in direct || or (isPrecondition man s <$> pres)

compareSteps :: [Instruction] -> Step -> Step -> Ordering
compareSteps man s s' =
  let isPrecondition' = isPrecondition man
      sFirst = s `isPrecondition'` s'
      s'First = s' `isPrecondition'` s
      sHasNoPre = hasNoPre man s
      s'HasNoPre = hasNoPre man s'
   in case (sFirst, s'First) of
        (True, True) -> error $ "circular dependency: " ++ [s] ++ "<>" ++ [s']
        (True, False) -> LT
        (False, True) -> GT
        _ ->
          case (sHasNoPre, s'HasNoPre) of
            (True, False)  -> LT
            (False, True)  -> GT
            (False, False) -> EQ
            (True, True)   -> compare s s'

--resolve :: [Instruction] -> Step -> Step -> Ordering
--resolve man s s' = _
hasNoPre :: [Instruction] -> Step -> Bool
hasNoPre man s = S.null $ allPreconditions man s

validate :: [Instruction] -> [Step] -> Either String [Step]
validate [] s = Right s
validate (i:is) solution =
  let pidx = elemIndex (_cond i) solution
      sidx = elemIndex (_step i) solution
   in case (pidx, sidx) of
        (Just p, Just s)
          | p > s -> Left $ "rule invalid: " ++ show i
        _ -> validate is solution

flipIdx :: Int -> Int -> [a] -> [a]
flipIdx x' y' l =
  let x = min x' y'
      y = max x' y'
      theX = l !! x
      theY = l !! y
      before = take x l
      between = take (y - x - 1) $ drop (x + 1) l
      after = drop (y + 1) l
   in before ++ [theY] ++ between ++ [theX] ++ after

apply :: [Instruction] -> (Bool, [Step]) -> (Bool, [Step])
apply [] (b, s) = (b, s)
apply (i:is) (flipped, s) =
  let pIx = fromMaybe 0 $ elemIndex (_cond i) s
      sIx = fromMaybe 0 $ elemIndex (_step i) s
   in if pIx < sIx
        then apply is (flipped, s)
        else apply is (True, flipIdx pIx sIx s)

onAndOn :: [Instruction] -> [Step] -> [Step]
onAndOn man s =
  case apply man (False, s) of
    (False, s) -> s
    (True, s') -> onAndOn man s'

data StateStep = StateStep
  { complete  :: [Step]
  , available :: [Step]
  , blocked   :: [Step]
  } deriving (Show)

candidates :: [Instruction] -> S.Set Step -> S.Set Step -> S.Set Step
candidates man compl blocked =
  let follows = S.fromList $ mconcat $ directFollow man <$> S.toList compl
      availableFollows = S.filter (`S.member` blocked) follows
   in S.filter
        (\f -> allPreconditions man f `S.isSubsetOf` compl)
        availableFollows

next :: [Instruction] -> StateStep -> StateStep
next man (StateStep d [] []) = StateStep d [] []
next man (StateStep _ [] _) = error "Something went bad"
next man (StateStep c a b) =
  let c' = c ++ [minimum a]
      newAvail = candidates man (S.fromList c') (S.fromList b)
      a' = (drop 1 . sort $ a) ++ S.toList newAvail
      b' = filter (`S.notMember` newAvail) b
   in StateStep c' a' b'

untilDone :: [Instruction] -> StateStep -> [Step]
untilDone man (StateStep d [] []) = d
untilDone man step                = untilDone man (next man step)

-- Wrong: "BFYXZDIMJKNOQRTUAHSGCPEVWL"
-- Wrong: "ZMYNFRHUITOKBXQJDASGPCEVWL"
-- Wrong: "MNYUZFRHITOKBXQJDASGPCEVWL"
-- Wrong: "MNYZUFRHITOKBXQJDASGPCEVWL"
-- Wrong: "MNYZUFOTIBRKXJQDAHSGCPEVWL"
--        "MNOUBYITKXZFHQRJDASGCPEVWL"
solution1 = do
  man <- input
  let steps = allSteps man
      starter = sort $ filter (hasNoPre man) steps
      initial = StateStep [] starter $ filter (not . (`elem` starter)) steps
      plan = untilDone man initial
  let validPlan = validate man plan
  pure validPlan

type Worker = (Int, Maybe Step)

data SchedulerState = SchedulerState
  { completed   :: [Step]
  , avail       :: Set Step
  , block       :: Set Step
  , currentTime :: Int
  , freeWorkers :: Set Worker
  , busyWorkers :: Set (Worker, Int)
  } deriving (Show)

--instance Show SchedulerState where
--  show s = show $ completed s
finished :: SchedulerState -> Bool
finished s =
  S.null (avail s) && S.null (block s) && S.null (busyWorkers s) ||
  (currentTime s > 1000)

untilFinished :: SchedulerState -> Reader [Instruction] SchedulerState
untilFinished s
  | finished s = pure s
untilFinished !s = do
  next <- tick s
  untilFinished next

initialScheduler :: [Step] -> Int -> SchedulerState
initialScheduler steps workerCount =
  let
   in SchedulerState
        mempty
        mempty
        (fromList steps)
        0
        (fromList $ (, Nothing) <$> [0 .. workerCount])
        empty

nextAvailable :: [Step] -> Set Step -> Reader [Instruction] (Set Step)
nextAvailable compl blocked = do
  man <- ask
  let follows = S.fromList $ mconcat $ directFollow man <$> compl
  let free = S.filter (S.null . allPreconditions man) blocked
  let complSet = fromList compl
  let availableFollows = free <> S.filter (`S.member` blocked) follows
  pure $
    S.filter
      (\f -> allPreconditions man f `S.isSubsetOf` complSet)
      availableFollows

tick :: SchedulerState -> Reader [Instruction] SchedulerState
tick s = do
  let updatedWorkers = S.map (fmap pred) (busyWorkers s)
      finishedWorkers = doneWorkers updatedWorkers
      workingWorkers' = workingWorkers updatedWorkers
      completed' =
        completed s <> sort (catMaybes $ snd <$> S.toList finishedWorkers)
  nextAvail <- nextAvailable completed' (block s)
  let avail' = nextAvail <> avail s
      block' = S.difference (block s) avail'
      time' = succ $ currentTime s
      freeWorkers' = freeWorkers s <> finishedWorkers
      (remainaing, starters) = scheduleWorkers freeWorkers' avail'
      busyWorkers' = workingWorkers' <> starters
      freeWorkers'' = S.difference freeWorkers' (S.map fst starters)
  pure $
    SchedulerState completed' remainaing block' time' freeWorkers' busyWorkers'

doneWorkers :: Set (Worker, Int) -> Set Worker
doneWorkers bw = fst `S.map` S.filter ((== 0) . snd) bw

workingWorkers :: Set (Worker, Int) -> Set (Worker, Int)
workingWorkers = S.filter ((/= 0) . snd)

scheduleWorkers :: Set Worker -> Set Step -> (Set Step, Set (Worker, Int))
scheduleWorkers freeWorkers availableSteps =
  if S.null freeWorkers
    then (availableSteps, empty)
    else let w = toList freeWorkers
             s = sort $ toList availableSteps
             schedule =
               (\(w, step) -> (Just step <$ w, duration step)) <$> zip w s
             remaining = fromList $ drop (length schedule) s
          in (remaining, fromList schedule)

duration :: Step -> Int
duration s = 61 + fromEnum s - fromEnum 'A'

--894 off by one 893 is correct
--893
solution2 = do
  man <- input
  let steps = allSteps man
      workerCount = 5
      init = initialScheduler steps workerCount
      finalState = runReader (untilFinished init) man
  --pure finalState
  pure $ pred (currentTime finalState)
