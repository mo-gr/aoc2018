module AOC4 where

import           Data.Hourglass
import           Data.List
import qualified Data.Map.Strict        as M
import           Data.Ord
import           Text.Parsec
import           Text.Parsec.ByteString (Parser, parseFromFile)
import           Time.Types

data Message
  = WakeUp DateTime
  | Sleep DateTime
  | ShiftStart DateTime
               GuardNum
  deriving (Show, Eq)

type GuardNum = Int

timeStamp :: Message -> DateTime
timeStamp (WakeUp t)       = t
timeStamp (Sleep t)        = t
timeStamp (ShiftStart t _) = t

instance Ord Message where
  compare m1 m2 = compare (timeStamp m1) (timeStamp m2)

messageParser :: Parser Message
messageParser = do
  dateTime <- string "[" *> dateTimeParser <* string "] "
  m <- wakeUpParser dateTime <|> sleepParser dateTime <|> shiftParser dateTime
  _ <- many space
  pure m

dateTimeParser :: Parser DateTime
dateTimeParser = do
  year <- number <* string "-"
  month <- toEnum . (+ (-1)) <$> number <* string "-"
  day <- number <* string " "
  hour <- Hours . fromIntegral <$> number <* string ":"
  min <- Minutes . fromIntegral <$> number
  pure $ DateTime (Date year month day) (TimeOfDay hour min 0 0)

number :: Parser Int
number = read <$> many1 digit

wakeUpParser :: DateTime -> Parser Message
wakeUpParser dt = do
  _ <- string "wakes up"
  pure $ WakeUp dt

sleepParser :: DateTime -> Parser Message
sleepParser dt = do
  _ <- string "falls asleep"
  pure $ Sleep dt

shiftParser :: DateTime -> Parser Message
shiftParser dt = do
  guard <- string "Guard #" *> number <* string " begins shift"
  pure $ ShiftStart dt guard

fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _         = b

input = fromRight [] <$> parseFromFile (many messageParser) "AOC4.input"

--input = fromRight [] <$> parseFromFile (many messageParser) "example.txt"
data SleepLog = SleepLog
  { _guard :: GuardNum
  , _sleep :: DateTime
  , _wake  :: DateTime
  }

instance Show SleepLog where
  show (SleepLog g s w) =
    show g ++
    ": " ++
    (show . todMin . dtTime $ s) ++ " - " ++ (show . todMin . dtTime $ w)

sleepLog :: GuardNum -> DateTime -> DateTime -> SleepLog
sleepLog = SleepLog

convertToSleepLog :: [Message] -> [SleepLog]
convertToSleepLog [] = []
convertToSleepLog ms = snd $ foldr f (Nothing, []) (sortOn Data.Ord.Down ms)
  where
    f :: Message
      -> (Maybe (GuardNum, Maybe DateTime), [SleepLog])
      -> (Maybe (GuardNum, Maybe DateTime), [SleepLog])
    f (ShiftStart _ guard) (_, ls) = (Just (guard, Nothing), ls)
    f (Sleep start) (Just (guard, Nothing), ls) = (Just (guard, Just start), ls)
    f (WakeUp wake) (Just (guard, Just start), ls) =
      (Just (guard, Nothing), sleepLog guard start wake : ls)
    f m s = error $ "unexpected message: " ++ show m ++ " state " ++ show s

sleepMinutes :: SleepLog -> Minutes
sleepMinutes s =
  let (Elapsed startTime) = timeGetElapsed . _sleep $ s
      (Elapsed endTime) = timeGetElapsed . _wake $ s
      sleepSeconds = (endTime - startTime)
      (sleepMin, _) = fromSeconds sleepSeconds
   in sleepMin

sleepTimeByGuard ::
     M.Map GuardNum Minutes -> [SleepLog] -> M.Map GuardNum Minutes
sleepTimeByGuard m [] = m
sleepTimeByGuard m (l:ls) =
  let mins = sleepMinutes l
      g = _guard l
   in case M.lookup g m of
        Nothing       -> sleepTimeByGuard (M.insert g mins m) ls
        Just prevMins -> sleepTimeByGuard (M.insert g (mins + prevMins) m) ls

findSleepiestMinutes :: [SleepLog] -> (Minutes, Int)
findSleepiestMinutes log =
  let minutes :: [Minutes]
      minutes = enumFromTo (Minutes 0) (Minutes 59)
      filterLog :: [SleepLog] -> Minutes -> (Minutes, Int)
      filterLog l m =
        ( m
        , length $
          filter
            (\l ->
               (todMin . dtTime . _sleep $ l) <= m &&
               (todMin . dtTime . _wake $ l) > m)
            l)
   in (last . sortOn snd) $ fmap (filterLog log) minutes

--119835
solution1 = do
  sleepLog <- convertToSleepLog <$> input
  sleepiestGuard <-
    pure . fst . last . sortOn snd . M.toList $
    sleepTimeByGuard M.empty sleepLog
  let sleepLogOfSleepiest = filter ((== sleepiestGuard) . _guard) sleepLog
  let sleepiestMin = fst $ findSleepiestMinutes sleepLogOfSleepiest
  pure $ sleepiestGuard * fromIntegral sleepiestMin

--12725
solution2 = do
  sleepLog <- sortOn _guard . convertToSleepLog <$> input
  let sleepLogByGuard = groupBy (\l1 l2 -> _guard l1 == _guard l2) sleepLog
  let sleepiest =
        (\l -> (_guard . head $ l, findSleepiestMinutes l)) <$> sleepLogByGuard
  let candidate = last . sortOn (snd . snd) $ sleepiest
  --print candidate
  pure $ fst candidate * fromIntegral (fst . snd $ candidate)
