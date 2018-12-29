module AOC14 where

import           Data.Int
import           Data.Vector.Unboxed as V

type Score = Int8

type ScoreBoard = Vector Score

start :: ScoreBoard
start = fromList [3, 7]

type Elves = (Int, Int)

type Kitchen = (ScoreBoard, Elves)

tick :: Kitchen -> Kitchen
tick (score, (e0, e1)) =
  let r0 = score ! e0
      r1 = score ! e1
      next = singleton $ (r0 + r1) `mod` 10
      next' =
        if r0 + r1 >= 10
          then singleton $ (r0 + r1) `div` 10
          else empty
      score' = V.concat [score, next', next]
      e0' = (fromIntegral r0 + 1 + e0) `mod` V.length score'
      e1' = (fromIntegral r1 + 1 + e1) `mod` V.length score'
   in (score', (e0', e1'))

unwrap :: Int -> Kitchen -> String
unwrap n k = mconcat $show <$> (V.toList . V.take 10 . V.drop n . fst) k

unwrap' :: ScoreBoard -> Kitchen -> Int
unwrap' target (k, _) = search target k 0

search :: ScoreBoard -> ScoreBoard -> Int -> Int
search target score c =
  let n = V.length target
      candidate = V.take n score
   in if target == candidate || V.null score
        then c
        else search target (V.tail score) (succ c)

-- 3147574107
solution1 :: IO String
solution1 = do
  putStrLn "Warning: This will take a bit"
  let scoreBoard = start
      elves = (0, 1)
      kitchen = (scoreBoard, elves)
      target = 293801
  pure $
    unwrap target $
    Prelude.head $ Prelude.drop (target + 10) $ iterate tick kitchen

-- WRONG: 1300565 too low
-- WRONG: 13032973 too low
solution2 :: IO Int
solution2 = do
  let scoreBoard = start
      elves = (0, 1)
      kitchen = (scoreBoard, elves)
      optmistic = 10000000
      target = V.fromList [2, 9, 3, 8, 0, 1]
  pure $ unwrap' target $ iterate tick kitchen !! optmistic
