module AOC16 where

import           Data.Bits
import           Data.Int
import           Data.List
import           Data.Maybe
import           Text.Parsec
import           Text.Parsec.ByteString (Parser, parseFromFile)

type Register = Int

data CPU = CPU
  { regA :: Register
  , regB :: Register
  , regC :: Register
  , regD :: Register
  } deriving (Eq)

instance Show CPU where
  show (CPU a b c d) = show [a, b, c, d]

data Op
  = ADDR
  | ADDI
  | MULR
  | MULI
  | BANR
  | BANI
  | BORR
  | BORI
  | SETR
  | SETI
  | GTIR
  | GTRI
  | GTRR
  | EQIR
  | EQRI
  | EQRR
  deriving (Show, Eq)

type OpCode = Int8

type Instruction = (OpCode, Register, Register, Register)

ops :: [Op]
ops =
  [ ADDR
  , ADDI
  , MULR
  , MULI
  , BANR
  , BANI
  , BORR
  , BORI
  , SETR
  , SETI
  , GTIR
  , GTRI
  , GTRR
  , EQIR
  , EQRI
  , EQRR
  ]

opCode :: Op -> OpCode
opCode EQRR = 11
opCode EQRI = 3
opCode EQIR = 13
opCode GTRI = 7
opCode GTRR = 15
opCode GTIR = 12
opCode SETR = 14
opCode SETI = 1
opCode ADDR = 8
opCode ADDI = 10
opCode BANI = 6
opCode BANR = 4
opCode BORI = 5
opCode BORR = 0
opCode MULI = 9
opCode MULR = 2

byOpCode :: OpCode -> Op
byOpCode opc =
  fromMaybe (error $ "Unknown opcode: " ++ show opc) $ find (\op -> opCode op == opc) ops

regAccess :: CPU -> Register -> Register
regAccess c 0 = regA c
regAccess c 1 = regB c
regAccess c 2 = regC c
regAccess c 3 = regD c

regWrite :: CPU -> Register -> (Register -> CPU)
regWrite c 0 = \r -> c {regA = r}
regWrite c 1 = \r -> c {regB = r}
regWrite c 2 = \r -> c {regC = r}
regWrite c 3 = \r -> c {regD = r}

instruct :: CPU -> Op -> Register -> Register -> Register -> CPU
instruct cpu ADDR a b c = regWrite cpu c $ regAccess cpu a + regAccess cpu b
instruct cpu ADDI a b c = regWrite cpu c $ regAccess cpu a + b
instruct cpu MULR a b c = regWrite cpu c $ regAccess cpu a * regAccess cpu b
instruct cpu MULI a b c = regWrite cpu c $ regAccess cpu a * b
instruct cpu BANR a b c = regWrite cpu c $ regAccess cpu a .&. regAccess cpu b
instruct cpu BANI a b c = regWrite cpu c $ regAccess cpu a .&. b
instruct cpu BORR a b c = regWrite cpu c $ regAccess cpu a .|. regAccess cpu b
instruct cpu BORI a b c = regWrite cpu c $ regAccess cpu a .|. b
instruct cpu SETR a _ c = regWrite cpu c (regAccess cpu a)
instruct cpu SETI a _ c = regWrite cpu c a
instruct cpu GTIR a b c =
  regWrite cpu c $
  if a > regAccess cpu b
    then 1
    else 0
instruct cpu GTRI a b c =
  regWrite cpu c $
  if regAccess cpu a > b
    then 1
    else 0
instruct cpu GTRR a b c =
  regWrite cpu c $
  if regAccess cpu a > regAccess cpu b
    then 1
    else 0
instruct cpu EQIR a b c =
  regWrite cpu c $
  if a == regAccess cpu b
    then 1
    else 0
instruct cpu EQRI a b c =
  regWrite cpu c $
  if regAccess cpu a == b
    then 1
    else 0
instruct cpu EQRR a b c =
  regWrite cpu c $
  if regAccess cpu a == regAccess cpu b
    then 1
    else 0

instruct' :: CPU -> Instruction -> CPU
instruct' cpu (op, a, b, c) = instruct cpu (byOpCode op) a b c

candidates :: CPU -> CPU -> Instruction -> [Op]
candidates cpu cpu' (_, a, b, c) =
  [op | op <- ops, instruct cpu op a b c == cpu']

candidates' :: CPU -> CPU -> Instruction -> [(Op, OpCode)]
candidates' cpu cpu' (opc, a, b, c) =
  [(op, opc) | op <- ops, instruct cpu op a b c == cpu']

-- Parser Stuff
input = parseFromFile inputParser "AOC16.input"

inputParser :: Parser ([Example], Program)
inputParser = do
  ex <- many exampleParser
  program <- many instructionParser
  pure (ex, program)

instructionParser :: Parser Instruction
instructionParser = do
  op <- fromIntegral <$> number
  a <- fromIntegral <$> number
  b <- fromIntegral <$> number
  c <- fromIntegral <$> number
  pure (op, a, b, c)

type Example = (CPU, CPU, Instruction)

type Program = [Instruction]

number :: Parser Int
number = read <$> many1 digit <* many space

cpuParser :: Parser CPU
cpuParser = do
  a <- string "[" *> number <* string ", "
  b <- number <* string ", "
  c <- number <* string ", "
  d <- number <* string "]" <* many space
  return $
    CPU (fromIntegral a) (fromIntegral b) (fromIntegral c) (fromIntegral d)

exampleParser :: Parser Example
exampleParser = do
  inputCpu <- string "Before: " *> cpuParser <* many space
  op <- fromIntegral <$> number
  a <- fromIntegral <$> number
  b <- fromIntegral <$> number
  c <- fromIntegral <$> number
  outputCpu <- string "After:" *> many space *> cpuParser <* many space
  return (inputCpu, outputCpu, (op, a, b, c))

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

verify :: [Example] -> IO ()
verify [] = print "All good"
verify ((c, c', i):more) =
  if instruct' c i == c'
    then verify more
    else print $ "Broken: " ++ show c ++ show i ++ " =/=> " ++ show c'

-- 588
solution1 = do
  examplesOrError <- input
  let examples =
        case examplesOrError of
          Left err      -> error $ show err
          Right (ex, _) -> ex
  let exCandidates = uncurry3 candidates <$> examples
  pure $ length $ filter ((>= 3) . length) exCandidates

findOpCodes = do
  examplesOrError <- input
  let examples =
        case examplesOrError of
          Left err      -> error $ show err
          Right (ex, _) -> ex
  let exCandidates = uncurry3 candidates' <$> examples
  print $ nub $ filter ((== 9) . length) exCandidates

-- 115 too low
-- 627
solution2 = do
  programOrError <- input
  let (e, p) =
        case programOrError of
          Left err            -> error $ show err
          Right (ex, program) -> (ex, program)
  verify e
  --print $ last p
  let cpu0 = CPU 0 0 0 0
  pure . regA $ foldl instruct' cpu0 p
