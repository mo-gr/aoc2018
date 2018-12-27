module AOC19 where
  
import Text.Parsec.ByteString (Parser, parseFromFile)  
import Text.Parsec
import Data.Int
import Data.Bits
import Data.List
import Data.Char
  
type Register = Int

data CPU = CPU {
  ip :: Int,
  regA :: Register,
  regB :: Register,
  regC :: Register,
  regD :: Register,
  regE :: Register,
  regF :: Register
} deriving (Eq)

instance Show CPU where
  show (CPU _ a b c d e f) = show [a,b,c,d,e,f]

data Op = ADDR | ADDI
  | MULR | MULI
  | BANR | BANI
  | BORR | BORI
  | SETR | SETI
  | GTIR | GTRI | GTRR
  | EQIR | EQRI | EQRR
  deriving (Show, Eq, Read)

type OpCode = Int8  
type Instruction = (OpCode, Register, Register, Register)

ops :: [Op]
ops = [ADDR , ADDI
  , MULR , MULI
  , BANR , BANI
  , BORR , BORI
  , SETR , SETI
  , GTIR , GTRI , GTRR
  , EQIR , EQRI , EQRR]
  
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
byOpCode opc = case find (\op -> opCode op == opc) ops of
  Nothing -> error $ "Unknown opcode: " ++ show opc
  Just o -> o
  
regAccess :: CPU -> Register -> Register
regAccess c 0 = regA c
regAccess c 1 = regB c
regAccess c 2 = regC c
regAccess c 3 = regD c
regAccess c 4 = regE c
regAccess c 5 = regF c

regWrite :: CPU -> Register -> (Register -> CPU)
regWrite c 0 = \r -> c {regA = r}
regWrite c 1 = \r -> c {regB = r}
regWrite c 2 = \r -> c {regC = r}
regWrite c 3 = \r -> c {regD = r}
regWrite c 4 = \r -> c {regE = r}
regWrite c 5 = \r -> c {regF = r}

instruct :: CPU -> Op -> Register -> Register -> Register-> CPU
instruct cpu ADDR a b c = regWrite cpu c $ (regAccess cpu a) + (regAccess cpu b)
instruct cpu ADDI a b c = regWrite cpu c $ (regAccess cpu a) + b
instruct cpu MULR a b c = regWrite cpu c $ (regAccess cpu a) * (regAccess cpu b)
instruct cpu MULI a b c = regWrite cpu c $ (regAccess cpu a) * b
instruct cpu BANR a b c = regWrite cpu c $ (regAccess cpu a) .&. (regAccess cpu b)
instruct cpu BANI a b c = regWrite cpu c $ (regAccess cpu a) .&. b
instruct cpu BORR a b c = regWrite cpu c $ (regAccess cpu a) .|. (regAccess cpu b)
instruct cpu BORI a b c = regWrite cpu c $ (regAccess cpu a) .|. b
instruct cpu SETR a _ c = regWrite cpu c $ (regAccess cpu a)
instruct cpu SETI a _ c = regWrite cpu c $ a
instruct cpu GTIR a b c = regWrite cpu c $ if (a > (regAccess cpu b)) then 1 else 0
instruct cpu GTRI a b c = regWrite cpu c $ if ((regAccess cpu a) > b) then 1 else 0
instruct cpu GTRR a b c = regWrite cpu c $ if ((regAccess cpu a) > (regAccess cpu b)) then 1 else 0
instruct cpu EQIR a b c = regWrite cpu c $ if (a == (regAccess cpu b)) then 1 else 0
instruct cpu EQRI a b c = regWrite cpu c $ if ((regAccess cpu a) == b) then 1 else 0
instruct cpu EQRR a b c = regWrite cpu c $ if ((regAccess cpu a) == (regAccess cpu b)) then 1 else 0

instruct' :: CPU -> Instruction -> CPU
instruct' cpu (op, a, b, c) = 
  let cpu' = instruct cpu (byOpCode op) a b c 
      ipNum = (ip cpu')
  in
    regWrite cpu' ipNum $ (succ (regAccess cpu' ipNum))

lookup' :: [a] -> Int -> Maybe a
lookup' xs n | n < 0 = Nothing
            | n > length xs = Nothing
            | otherwise = Just $ xs !! n

execute :: [Instruction] -> CPU ->  CPU
execute prog cpu = let
  instructionPointer = regAccess cpu (ip cpu)
  instruction = lookup' prog instructionPointer
  in
    case instruction of
      Nothing -> cpu
      Just instruction' -> execute prog (instruct' cpu instruction')

-- Parser Stuff
input = parseFromFile (inputParser) "AOC19.input"

fromRight :: Show a => Either a b -> b
fromRight (Right b) = b
fromRight (Left e)  = error $ show e

inputParser :: Parser (Int, Program)
inputParser = do
  ip <- string "#ip " *> number <* many space
  program <- (many instructionParser)
  pure (ip, program)

instructionParser :: Parser Instruction
instructionParser = do
  op <- (fmap toUpper) <$> count 4 letter <* many space
  let opc = opCode . read $ op
  a <- fromIntegral <$> number 
  b <- fromIntegral <$> number
  c <- fromIntegral <$> number
  pure $ (opc, a, b, c)

type Program = [Instruction]

number :: Parser Int
number = read <$> many1 digit <* many space

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f = \(a, b, c) -> f a b c 

solution1 = do
  (ip, prog) <- fromRight <$> input
  let cpu = CPU ip 0 0 0 0 0 0
  let cpu' = execute prog cpu
  pure cpu'
  
solution2 = do
  (ip, prog) <- fromRight <$> input
  let cpu = CPU ip 1 0 0 0 0 0
  let cpu' = execute prog cpu
  pure cpu'