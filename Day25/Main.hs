module Main where

import Control.Monad.State.Strict
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, fromJust)
import Data.Char (isLetter)
import Parser


main :: IO ()
main = do

  p <- readProgram "./prg.txt"
  putStrLn $ "part 1: " ++ (show $ part1 p)


type Runtime a = State Env a

data Env
  = Env
  { program            :: Program
  , instructionPointer :: Int
  , registers          :: M.Map Register Int
  , output             :: [Int]
  } deriving Show

type Program = IM.IntMap Instruction

data Instruction
  = Copy Value Value
  | Increase Value
  | Decrease Value
  | JumpNotZero Value Value
  | Output Value
  deriving (Show, Eq, Ord)


data Value
  = Constant Int
  | Register Register
  deriving (Show, Eq, Ord)


type Register = Char


part1 :: Program -> Int
part1 prg = head $ filter (isSignal . flip tryInp prg) [1..]
  where isSignal = all id . zipWith (==) (cycle [0,1])

tryInp :: Int -> Program -> [Int]
tryInp a prg =
  reverse . output $ execState (setRegister (a + 2541) 'a' >> run 50000) (Env prg 0 M.empty [])


run :: Int -> Runtime ()
run 0 = return ()
run n = do
  inst <- getInstruction
  case inst of
    Nothing -> return ()
    Just i  -> do
      regs <- gets registers
      step i
      run (n-1)


step :: Instruction -> Runtime ()
step (Copy val (Register reg)) = do
  v <- getValue val
  setRegister v reg
  moveNext
step (Copy _ (Constant _)) =
  moveNext
step (Increase (Register reg)) = do
  incrRegister reg
  moveNext
step (Increase (Constant _)) =
  moveNext
step (Decrease (Register reg)) = do
  decrRegister reg
  moveNext
step (Decrease (Constant _)) =
  moveNext
step (JumpNotZero b j) = do
  bv <- getValue b
  if bv /= 0
    then do
      jv <- getValue j
      jump (+ jv)
    else moveNext
step (Output val) = do
  v <- getValue val
  addOutput v
  moveNext


getValue :: Value -> Runtime Int
getValue (Constant n) = return n
getValue (Register r) = readRegister r


readRegister :: Register -> Runtime Int
readRegister reg = fromMaybe 0 <$> gets (\env -> M.lookup reg (registers env))


incrRegister :: Register -> Runtime ()
incrRegister = modRegister (+ 1)


decrRegister :: Register -> Runtime ()
decrRegister = modRegister (subtract 1)


setRegister :: Int -> Register -> Runtime ()
setRegister val reg = modify' (\env -> env { registers = M.insert reg val (registers env) })


modRegister :: (Int -> Int) -> Register -> Runtime ()
modRegister upd reg = modify' (\env -> env { registers = M.update (Just . upd) reg (registers env) })


addOutput :: Int -> Runtime ()
addOutput v = modify' (\env -> env { output = v : output env })


getInstruction :: Runtime (Maybe Instruction)
getInstruction = gets (\env -> instructionPointer env `IM.lookup` program env)


modifyInstruction :: (Instruction -> Instruction) -> Int -> Runtime ()
modifyInstruction upd delta = modify' (\env -> env { program = IM.alter (fmap upd) (instructionPointer env + delta) (program env) })


moveNext :: Runtime ()
moveNext = jump (+1)


jump :: (Int -> Int) -> Runtime ()
jump jmp = modify' (\env -> env { instructionPointer = jmp (instructionPointer env) })


readProgram :: FilePath -> IO Program
readProgram file =
  IM.fromList . zip [0..] . map (fromJust . eval instructionP) . lines <$> readFile file


instructionP :: Parser Instruction
instructionP = parseOneOf [ copyP, incP, decP, jumpP, outputP ]
  where
    copyP   = Copy <$> (parseString "cpy " *> valueP) <*> valueP
    incP    = Increase <$> (parseString "inc " *> valueP)
    decP    = Decrease <$> (parseString "dec " *> valueP)
    jumpP   = JumpNotZero <$> (parseString "jnz " *> valueP) <*> valueP
    outputP = Output <$> (parseString "out " *> valueP)
    valueP  = (parseEither regP constP) <* ignoreWhiteSpace
    regP    = Register <$> parsePred isLetter
    constP  = Constant <$> parseInt
