module Main where

import Control.Monad.State.Strict
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, fromJust)
import Data.Char (isLetter)
import Parser
import Debug.Trace


main :: IO ()
main = do

  p <- readProgram "./input.txt"
  putStrLn $ "part 1: " ++ show (part1 p)
  putStrLn $ "part 2: " ++ show (part2 p)


type Runtime a = State Env a

data Env
  = Env
  { program            :: Program
  , instructionPointer :: Int
  , registers          :: M.Map Register Int
  } deriving Show

type Program = IM.IntMap Instruction

data Instruction
  = Copy Value Value
  | Increase Value
  | Decrease Value
  | JumpNotZero Value Value
  | Toggle Value
  deriving (Show, Eq, Ord)


data Value
  = Constant Int
  | Register Register
  deriving (Show, Eq, Ord)


type Register = Char


part2 :: Program -> Int
part2 prg =
  evalState (setRegister 12 'a' >> run >> readRegister 'a') (Env prg 0 M.empty)


part1 :: Program -> Int
part1 prg =
  evalState (setRegister 7 'a' >> run >> readRegister 'a') (Env prg 0 M.empty)


run :: Runtime ()
run = do
  inst <- getInstruction
  case inst of
    Nothing -> return ()
    Just i  -> do
      regs <- gets registers
      step i
      run


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
step (Toggle at) = do
  atV <- getValue at
  modifyInstruction toggle atV
  moveNext
  where
    toggle (Increase a)      = Decrease a
    toggle (Decrease a)      = Increase a
    toggle (Toggle   a)      = Increase a
    toggle (JumpNotZero a b) = Copy a b
    toggle (Copy a b)        = JumpNotZero a b


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
instructionP = parseOneOf [ copyP, incP, decP, jumpP, toggleP ]
  where
    copyP   = Copy <$> (parseString "cpy " *> valueP) <*> valueP
    incP    = Increase <$> (parseString "inc " *> valueP)
    decP    = Decrease <$> (parseString "dec " *> valueP)
    jumpP   = JumpNotZero <$> (parseString "jnz " *> valueP) <*> valueP
    toggleP = Toggle <$> (parseString "tgl " *> valueP)
    valueP  = (parseEither regP constP) <* ignoreWhiteSpace
    regP    = Register <$> parsePred isLetter
    constP  = Constant <$> parseInt
