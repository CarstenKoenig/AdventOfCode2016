module Main where

import Data.Char (toLower)

import Data.Maybe (catMaybes, isNothing)

import Parser


data Register
  = RegA
  | RegB
  | RegC
  | RegD
  deriving Show


data Target
  = ToRegister Register
  | ToValue Int
  deriving Show

data Command
  = Cpy Target Register
  | Inc Register
  | Dec Register
  | Jnz Target Int
  deriving Show


type Programm = [Command]

data CmdPointer =
  CmdPointer { prev :: [Command]
             , current :: Command
             , next :: [Command]
             } deriving Show


data CPU =
  CPU { regA :: Int
      , regB :: Int
      , regC :: Int
      , regD :: Int
      , pointer :: Maybe CmdPointer
      } deriving Show


initialize :: Programm -> CPU
initialize (l:ls) = CPU 0 0 0 0 (Just pt)
  where pt = CmdPointer [] l ls


run :: CPU -> CPU
run cpu
  | halting cpu = cpu
  | otherwise =
    run (step cpu)

step :: CPU -> CPU
step cpu =
  case curCommand cpu of
    Just (Cpy t r) -> copy t r cpu
    Just (Inc r) -> increase r cpu
    Just (Dec r) -> decrease r cpu
    Just (Jnz t i) -> jumpNotZero t i cpu
    _ -> cpu


halting :: CPU -> Bool
halting cpu = isNothing (pointer cpu)


curCommand :: CPU -> Maybe Command
curCommand cpu =
  case pointer cpu of
    Just (CmdPointer _ current _) ->
      Just current
    _ -> Nothing

copy :: Target -> Register -> CPU -> CPU
copy (ToValue v) RegA cpu =
  moveNext $ cpu { regA = v }
copy (ToValue v) RegB cpu =
  moveNext $ cpu { regB = v }
copy (ToValue v) RegC cpu =
  moveNext $ cpu { regC = v }
copy (ToValue v) RegD cpu =
  moveNext $ cpu { regD = v }
copy (ToRegister r) RegA cpu =
  moveNext $ cpu { regA = getRegister r cpu }
copy (ToRegister r) RegB cpu =
  moveNext $ cpu { regB = getRegister r cpu }
copy (ToRegister r) RegC cpu =
  moveNext $ cpu { regC = getRegister r cpu }
copy (ToRegister r) RegD cpu =
  moveNext $ cpu { regD = getRegister r cpu }
  

increase :: Register -> CPU -> CPU
increase reg cpu =
  let v = getRegister reg cpu
  in copy (ToValue $ v+1) reg cpu


decrease :: Register -> CPU -> CPU
decrease reg cpu =
  let v = getRegister reg cpu
  in copy (ToValue $ v-1) reg cpu


jumpNotZero :: Target -> Int -> CPU -> CPU
jumpNotZero (ToRegister reg) delta cpu =
  let v = getRegister reg cpu
  in jumpNotZero (ToValue v) delta cpu
jumpNotZero (ToValue v) delta cpu =
  if v /= 0
  then cpu { pointer = movePointer delta (pointer cpu) }
  else moveNext cpu


moveNext :: CPU -> CPU
moveNext cpu =
  cpu { pointer = movePointer 1 (pointer cpu) }


movePointer :: Int -> Maybe CmdPointer -> Maybe CmdPointer
movePointer _ Nothing = Nothing
movePointer 0 (Just pt) = Just pt
movePointer n (Just pt)
  | n > 0 =
    case pt of
      CmdPointer prs c (nx:nxs) ->
        let pt' = CmdPointer (c:prs) nx nxs
        in movePointer (n-1) (Just pt')
      _ -> Nothing
  | n < 0 =
    case pt of
      CmdPointer (pr:prs) c nxs ->
        let pt' = CmdPointer prs pr (c:nxs)
        in movePointer (n+1) (Just pt')
      _ -> Nothing


getRegister :: Register -> CPU -> Int
getRegister RegA = regA
getRegister RegB = regB
getRegister RegC = regC
getRegister RegD = regD


parseCmd :: Parser Command
parseCmd =
  parseEither
    (parseEither parseCpy parseJnz)
    (parseEither parseInc parseDec)
  where
    parseCpy = do
      parseString "cpy"
      f <- parseTarget
      t <- parseRegister
      return $ Cpy f t
    parseJnz = do
      parseString "jnz"
      f <- parseTarget
      i <- parseInt
      return $ Jnz f i
    parseInc = do
      parseString "inc"
      f <- parseRegister
      return $ Inc f
    parseDec = do
      parseString "dec"
      f <- parseRegister
      return $ Dec f


parseTarget :: Parser Target
parseTarget =
  parseEither
    (ToRegister <$> parseRegister)
    (ToValue <$> parseInt)


parseRegister :: Parser Register
parseRegister = do
  ignoreWhiteSpace
  c <- parseAlpha
  case toLower c of
    'a' -> pure RegA
    'b' -> pure RegB
    'c' -> pure RegC
    'd' -> pure RegD
    _ -> failParse


input :: IO String
input = readFile "input.txt"


inputLines :: IO [String]
inputLines = lines <$> input


programm :: IO Programm
programm =
  catMaybes . fmap (eval parseCmd) <$> inputLines


inputCPU :: IO CPU
inputCPU = initialize <$> programm


main :: IO ()
main = do
  ls <- inputLines
  putStrLn "all done"


testProgramm :: Programm
testProgramm =
  [ Cpy (ToValue 41) RegA
  , Inc RegA
  , Inc RegA
  , Dec RegA
  , Jnz (ToRegister RegA) 2
  , Dec RegA
  ]
