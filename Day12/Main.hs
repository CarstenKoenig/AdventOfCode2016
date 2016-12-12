module Main where

import Data.Char (toLower)

import Data.Maybe (catMaybes)
import qualified Data.Map as M

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
  = Cpy Target Target
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
      , pointer :: CmdPointer
      } deriving Show


parseCmd :: Parser Command
parseCmd =
  parseEither
    (parseEither parseCpy parseJnz)
    (parseEither parseInc parseDec)
  where
    parseCpy = do
      parseString "cpy"
      f <- parseTarget
      t <- parseTarget
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


main :: IO ()
main = do
  ls <- inputLines
  putStrLn "all done"
