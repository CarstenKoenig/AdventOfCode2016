module Model
  ( BotNr, Bot (..)
  , Value (..)
  , Target (..)
  , Instruction (..)
  , Bots
  , Outputs
  , parseInstruction
  , bots
  , inputs
  , allIdle
  , findJonny
  ) where

import Data.List (find)
import Data.Maybe (catMaybes)
import Data.Map (Map)
import qualified Data.Map as M

import Parser


type Bots = Map BotNr Bot

type Outputs = Map Int [Int]

data Instruction
  = BotRule Bot
  | Input Target Value
  deriving (Eq, Show)


data Bot
  = Bot
  { botNr :: BotNr
  , rule :: (Target, Target)
  , botInput :: [Int]
  }
  deriving (Eq, Show)


newtype Value
  = Value Int
  deriving (Eq, Show)

data Target
  = ToBot BotNr
  | Output Int
  deriving (Eq, Show)

type BotNr = Int


----------------------------------------------------------------------
-- retrieve bots and starting inputs

bots :: [Instruction] -> Bots
bots = foldr add M.empty . catMaybes . map getBot
  where
    add bot map = M.insert (botNr bot) bot map
    getBot (BotRule bot) = Just bot
    getBot _ = Nothing


inputs :: [Instruction] -> [(Target, Value)]
inputs = catMaybes . map getInput
  where getInput (Input botNr val) = Just (botNr, val)
        getInput _ = Nothing


----------------------------------------------------------------------
-- Bots without something in their input queue

allIdle :: Bots -> Bool
allIdle = all isIdle . M.elems


isIdle :: Bot -> Bool
isIdle = null . botInput


----------------------------------------------------------------------
-- find the one bot for part 1

findJonny :: [Bots] -> Bot
findJonny (bots : botss) =
  case getJonny bots of
    Just jonny -> jonny
    Nothing -> findJonny botss


isJonny :: Bot -> Bool
isJonny bot =
  case botInput bot of
    (61 : 17 : _) -> True
    (17 : 61 : _) -> True
    _ -> False


getJonny :: Bots -> Maybe Bot
getJonny = find isJonny . M.elems


----------------------------------------------------------------------
-- Parser definition

parseInstruction :: Parser Instruction
parseInstruction =
  parseEither parseInput parseBotRule

  
parseInput :: Parser Instruction
parseInput = do
  val <- parseValue
  parseString "goes to "
  bot <- parseBotNr
  return $ Input (ToBot bot) val


parseBotRule :: Parser Instruction
parseBotRule = do
  bot <- parseBotNr
  parseString "gives low "
  low <- parseTarget
  parseString "and high "
  high <- parseTarget
  return . BotRule $ Bot bot (low, high) []
  

parseTarget :: Parser Target
parseTarget = do
  parseString "to "
  parseEither parseOutput (ToBot <$> parseBotNr)
  where
    parseOutput = do
      parseString "output "
      Output <$> parseNumber


parseBotNr :: Parser BotNr
parseBotNr = do
      parseString "bot "
      parseNumber


parseValue :: Parser Value
parseValue = do
      parseString "value "
      Value <$> parseNumber
