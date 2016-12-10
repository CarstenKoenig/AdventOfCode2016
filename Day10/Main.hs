module Main where

import Parser
import Data.Maybe (fromMaybe, catMaybes)


type BotNr = Int

newtype Bot
  = Bot BotNr
  deriving (Eq, Show)


newtype Value
  = Value Int
  deriving (Eq, Show)

data Target
  = ToBot Bot
  | Output Int
  deriving (Eq, Show)


data Instruction
  = BotRule Bot Target Target
  | Input Bot Value
  deriving (Eq, Show)


parseInstruction :: Parser Instruction
parseInstruction =
  parseEither parseInput parseBotRule

  
parseInput :: Parser Instruction
parseInput = do
  val <- parseValue
  parseString "goes to "
  bot <- parseBot
  return $ Input bot val


parseBotRule :: Parser Instruction
parseBotRule = do
  bot <- parseBot
  parseString "gives low "
  low <- parseTarget
  parseString "and high "
  high <- parseTarget
  return $ BotRule bot low high
  

parseTarget :: Parser Target
parseTarget = do
  parseString "to "
  parseEither parseOutput (ToBot <$> parseBot)
  where
    parseOutput = do
      parseString "output "
      Output <$> parseNumber


parseBot :: Parser Bot      
parseBot = do
      parseString "bot "
      Bot <$> parseNumber


parseValue :: Parser Value
parseValue = do
      parseString "value "
      Value <$> parseNumber


input :: IO [Instruction]
input = catMaybes . map (eval parseInstruction) . lines <$> readFile "input.txt"


main :: IO ()
main = do
  instr <- input
  print instr
  putStrLn "all done"


test :: String
test = "value 5 goes to bot 2"

test' :: String
test' = "bot 0 gives low to output 2 and high to output 0"
