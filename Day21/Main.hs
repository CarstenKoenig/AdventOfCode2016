module Main where

import Data.Maybe (catMaybes)

import Parser

main :: IO ()
main = do
  putStrLn "all done"


data Instruction
  = SwapPos Int Int
  | SwapLetter Char Char
  | Rotate Int
  | RotateOn Char
  | Reverse Int Int
  | Move Int Int
  deriving Show


instructions :: IO [Instruction]
instructions =
  catMaybes . map (eval parseInstruction) <$> input


input :: IO [String]
input = lines <$> readFile "input.txt"


parseInstruction :: Parser Instruction
parseInstruction = do
  parseOneOf [ swapPosP
             , swapLetP
             , rotateP
             , rotateOnP
             , reverseP
             , moveP ]
  

swapPosP :: Parser Instruction
swapPosP = do
  parseString "swap position"
  a <- parseNumber
  parseString "with position"
  b <- parseNumber
  return $ SwapPos a b


swapLetP :: Parser Instruction
swapLetP = do
  parseString "swap letter "
  a <- parseChar (const True)
  parseString " with letter "
  b <- parseChar (const True)
  return $ SwapLetter a b


rotateP :: Parser Instruction
rotateP = do
  parseString "rotate"
  parseWhiteSpaces
  lr <- leftRightP
  parseWhiteSpaces
  n <- parseNumber
  parseString "step"
  return $ Rotate (lr n)
  where leftRightP = parseEither leftP rightP
        leftP = parseString "left" >> pure negate
        rightP = parseString "right" >> pure id


rotateOnP :: Parser Instruction
rotateOnP = do
  parseString "rotate based on position of letter "
  c <- parseChar (const True)
  return $ RotateOn c


reverseP :: Parser Instruction
reverseP = do
  parseString "reverse positions"
  a <- parseNumber
  parseString "through"
  b <- parseNumber
  return $ Reverse a b


moveP :: Parser Instruction
moveP = do
  parseString "move position"
  a <- parseNumber
  parseString "to position"
  b <- parseNumber
  return $ Move a b
