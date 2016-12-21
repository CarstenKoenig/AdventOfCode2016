module Main where

import Data.List (elemIndex, foldl', inits)
import Data.Maybe (catMaybes)

import Parser


text :: String
text = "abcdefgh" -- should be bfheacgd

scrampeled :: String
scrampeled = "fbgdceah"

main :: IO ()
main = do
  insts <- instructions
  putStrLn $ "Part 1: " ++ process insts text
  putStrLn $ "Part 2: " ++ processInv insts scrampeled
  putStrLn "all done"


data Instruction
  = SwapPos Int Int
  | SwapLetter Char Char
  | Rotate Int
  | RotateOn Char
  | Reverse Int Int
  | Move Int Int
  deriving Show


process :: [Instruction] -> String -> String
process insts input = foldl' (flip apply) input insts


processInv :: [Instruction] -> String -> String
processInv insts input = foldl' (flip applyInv) input (reverse insts)


apply :: Instruction -> String -> String
apply (SwapPos a b) input =
  apply (SwapLetter (input !! a) (input !! b)) input
apply (SwapLetter a b) input =
  map swap input
  where swap c
          | c == a = b
          | c == b = a
          | otherwise = c
apply (Rotate n) input =
  take l $ drop (n `mod` l) $ cycle input
  where l = length input
apply (RotateOn c) input =
  case elemIndex c input of
    Nothing -> input
    Just n ->
      let r = if n >= 4 then n+2 else n+1
      in apply (Rotate $ negate r) input
apply (Reverse a b) input
  | a > b = apply (Reverse b a) input
  | a <= b =
    let (prev,end) = splitAt (b+1) input
        (start, mid) = splitAt a prev
    in concat [start, reverse mid, end]
apply (Move a b) input =
  let (prev,(ca:rest)) = splitAt a input
      (prev', end) = splitAt b (prev++rest)
  in prev' ++ ca:end


applyInv :: Instruction -> String -> String
applyInv x@(SwapPos _ _) input = apply x input
applyInv x@(SwapLetter _ _) input = apply x input
applyInv (Rotate n) input = apply (Rotate $ negate n) input
applyInv (RotateOn c) input =
  head $ [ xs | i <- [0..length input-1]
              , let xs = apply (Rotate i) input
              , apply (RotateOn c) xs == input ]
applyInv x@(Reverse _ _) input = apply x input
applyInv (Move a b) input = apply (Move b a) input
  

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


----------------------------------------------------------------------
-- test
test :: [String]
test = map (\is -> process is "abcde") $ inits testInstructions


testInv :: String
testInv = processInv testInstructions "decab"


testInstructions =
  [ SwapPos 4 0
  , SwapLetter 'd' 'b'
  , Reverse 0 4
  , Rotate 1
  , Move 1 4
  , Move 3 0
  , RotateOn 'b'
  , RotateOn 'd'
  ]
