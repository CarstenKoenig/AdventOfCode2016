module Main where

import Parser

data Input
  = Text String
  | Repeater Int Int
  deriving Show


inputParser :: Parser String
inputParser =
  concat <$> (parseMany $
     parseEither textParser repeaterParser)


textParser :: Parser String
textParser = do
  txt <- parseAlphas
  if null txt
    then failParse
    else return txt


repeaterParser :: Parser String
repeaterParser = do
  parseChar (== '(')
  n <- parseNumber
  parseChar (== 'x')
  t <- parseNumber
  parseChar (== ')')
  txt <- parseN n
  return $ (concat $ replicate t txt)


input :: IO String
input =
  concat . eval inputParser <$> readFile "input.txt"


main :: IO ()
main = do
  parts <- input
  print $ length parts
  putStrLn "all done"


test :: String
test = "X(8x2)(3x3)ABCY"
