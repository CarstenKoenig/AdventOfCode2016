module Main where

import Parser
import Data.Maybe (fromMaybe)


inputParser :: Parser String
inputParser =
  concat <$> (parseMany $
     parseEither textParser repeaterParser)


inputParser' :: Parser Integer
inputParser' =
  sum <$> (parseMany $
     parseEither textParser' repeaterParser')


textParser :: Parser String
textParser = do
  txt <- parseAlphas
  if null txt
    then failParse
    else return txt


textParser' :: Parser Integer
textParser' = do
  txt <- parseAlphas
  if null txt
    then failParse
    else return $ fromIntegral (length txt)


repeaterParser :: Parser String
repeaterParser = do
  parseChar (== '(')
  n <- parseNumber
  parseChar (== 'x')
  t <- parseNumber
  parseChar (== ')')
  txt <- parseN parseAny n
  return $ (concat $ replicate t txt)


repeaterParser' :: Parser Integer
repeaterParser' = do
  parseChar (== '(')
  n <- parseNumber
  parseChar (== 'x')
  t <- parseNumber
  parseChar (== ')')
  inner <- parseN parseAny n
  let m = fromMaybe 0 $ eval inputParser' inner
  return $ fromIntegral t * m


input :: IO String
input = readFile "input.txt"


main :: IO ()
main = do
  text <- input
  putStrLn $ "part 1: " ++ (show . length . concat $ eval inputParser text)
  putStrLn $ "part 2: " ++ (show $ eval inputParser' text)
  putStrLn "all done"


test :: String
test = "X(8x2)(3x3)ABCY"
