module Main where

import Data.List (group, sort)
import Data.Maybe (catMaybes)
import Data.Char (toLower, isDigit, isSpace, isAlpha)

import Parser

data SecToken
  = SecToken
    { tokenParts :: [String]
    , tokenId :: Int
    , tokenCheck :: String
    }
  deriving Show


parseSecToken :: Parser SecToken
parseSecToken = do
  parts <- parseParts
  id <- parseNumber
  _ <- parseChar (== '[')
  check <- parseAlphas
  _ <- parseChar (== ']')
  return $ SecToken parts id check
  where parseParts = parseMany parsePart
        parsePart = do
          str <- parseAlphas
          _ <- parseChar (== '-')
          return str
          

occurences :: SecToken -> [(Int, Char)]
occurences = sort . groups . sort . concat . tokenParts
  where groups = map getGroup . group
        getGroup xs@(x:_) = (negate (length xs), x)


isReal :: SecToken -> Bool
isReal token = tokenCheck token == check
  where check = take 5 . map snd $ occurences token
  

main :: IO ()
main = do
  contents <- lines <$> readFile "input.txt"
  let inputs = catMaybes $ map (eval parseSecToken) contents
  let reals = filter isReal inputs
  let output = sum $ map tokenId reals
  print output


testInput :: String
testInput = "aaaaa-bbb-z-y-x-123[abxyz]"
