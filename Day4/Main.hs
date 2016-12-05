module Main where

import Data.List (group, sort, find, isPrefixOf)
import Data.Maybe (catMaybes, isJust)
import Data.Char (toLower, isLetter, ord, chr)

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


shiftChar :: Char -> Char
shiftChar '-' = ' '
shiftChar 'z' = 'a'
shiftChar 'Z' = 'A'
shiftChar c
  | isLetter c = succ c
  | otherwise = c


nTimes :: (a -> a) -> Int -> a -> a
nTimes _ 0 x = x
nTimes f n x = nTimes f (n-1) (f x)


decrypt :: SecToken -> [String]
decrypt token =
  map (map decode) $ tokenParts token
  where decode = nTimes shiftChar n
        n = tokenId token `mod` 26


containsNorth :: SecToken -> Bool
containsNorth token =
  any (isPrefixOf "north") wrds
  where wrds = decrypt token
  

main :: IO ()
main = do
  contents <- lines <$> readFile "input.txt"
  let inputs = catMaybes $ map (eval parseSecToken) contents
  let reals = filter isReal inputs
  let withN = filter containsNorth reals
  print withN
  let output = sum $ map tokenId reals
  print output


testInput :: String
testInput = "qzmt-zixmtkozy-ivhz-343[abcde]"
