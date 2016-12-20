module Main where

import Data.List (foldl1', foldl', sort)
import Data.Maybe (catMaybes)
import Parser


main :: IO ()
main = do
  inters <- intervals
  putStrLn $ "Part 1: " ++ show (part1 inters)
  putStrLn "all done"


data ValidIPs
  = Range Int Int
  | Empty
  | Split ValidIPs ValidIPs
  deriving Show


everything :: ValidIPs
everything = Range 0 maxBound


calculate :: [Interval] -> ValidIPs
calculate = foldl' ((simplify .) . flip remove) everything


remove :: Interval -> ValidIPs -> ValidIPs
remove _ Empty = Empty
remove int (Split a b) = Split (remove int a) (remove int b)
remove int r@(Range l u) =
  if int `contains` l && int `contains` u
  then Empty
  else if int `contains` l
  then Range (succ $ upper int) u
  else if int `contains` u
  then Range l (pred $ lower int)
  else Split (Range l (pred $ lower int)) (Range (succ $ upper int) u)


simplify :: ValidIPs -> ValidIPs
simplify Empty = Empty
simplify r@(Range a b)
  | a > b = Empty
  | otherwise = r
simplify (Split a b) =
  case (simplify a, simplify b) of
    (Empty, v) -> v
    (v, Empty) -> v
    (a,b) -> Split a b

data Interval =
  Inter { lower :: Int
        , upper :: Int
        }
  deriving (Show, Eq)


instance Ord Interval where
  (Inter l u) `compare` (Inter l' u') =
    if l < l' then LT
    else if l == l' && u > u' then LT
    else if l > l' then GT
    else if l == l' && u < u' then GT
    else EQ


part1 :: [Interval] -> Int
part1 ints = head $ validInts ints


bounds :: [Interval] -> Interval
bounds = foldl1' merge
  where merge (Inter l u) (Inter l' u') =
          Inter (minimum [l,u,l',u']) (maximum [l,u,l',u'])


validInts :: [Interval] -> [Int]
validInts ints = [ i | i <- [0..maxBound], notInAny ints i ]


notInAny :: [Interval] -> Int -> Bool
notInAny ints x = not (anyContains ints x)


anyContains :: [Interval] -> Int -> Bool
anyContains ints x = any (`contains` x) ints


contains :: Interval -> Int -> Bool
contains int x = x >= lower int && x <= upper int


intervals :: IO [Interval]
intervals =
  sort . catMaybes . map (eval interParser) <$> input


input :: IO [String]
input = lines <$> readFile "input.txt"


interParser :: Parser Interval
interParser = do
  l <- parseNumber
  parseChar (== '-')
  u <- parseNumber
  return $ Inter l u
