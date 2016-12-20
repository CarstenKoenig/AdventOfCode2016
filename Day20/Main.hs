module Main where

import Data.List (foldl1', foldl', sort)
import Data.Maybe (catMaybes)
import Parser


main :: IO ()
main = do
  inters <- intervals
  putStrLn $ "Part 1: " ++ show (part1 inters)
  putStrLn $ "Part 2: " ++ show (part2 inters)
  putStrLn "all done"


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
part1 = head . validNumbers . fixMerge


part2 :: [Interval] -> Int
part2 = length . validNumbers . fixMerge


validNumbers :: [Interval] -> [Int]
validNumbers ((Inter _ u) : b@(Inter l _) : ints) =
  [succ u .. pred l] ++ validNumbers (b:ints)
validNumbers _ = []


fixMerge :: [Interval] -> [Interval]
fixMerge is =
  let is' = mergeInts is
  in if length is' < length is then fixMerge is' else is'

mergeInts :: [Interval] -> [Interval]
mergeInts (a@(Inter l u) : b@(Inter l' u') : ints)
  | l' <= u && u' <= u = mergeInts (a : ints)
  | succ u == l' = mergeInts ((Inter l u') : ints)
  | l' <= u && u' > u = mergeInts ((Inter l u') : ints)
  | otherwise = a : mergeInts (b:ints)
mergeInts xs = xs  


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
