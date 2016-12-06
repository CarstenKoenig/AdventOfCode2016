{-# LANGUAGE OverloadedStrings  #-}

module Main where

import Data.Function (on)
import Data.Ord (comparing)
import Data.List (foldl', sortBy)
import Data.Map (Map)
import qualified Data.Map as M

type Position = Int
type Occurence = Int

type Frequency =
  Map Char Occurence

type Received =
  Map Position Frequency


addOccurence :: Received -> (Position, Char) -> Received
addOccurence map (pos, c) =
  M.alter addChar pos map
  where addChar (Just map') =
          Just $ M.alter (Just . maybe 1 (+1)) c map'
        addChar Nothing =
          Just $ M.singleton c 1


empty :: Received
empty = M.empty


processLine :: Received -> String -> Received
processLine map =
  foldl' addOccurence map . zip [0..]


processLines :: [String] -> Received
processLines =
  foldl' processLine empty


mostOccuringChar :: Frequency -> Char
mostOccuringChar =
  fst . head . reverse . sortBy (compare `on` snd) .  M.toList


leastOccuringChar :: Frequency -> Char
leastOccuringChar =
  fst . head .  sortBy (compare `on` snd) .  M.toList


decodeMessage :: Received -> String
decodeMessage =
  map (mostOccuringChar . snd) . M.toList

decodeMessage' :: Received -> String
decodeMessage' =
  map (leastOccuringChar . snd) . M.toList


input :: IO [String]
input = lines <$> readFile "input.txt"


main :: IO ()
main = do
  received <- processLines <$> input
  putStr "Part1: "
  print $ decodeMessage received
  
  putStr "Part2: "
  print $ decodeMessage' received
  
  putStrLn "all done"
