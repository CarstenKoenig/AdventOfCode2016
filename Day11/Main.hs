module Main where

import Data.Maybe (catMaybes)

import Model
import Parser (eval)


input :: IO [String]
input = lines <$> readFile "input.txt"


puzzle :: IO [(Floor, [Item])]
puzzle =
  catMaybes . fmap (eval parseFloor) <$> input

main :: IO ()
main = do
  floorInputs <- input
  putStrLn "all done"
