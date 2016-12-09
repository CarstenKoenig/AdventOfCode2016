module Main where

import Parser
import Data.Maybe (fromMaybe)


input :: IO String
input = readFile "input.txt"

main :: IO ()
main = do
  text <- input
  putStrLn "all done"
