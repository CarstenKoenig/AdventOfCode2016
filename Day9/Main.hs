module Main where

import Parser

input :: IO [String]
input = lines <$> readFile "input.txt"


main :: IO ()
main = do
  putStrLn "all done"
