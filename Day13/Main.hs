module Main where

import Parser


input :: IO String
input = readFile "input.txt"


inputLines :: IO [String]
inputLines = lines <$> input


main :: IO ()
main = do
  p1 <- inputCPU
  p2 <- inputCPU2
  putStrLn $ "part 1: " ++ show (eval p1)
  putStrLn $ "part 2: " ++ show (eval p2)
  putStrLn "all done"
  where
    eval p = regA $ run p
