module Main where

import Data.List (unfoldr)

main :: IO ()
main = do
  inp <- input
  putStrLn $ "Part 1: " ++ show (safeTiles inp 40)
  putStrLn $ "Part 2: " ++ show (safeTiles inp 400000)
  putStrLn "all done"


test :: String
test = ".^^.^.^^^^"


safeTiles :: String -> Int -> Int
safeTiles inp nrLines =
  sum . map (length . filter (== '.')) . take nrLines $ mapLines inp

mapLines :: String -> [String]
mapLines = unfoldr iter
  where iter xs =
          Just (xs, mapLine xs)

  
mapLine :: String -> String
mapLine line = map (mapTrap) $ windows ('.' : line ++ ".")


mapTrap :: String -> Char
mapTrap "^^." = '^'
mapTrap ".^^" = '^'
mapTrap "^.." = '^'
mapTrap "..^" = '^'
mapTrap _ = '.' 


windows :: String -> [String]
windows (a:xs@(b:c:_)) = [a,b,c] : windows xs
windows _ = []
  

input :: IO String
input = head . lines <$> readFile "input.txt"
