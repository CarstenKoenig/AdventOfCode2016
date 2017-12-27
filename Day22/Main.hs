module Main where

import Data.List (sortBy)
import Data.Maybe (fromJust)
import Parser

data Node =
  Node
  { nodeCoord :: Coord
  , nodeSize  :: Size
  , nodeUsed  :: Size
  , nodeAvail :: Size
  } deriving Show

type Coord = (Int, Int)
type Size = Int

main :: IO ()
main = do
  inp <- readNodes
  putStrLn $ "part 1: " ++ show (part1 inp)

--- Part 2:
-- for wart two I mapped the "walls" with
-- filter (\n -> nodeUsed n > 100)
-- wich gave a line of walls between (13,19) and (37,19) (end of grid)
-- so you have to move around it starting at the empty node ad (35,24)
-- to (35,20) to (12,20) to (12,0) to (37,0)
-- from there strts the 5-cycle from the home page which leads to a total
-- number of
-- 23 + 25 + 24 + 5*36 = 252 steps which was the answer

part1 :: [Node] -> Int
part1 nodes = go 0 used avail - selfFit
  where
    go _ [] _ = 0
    go n (u:us') as@[a]
      | u <= a    = (n+1) + go n us' as
      | otherwise = n + go n us' as
    go n us@(u:us') as@(a:as')
      | u > a     = n + go n us' as
      | otherwise = go (n+1) us as'
    used = sortBy (flip compare) . filter (> 0) $ map nodeUsed nodes
    avail = sortBy (flip compare) $ map nodeAvail nodes
    selfFit = length $ filter (\n -> nodeAvail n >= nodeUsed n && nodeUsed n > 0) nodes


-- need to parse something like this:
-- Filesystem              Size  Used  Avail  Use%
-- /dev/grid/node-x0-y0     93T   68T    25T   73%

readNodes :: IO [Node]
readNodes = map (fromJust . eval nodeP) . drop 2 . lines <$> readFile "input.txt"


nodeP :: Parser Node
nodeP = Node <$> nodeCoordP <*> sizeP <*> sizeP <*> sizeP


nodeCoordP :: Parser Coord
nodeCoordP = (,)
             <$> (parseString "/dev/grid/node-x" *> parseInt)
             <*> (parseString "-y" *> parseInt <* ignoreWhiteSpace)


sizeP :: Parser Size
sizeP = parseInt <* parseString "T" <* ignoreWhiteSpace
