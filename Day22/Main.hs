module Main where

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
  putStrLn "all done"

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
