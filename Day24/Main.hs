module Main where

import qualified Data.IntMap.Lazy as M
import qualified Data.Set as S
import Data.Char (isDigit)
import Astar
import Parser


data Maze =
  Maze
  { walls     :: S.Set Coord
  , positions :: M.IntMap Coord
  } deriving Show

type Pos = Int
type Dist = Int
type Coord = (Int,Int)


main :: IO ()
main = do
  putStrLn "all done"


initMaze :: Maze
initMaze = Maze S.empty M.empty 


distance :: Maze -> Pos -> Pos -> Dist
distance mz =
  let distMap = M.fromList [ (f, toMap f) | f <- ks ]
  in \ f t -> distMap M.! f M.! t
  where
    toMap f = M.fromList [ (t, length $ distance' mz (coordOf f) (coordOf t)) | t <- ks ]
    ks = M.keys (positions mz)
    coordOf p = positions mz M.! p


distance' :: Maze -> Coord -> Coord -> [Coord]
distance' mz from to
  | from <= to = aStar p from
  | otherwise  = distance' mz to from
  where p           = Parameter heur neigh goal id
        heur        = manhatten to
        neigh (x,y) = filter notWall
                      [ (x-1,y-1), (x,y-1), (x+1,y-1)
                      , (x-1,y),            (x+1,y)
                      , (x-1,y+1), (x,y+1), (x+1,y+1)
                      ]
        goal        = (== to)
        manhatten (x,y) (x',y') = abs (x'-x) + abs (y'-y)
        notWall c   = not (c `S.member` walls mz)


readMaze :: FilePath -> IO Maze
readMaze file =
  foldr addPoint initMaze .
  concatMap (\ (y,cs) -> zipWith (toCoord y) [0..] cs) . zip [0..] . lines
  <$> readFile file
  where
    toCoord :: Int -> Int -> Char -> (Coord, Char)
    toCoord y x c = ((x,y), c)
    addPoint :: (Coord, Char) -> Maze -> Maze
    addPoint (coord, c) mz
      | isDigit c = insertPos coord (read [c]) mz
      | c == '#'  = insertWall coord mz
      | otherwise = mz
    insertWall :: Coord -> Maze -> Maze
    insertWall c m = m { walls = S.insert c (walls m) }
    insertPos :: Coord -> Pos -> Maze -> Maze
    insertPos c p m = m { positions = M.insert p c (positions m) }
