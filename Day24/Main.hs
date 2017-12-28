module Main where

import Data.Char (isDigit)
import Data.Graph.AStar
import qualified Data.HashSet as HS
import qualified Data.IntMap.Lazy as M
import Data.List ((\\), sort)
import Data.Maybe (fromJust)
import qualified Data.Set as S


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
  mz <- readMaze "input.txt"
  let opt = optimalRoute0 mz
  putStrLn $ "part 1: " ++ show opt ++ " - " ++ show (routeLength mz opt)

  let opt' = optimalRoute1 mz
  putStrLn $ "part 2: " ++ show opt' ++ " - " ++ show (routeLength mz opt')


routeLength :: Maze -> [Pos] -> Dist
routeLength mz ps = sum $ zipWith (distance mz) ps (tail ps)


optimalRoute0 :: Maze -> [Pos]
optimalRoute0 mz = (0 :) . map fst . fromJust $ aStar neigh dist heur goal (0, [0])
  where heur   = const 0
        dist (f,_) (t,_) = distance mz f t
        neigh (_,vs) = HS.fromList [ (p, p : vs) | p <- (ks \\ vs) ]
        goal  (_, vs) = length vs == length ks
        ks     = M.keys (positions mz)


optimalRoute1 :: Maze -> [Pos]
optimalRoute1 mz = (0 :) . map fst . fromJust $ aStar neigh dist heur goal (0, [0])
  where heur   = const 0
        dist (f,_) (t,_) = distance mz f t
        neigh (_,vs)     = HS.fromList [ (p, p : vs) | p <- more vs ]
        goal  (p, vs)    = p == 0 && length vs == length ks + 1
        ks               = M.keys (positions mz)
        more vs          = let vs' = ks \\ vs in if null vs' then [0] else vs'



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
  | from <= to = fromJust $ aStar neigh manhatten heur goal from
  | otherwise  = distance' mz to from
  where heur        = manhatten to
        neigh (x,y) = HS.fromList $ filter notWall
                      [            (x,y-1)
                      , (x-1,y),            (x+1,y)
                      ,            (x,y+1)
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
