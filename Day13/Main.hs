module Main where

import Astar


newtype State
  = State (Int, Int)
  deriving (Show, Eq, Ord)


start :: State
start = State (1,1)

goal :: (Int, Int)
goal = (31, 39)

favNumber :: Int
favNumber = 1362

atGoal :: State -> Bool
atGoal (State (x, y)) =
  (x,y) == goal


aStarParams :: Parameter State State
aStarParams =
  Parameter heur surroundings atGoal id
  where
    heur (State (x,y)) =
      let (gx,gy) = goal
      in abs (gx-x) + abs (gy-y) + 1


surroundings :: State -> [State]
surroundings pos =
  [ pos' | d <- directions
         , let pos' = move d pos
         , inside pos'
         , openSpace favNumber pos' ]


openSpace :: Int -> State -> Bool
openSpace fav (State (x,y)) =
  not (calcWall fav x y)


calcWall :: Int -> Int -> Int -> Bool
calcWall fav x y =
  let d = x*x + 3*x + 2*x*y + y + y*y + fav
      ones = sumOnes d
  in odd ones


sumOnes :: Int -> Int
sumOnes 0 = 0
sumOnes 1 = 1
sumOnes n =
  let (d,m) = n `divMod` 2
  in m + sumOnes d


inside :: State -> Bool
inside (State (x,y)) =
  x >= 0 && y >= 0


move  :: (Int,Int) -> State -> State
move (dx,dy) (State (x,y)) = State (x+dx,y+dy)


directions :: [(Int,Int)]
directions =
  [ (-1,0),(1,0),(0,-1),(0,1) ]


solutionPath :: Path State
solutionPath =
  aStar aStarParams start


solution :: Int
solution =
  length solutionPath - 1

main :: IO ()
main = do
  putStrLn "all done"


showSolution :: IO ()
showSolution = do
  let path = solutionPath
  showGrid favNumber 50 50 path

showGrid :: Int -> Int -> Int -> Path State -> IO ()
showGrid fav w h poss =
  let cells = [ [ wall fav poss (x,y) | x <- [0..w] ] | y <- [0..h] ]
  in printLines cells
  where
    wall fav poss (x,y) =
      let isW = calcWall fav x y
          isP = State (x,y) `elem` poss
      in if isW && isP
         then 'X'
         else if isP
              then 'O'
              else if isW then '#' else '.'
    printLines = putStrLn . unlines
