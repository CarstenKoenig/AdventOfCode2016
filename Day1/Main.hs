module Main where

import Data.List (foldl')
import Data.Char (toLower, isDigit)

data Turn
  = TurnRight
  | TurnLeft
  deriving Show

type Command = (Turn, Int)

parseTurn :: String -> Maybe (Turn, String)
parseTurn (c : remainder)
  | toLower c == 'r' = Just (TurnRight, remainder)
  | toLower c == 'l' = Just (TurnLeft, remainder)
  | otherwise = Nothing


parseDigits :: String -> Maybe (String, String)
parseDigits (c : remainder)
  | isDigit c = do
      (ds, rem) <- parseDigits remainder
      return (c:ds, rem)
  | otherwise = Just ("", c:remainder)

parseDistance :: String -> Maybe (Int, String)
parseDistance input = do
  (ds, rem) <- parseDigits input
  case ds of
    "" -> Nothing
    ds -> Just (read ds, rem)

parseSep :: String -> Maybe ((), String)
parseSep (',' : ' ' : remainder) = Just ((), remainder)
parseSep other = Just ((), other)

parseCommand :: String -> Maybe (Command, String)
parseCommand input = do
  (turn, rem) <- parseTurn input
  (dist, rem') <- parseDistance rem
  (_, rem'') <- parseSep rem'
  return ((turn, dist), rem'')

parseCommands :: String -> [Command]
parseCommands input =
  case parseCommand input of
    Nothing -> []
    Just (cmd, remainder) -> cmd : parseCommands remainder


type Direction = (Int, Int)

north :: Direction
north = (0,1)

turnDirection :: Turn -> Direction -> Direction
turnDirection TurnRight (x,y) = (y,-x)
turnDirection TurnLeft (x,y) = (-y,x)

type Coord = (Int, Int)

walk :: Direction -> Int
     -> (Coord, [Coord], Maybe Coord)
     -> (Coord, [Coord], Maybe Coord)
walk (dx,dy) 0 st = st
walk (dx,dy) d ((x,y), visited, Nothing) =
  let coord = (x+dx,y+dy)
  in if elem coord visited
     then walk (dx,dy) (d-1) (coord, visited, Just coord)
     else walk (dx,dy) (d-1) (coord, coord:visited, Nothing)
walk (dx,dy) d ((x,y), visited, found) =
  walk (dx,dy) (d-1) ((x+dx,y+dy), visited, found)

type State = (Direction, Coord, [Coord], Maybe Coord)

start :: State
start = (north, (0,0), [], Nothing)

move :: State -> Command -> State
move (dir, coord, visited, found) (turn, dist) =
  let dir' = turnDirection turn dir
      (coord', visited', found') = walk dir' dist (coord, visited, found)
  in (dir', coord', visited', found')

distanceCoord :: Coord -> Coord -> Int
distanceCoord (sx,sy) (ex,ey) =
  abs (ex-sx) + abs (ey-sy)

distance :: State -> State -> Int
distance (_, start, _, _) (_, end, _, _) =
  distanceCoord start end

calculateDistance :: [Command] -> (Int, Maybe Int)
calculateDistance cmds =
  let end@(_,_,_,found) = foldl' move start cmds
      toHq = fmap (distanceCoord (0,0)) found
  in (distance start end, toHq)

main :: IO ()
main = do
  input <- getContents
  let cmds = parseCommands input
  let (dist,toHq) = calculateDistance cmds
  print dist
  print toHq
