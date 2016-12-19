{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (Left, Right)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Char8 (snoc, unpack, index)
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import Crypto.Hash.MD5 (hash)
import Numeric (showHex)
import Data.Maybe (catMaybes)
import Data.Char (isAlpha)

main :: IO ()
main = do
  putStrLn $ "Part 1: " ++ show (path part1)
  putStrLn $ "Part 2: " ++ show part2
  putStrLn "all done"


puzzleInput :: ByteString
-- puzzleInput = "ihgpwlah"
puzzleInput = "hhhxzeay"


startCoord :: (Int,Int)
startCoord = (0,0)


goalCoord :: (Int,Int)
goalCoord = (3,3)


part1 :: Node
part1 =
  let start = Node "" startCoord
      paths' = paths [start]
  in head paths'


part2 :: Int
part2 =
  let start = Node "" startCoord
  in maximum $ pathLens [start]


data Node = Node
  { path :: Path
  , coords :: (Int,Int)
  } deriving (Eq, Ord, Show)


type Path = ByteString
data Directions
  = Up | Down | Left | Right
  deriving Show


pathLens :: [Node] -> [Int]
pathLens = map (BS.length . path) . paths
  

paths :: [Node] -> [Node]
paths [] = []
paths (node:nodes) =
  let nodes' = neighbors node
  in if isGoal node
     then node : paths nodes
     else paths (nodes ++ nodes')


neighbors :: Node -> [Node]
neighbors node =
  map (move node) $ possibleDirs node


move :: Node -> Directions -> Node
move node Up =
  Node { coords = coords' Up (coords node)
       , path = path node `snoc` 'U' }
move node Down =
  Node { coords = coords' Down (coords node)
       , path = path node `snoc` 'D' }
move node Left =
  Node { coords = coords' Left (coords node)
       , path = path node `snoc` 'L' }
move node Right =
  Node { coords = coords' Right (coords node)
       , path = path node `snoc` 'R' }


isGoal :: Node -> Bool
isGoal node = (coords node) == goalCoord


possibleDirs :: Node -> [Directions]
possibleDirs node =
  filter (isValidDir $ coords node) $ hashDirs node


hashDirs :: Node -> [Directions]
hashDirs node =
  case unpack . BS.take 4 $ nodeHash node of
    [ up, down, left, right ] ->
      catMaybes [ valid Up up, valid Down down
                , valid Left left, valid Right right ]
    _ -> []

  where
    valid dir c | isAlpha c && c /= 'a'
      = Just dir
    valid _ _ = Nothing
      

validCoord :: (Int,Int) -> Bool
validCoord (x,y) = x >= 0 && x < 4
                   && y >= 0 && y < 4


isValidDir :: (Int,Int) -> Directions -> Bool
isValidDir (_,y) Up = y > 0
isValidDir (_,y) Down = y < 3
isValidDir (x,_) Left = x > 0
isValidDir (x,_) Right = x < 3
                   

coords' :: Directions -> (Int, Int) -> (Int,Int)
coords' Up (x,y) = (x,y-1)
coords' Down (x,y) = (x,y+1)
coords' Left (x,y) = (x-1,y)
coords' Right (x,y) = (x+1,y)


nodeHash :: Node -> ByteString
nodeHash node = hexHash $ puzzleInput `BS.append` (path node)


hexify :: ByteString -> ByteString
hexify = L.toStrict . toLazyByteString . byteStringHex


hexHash :: ByteString -> ByteString
hexHash = hexify . hash
  
