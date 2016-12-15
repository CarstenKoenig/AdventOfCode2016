{-# LANGUAGE BangPatterns #-}
module Main where

import Data.List (foldr1)


type Time = Integer
data Disc =
  Disc { possitions :: Int
       , delta :: Int
       }
       


main :: IO ()
main = do
  putStrLn $ "part 1: " ++ show (solution part1)
  putStrLn $ "part 2: " ++ show (solution part2) 
  putStrLn "all done"


----------------------------------------------------------------------
-- Problem Input

part1 :: [Disc]
part1 =
  zipWith ($)
    [ createDisc 13 11
    , createDisc 5 0
    , createDisc 17 11
    , createDisc 3 0
    , createDisc 7 2
    , createDisc 19 17 ]
    [ 1.. ]


part2 :: [Disc]
part2 = part1 ++ [ createDisc 11 0 7 ]


example :: [Disc]
example = [ createDisc 5 1 4, createDisc 2 2 1 ]


----------------------------------------------------------------------
-- calculation

solution :: [Disc] -> Time
solution = head . solutions


solutions :: [Disc] -> [Time]
solutions = foldr1 merge . map zeros


zeros :: Disc -> [Time]
zeros disc@(Disc n delta) =
  let !fZ = firstZero disc
      step = fromIntegral n
  in [ fZ + i | i <- [0,step..] ]


firstZero :: Disc -> Integer
firstZero (Disc n delta) =
  fromIntegral $ (negate delta) `mod` n


createDisc :: Int -> Int -> Int -> Disc
createDisc posCnt startPos nr =
  let delta = (nr + startPos)  `mod` posCnt
  in Disc posCnt delta


merge :: Ord a => [a] -> [a] -> [a]
merge as'@(a:as) bs'@(b:bs)
  | a < b = merge as bs'
  | b < a = merge as' bs
  | a == b = a : merge as bs
merge _ _ = []
