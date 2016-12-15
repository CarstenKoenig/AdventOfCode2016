module Main where

import Data.List (foldr1)

type Time = Integer

type Disc = (Int, Time -> Bool)

createDisc :: Int -> Int -> Int -> Disc
createDisc posCnt nr startPos =
  let delta = (nr + startPos)  `mod` posCnt
  in (posCnt, \ time -> (fromIntegral delta + time) `mod` fromIntegral posCnt == 0)


firstZero :: Disc -> Time
firstZero (_, slot) = head $ [ n | n <- [0..], slot n ]

zeros :: Disc -> [Time]
zeros disc@(n, slot) =
  let fZ = firstZero disc
      step = fromIntegral n
  in [ fZ + n | n <- [0,step..] ]


merge :: Ord a => [a] -> [a] -> [a]
merge as'@(a:as) bs'@(b:bs)
  | a < b = merge as bs'
  | b < a = merge as' bs
  | a == b = a : merge as bs
merge _ _ = []


solutions :: [Disc] -> [Time]
solutions = foldr1 merge . map zeros


solution :: [Disc] -> Time
solution = head . solutions


test :: [Disc]
test = [ createDisc 5 1 4, createDisc 2 2 1 ]

part1 :: [Disc]
part1 =
  [ createDisc 13 1 11
  , createDisc 5 2 0
  , createDisc 17 3 11
  , createDisc 3 4 0
  , createDisc 7 5 2
  , createDisc 19 6 17 ]


part2 :: [Disc]
part2 = part1 ++ [ createDisc 11 7 0 ]


main :: IO ()
main = do
  putStrLn $ "part 1: " ++ show (solution part1)
  putStrLn $ "part 2: " ++ show (solution part2) 
  putStrLn "all done"
