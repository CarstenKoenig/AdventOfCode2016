module Main where

import Data.List (replicate)

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Maybe (fromJust)

main :: IO ()
main = do
  putStrLn $ "Part 1: " ++ show (part1 myRound)
  putStrLn $ "Part 2: " ++ show (part2 myRound)
  putStrLn "all done"


myInput :: Int
myInput = 3014603


myRound :: [Elf]
myRound = [1..myInput]


type Elf = Int


test :: [Elf]
test = [1..5]


part1 :: [Elf] -> [Elf]
part1 as = reduce as []


reduce :: [Elf] -> [Elf] -> [Elf]
reduce [elf] [] = [elf]
reduce [] acc = reduce (reverse acc) []
reduce [elf] acc = reduce (elf : reverse acc) []
reduce (a : _ : as) acc = reduce as (a : acc)


part2 :: [Elf] -> Elf
part2 = fromJust . part2' . Seq.fromList

part2' :: Seq Elf -> Maybe Elf
part2' = part2'' . Seq.viewl

part2'' :: Seq.ViewL Elf -> Maybe Elf
part2'' Seq.EmptyL = Nothing
part2'' (x Seq.:< xs)
  | Seq.null xs = Just x
  | otherwise = 
      part2' $ (l Seq.>< Seq.drop 1 r) Seq.|> x
  where (l,r) = Seq.splitAt (mid xs) xs
        mid xs = (length xs - 1) `div` 2
