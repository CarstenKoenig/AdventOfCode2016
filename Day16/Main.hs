{-# LANGUAGE BangPatterns #-}

module Main where

import Data.List (foldl', unfoldr)

type Data = String


checkSumFor :: Data -> Int -> Data
checkSumFor from len =
  checkSum $ random from len


checkSum :: Data -> Data
checkSum dt =
  let dt' = reduce dt
  in if even (length dt')
     then checkSum dt'
     else dt'


reduce :: Data -> Data
reduce (x:y:xs)
  | x == y = '1' : reduce xs
  | otherwise = '0' : reduce xs
reduce xs = []


random :: Data -> Int -> Data
random from len =
  take len $ fillUp from len


fillUp :: Data -> Int -> Data
fillUp from len =
  snd . head . dropWhile (\ (l,_) -> l < len) $ dragons from


dragons :: Data -> [(Int, Data)]
dragons from = unfoldr uf (length from, from)
  where uf x = Just (x, dragon x)


dragon :: (Int, Data) -> (Int, Data)
dragon (len,xs) = (2*len+1, xs ++ ('0' : revInvert xs))


revInvert :: Data -> Data
revInvert = foldl' inv []
  where inv xs x = inv' x : xs
        inv' '1' = '0'
        inv' _ = '1'


testInput ::  Data
testInput = "10000"

input :: Data
input = "10001001100000001"

testTargetLength :: Int
testTargetLength = 20

targetLength :: Int
targetLength = 272


main :: IO ()
main = do
  putStrLn $ "Part 1: " ++ show (checkSumFor input targetLength)
  putStrLn "all done"
