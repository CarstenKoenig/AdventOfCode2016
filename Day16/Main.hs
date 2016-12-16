{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Main where

import Data.List (unfoldr)
import Data.ByteString.Char8 (ByteString, foldl')
import qualified Data.ByteString.Char8 as BS

type Data = ByteString


checkSumFor :: Data -> Int -> Data
checkSumFor from len =
  checkSum $ random from len


checkSum :: Data -> Data
checkSum = BS.pack . checkSum' . BS.unpack


checkSum' :: String -> String
checkSum' dt =
  let dt' = reduce dt
  in if even (length dt')
     then checkSum' dt'
     else dt'


reduce :: String -> String
reduce (x:y:xs)
  | x == y = '1' : reduce xs
  | otherwise = '0' : reduce xs
reduce _ = []


random :: Data -> Int -> Data
random from len =
  BS.take len $ fillUp from len


fillUp :: Data -> Int -> Data
fillUp from len =
  snd . head . dropWhile (\ (l,_) -> l < len) $ dragons from


dragons :: Data -> [(Int, Data)]
dragons from = unfoldr uf (BS.length from, from)
  where uf x = Just (x, dragon x)


dragon :: (Int, Data) -> (Int, Data)
dragon (len, xs) = (2*len+1, xs `BS.append` ('0' `BS.cons` revInvert xs))


revInvert :: Data -> Data
revInvert = BS.map inv . BS.reverse
  where inv '1' = '0'
        inv _ = '1'


testInput ::  Data
testInput = "10000"

input :: Data
input = "10001001100000001"

testTargetLength :: Int
testTargetLength = 20

targetLength :: Int
targetLength = 272


targetLength2 :: Int
targetLength2 = 35651584


main :: IO ()
main = do
  putStrLn $ "Part 1: " ++ show (checkSumFor input targetLength)
  putStrLn $ "Part 2: " ++ show (checkSumFor input targetLength2)
  putStrLn "all done"
