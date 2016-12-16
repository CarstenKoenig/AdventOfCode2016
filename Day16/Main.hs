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
checkSum dt =
  let dt' = reduce dt
  in if even (BS.length dt')
     then checkSum dt'
     else dt'


reduce :: Data -> Data
reduce input =
  let (bs,rest) = BS.splitAt 2 input
  in case BS.unpack bs of
    [x,y] | x == y -> '1' `BS.cons` reduce rest
          | otherwise -> '0' `BS.cons` reduce rest
    _ -> BS.empty


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
revInvert = foldl' inv BS.empty
  where inv xs x = inv' x `BS.cons` xs
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


targetLength2 :: Int
targetLength2 = 35651584


main :: IO ()
main = do
  putStrLn $ "Part 1: " ++ show (checkSumFor input targetLength)
  putStrLn $ "Part 2: " ++ show (checkSumFor input targetLength2)
  putStrLn "all done"
