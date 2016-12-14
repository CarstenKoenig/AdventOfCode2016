{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 (pack, unpack, index, group, isPrefixOf, tails)
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.ByteString.Lazy.Builder (lazyByteStringHex, toLazyByteString)
import Numeric (showHex)

import Data.Tuple (swap)
import Data.Function (on)
import Data.List (find, sort, nub, sortBy)
import Data.Maybe (mapMaybe, maybeToList)
import Control.Applicative ((<|>))

import Crypto.Hash.MD5 (hashlazy)


main :: IO ()
main = do
  print the64th
  putStrLn "all done"


salt :: L.ByteString
salt = "cuanljph"
-- salt = "abc"

hexify :: L.ByteString -> L.ByteString
hexify = toLazyByteString . lazyByteStringHex

hash :: L.ByteString -> L.ByteString
hash = hexify . L.fromStrict . hashlazy

hashMore :: Int -> L.ByteString -> L.ByteString
hashMore 0 xs = xs
hashMore n input =
  hashMore (n-1) (hash input)

puzzleHash :: L.ByteString -> Int -> L.ByteString
puzzleHash input index =
  hashMore 2017 $ input `L.append` (pack $ show index)


inputStream :: [(Int, L.ByteString)]
inputStream = map (\i -> (i, puzzleHash salt i)) [0..]


candidates :: [(Int, Candidate)]
candidates = mapMaybe containsCandidate inputStream


the64th :: [(Int,Int,Char)]
the64th = drop 63 . sort . filter validate $ take 66 keys

keys :: [(Int,Int,Char)]
keys = concatMap fst steps

steps = scanl step ([], []) candidates


contains :: L.ByteString -> L.ByteString -> Bool
contains b a = any (a `isPrefixOf`) (tails b)


validate :: (Int,Int,Char) -> Bool
validate (ind,ind',c) =
  puzzleHash salt ind `contains` (C8.replicate 3 c)
  && puzzleHash salt ind' `contains` (C8.replicate 5 c)

containsCandidate :: (Int, L.ByteString) -> Maybe (Int, Candidate)
containsCandidate inp =
   containsFives inp <|> containsTripple inp


data Candidate
  = Fives [Char]
  | Tripple Char
  deriving Show

candidateChar :: Candidate -> Char
candidateChar (Fives (c:_)) = c
candidateChar (Tripple c) = c

instance Eq Candidate where
  a == b = candidateChar a == candidateChar b


containsFives :: (Int, L.ByteString) -> Maybe (Int, Candidate)
containsFives (i, input) =
  let fvs = nub . map C8.head . filter ((>= 5) . C8.length) $ group input
  in if null fvs then Nothing else Just ((i,) $ Fives fvs)


containsTripple :: (Int, L.ByteString) -> Maybe (Int, Candidate)
containsTripple (i, input) =
   (i,) . Tripple . C8.head <$> (find ((>= 3) . C8.length) $ group input)


type Cache = [(Char, Int)]

step :: ([(Int,Int,Char)], Cache) -> (Int, Candidate) -> ([(Int,Int,Char)], Cache)
step (_, cache) (ind, Tripple c) =
  ([], clear (ind-1000) $ insertIndex c ind cache)
step (_, cache) (ind, Fives (cs@(c:_))) =
  let cache' = clear (ind-1000) cache
      founds = (\ (ind',c) -> (ind',ind,c)) <$> concatMap (findIndizes cache') cs
      cache'' = filter (\ (_,ind) -> not (ind `elem` map (\ (i,_,_) -> i) founds)) cache'
  in (founds, insertIndex c ind cache'')


insertIndex :: Char -> Int -> Cache -> Cache
insertIndex c i xs = sortBy (compare `on` snd) $ ((c,i):xs)


findIndizes :: Cache -> Char -> [(Int,Char)]
findIndizes xs c = map swap $ filter (\ (x,i) -> c == x) xs


clear :: Int -> Cache -> Cache
clear n = filter (\ (c,i) -> i >= n)
