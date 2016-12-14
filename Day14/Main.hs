{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 (pack, unpack, index, group)
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.ByteString.Lazy.Builder (lazyByteStringHex, toLazyByteString)
import Numeric (showHex)

import Data.Function (on)
import Data.List (find, sort, nub, sortBy)
import Data.Maybe (mapMaybe, maybeToList)
import Control.Applicative ((<|>))

import Crypto.Hash.MD5 (hashlazy)


salt :: L.ByteString
salt = "cuanljph"
-- salt = "abc"

hexify :: L.ByteString -> L.ByteString
hexify = toLazyByteString . lazyByteStringHex


puzzleHash :: L.ByteString -> Int -> L.ByteString
puzzleHash input index =
  hexify . L.fromStrict . hashlazy $ input `L.append` (pack $ show index)


inputStream :: [(Int, L.ByteString)]
inputStream = map (\i -> (i, puzzleHash salt i)) [0..]


candidates :: [(Int, Candidate)]
candidates = mapMaybe containsCandidate inputStream


the64th :: [Int]
the64th = sort $ take 64 keys

keys :: [Int]
keys = concatMap fst steps

steps = scanl step ([], []) candidates


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

step :: ([Int], Cache) -> (Int, Candidate) -> ([Int], Cache)
step (_, cache) (ind, Tripple c) =
  ([], insertIndex c ind cache)
step (_, cache) (ind, Fives (cs@(c:_))) =
  let cache' = clear (ind-1000) cache
      founds = concatMap (findIndizes cache') cs
  in (founds, insertIndex c ind cache')


insertIndex :: Char -> Int -> Cache -> Cache
insertIndex c i xs = sortBy (compare `on` snd) $ ((c,i):xs)


findIndizes :: Cache -> Char -> [Int]
findIndizes xs c = map snd $ filter (\ (x,i) -> c == x) xs


clear :: Int -> Cache -> Cache
clear n = filter (\ (c,i) -> i >= n)

-- passwordChar :: L.ByteString -> Maybe Char
-- passwordChar hash
--   | "00000" `isPrefixOf` hash = Just $ hash `index` 5
--   | otherwise = Nothing


-- passwordChars :: L.ByteString -> String
-- passwordChars input = catMaybes . map passwordChar $ map (puzzleHash input) [0..]


-- password :: L.ByteString -> String
-- password = take 8 . passwordChars


main :: IO ()
main = do
  putStrLn "all done"
