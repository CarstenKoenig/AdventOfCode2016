{-# LANGUAGE OverloadedStrings  #-}

module Main where

import Data.Maybe (catMaybes)
import Data.Char (isDigit)

import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 (pack, unpack, index, isPrefixOf)
import Data.ByteString.Lazy.Builder (lazyByteStringHex, toLazyByteString)
import Numeric (showHex)

import Crypto.Hash.MD5 (hashlazy)


hexify :: L.ByteString -> L.ByteString
hexify = toLazyByteString . lazyByteStringHex


puzzleHash :: L.ByteString -> Int -> L.ByteString
puzzleHash input index =
  hexify . L.fromStrict . hashlazy $ input `L.append` (pack $ show index)


passwordChar :: L.ByteString -> Maybe Char
passwordChar hash
  | "00000" `isPrefixOf` hash = Just $ hash `index` 5
  | otherwise = Nothing


passwordChars :: L.ByteString -> String
passwordChars input = catMaybes . map passwordChar $ map (puzzleHash input) [0..]


password :: L.ByteString -> String
password = take 8 . passwordChars


passwordCharPos :: L.ByteString -> Maybe (Int, Char)
passwordCharPos hash
  | "00000" `isPrefixOf` hash && isDigit (hash `index` 5) =
    Just $ (read [hash `index` 5], hash `index` 6)
  | otherwise = Nothing


passwordCharPoss :: L.ByteString -> [(Int, Char)]
passwordCharPoss input =
  catMaybes . map passwordCharPos $ map (puzzleHash input) [0..]


decode' :: String -> [(Int, Char)] -> IO String
decode' acc poss
  | not (any (== '_') acc) = pure acc
  | otherwise =
    case poss of
      (p,c) : rest | validPos p acc -> do
                     let acc' = setAt p c acc
                     print acc'
                     decode' acc' rest
      _ : rest -> decode' acc rest


decode :: L.ByteString -> IO String
decode inp = decode' "________" (passwordCharPoss inp)


validPos :: Int -> String -> Bool
validPos n inp
  | n < length inp = inp !! n == '_'
  | otherwise = False

                 
setAt :: Int -> Char -> String -> String
setAt n c inp = take n inp ++ c : drop (n+1) inp


main :: IO ()
main = do
  print $ password input
  _ <- decode input
  putStrLn "all done"


input :: L.ByteString
input = "reyedfim"
