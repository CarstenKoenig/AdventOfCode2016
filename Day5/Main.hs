{-# LANGUAGE OverloadedStrings  #-}

module Main where

import Data.Maybe (catMaybes)

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


main :: IO ()
main = do
  print $ password input


input :: L.ByteString
input = "reyedfim"
