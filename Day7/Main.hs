{-# LANGUAGE OverloadedStrings  #-}

module Main where

import Data.Maybe (catMaybes, mapMaybe)
import Data.Function (on)
import Data.Ord (comparing)
import Data.List (foldl', sortBy, intersect)
import Data.Map (Map)
import qualified Data.Map as M

import Parser


data IP7Block
  = Normal String
  | Hypernet String
  deriving Show

newtype IP7
  = IP7 [IP7Block]
  deriving Show


isTLS :: IP7 -> Bool
isTLS (IP7 blocks) =
  not (any containsABBA $ hypernets blocks)
  && (any containsABBA $ normals blocks)


isSSL :: IP7 -> Bool
isSSL (IP7 blocks) =
  let abs = concatMap abas $ normals blocks
      bas = concatMap babs $ hypernets blocks
  in not (null $ abs `intersect` bas)


hypernets :: [IP7Block] -> [String]
hypernets = mapMaybe pickHyper
  where pickHyper (Hypernet b) = Just b
        pickHyper _ = Nothing
  

normals :: [IP7Block] -> [String]
normals = mapMaybe pickNormal
  where pickNormal (Normal b) = Just b
        pickNormal _ = Nothing
  

containsABBA :: String -> Bool
containsABBA (a:xs@(b:b':a':_))
  | a == a' && a /= b && b == b' = True
  | otherwise = containsABBA xs
containsABBA _ = False


abas :: String -> [String]
abas (a:xs@(b:a':_))
  | a == a' && a /= b = [a,b,a] : abas xs
  | otherwise = abas xs
abas _ = []


babs :: String -> [String]
babs (b:xs@(a:b':_))
  | b == b' && a /= b = [a,b,a] : babs xs
  | otherwise = babs xs
babs _ = []


ip7Parser :: Parser IP7
ip7Parser = do
  blocks <- parseMany (parseEither hyperParser normalParser)
  return $ IP7 blocks


hyperParser :: Parser IP7Block  
hyperParser = do
  _ <- parseChar (== '[')
  i <- parseAlphas
  _ <- parseChar (== ']')
  if null i
    then failParse
    else return $ Hypernet i


normalParser :: Parser IP7Block  
normalParser = do
  i <- parseAlphas
  if null i
    then failParse
    else return $ Normal i


input :: IO [String]
input = lines <$> readFile "input.txt"


main :: IO ()
main = do
  addresses <- catMaybes . fmap (eval ip7Parser) <$> input
  let nrTls = length $ filter isTLS addresses
  let nrSSL = length $ filter isSSL addresses
  print nrTls
  print nrSSL
  putStrLn "all done"


example :: String
example = "ioxxoj[asdfgh]zxcvbn"

example' :: String
example' = "zazbz[bzb]cdb"
