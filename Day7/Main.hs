{-# LANGUAGE OverloadedStrings  #-}

module Main where

import Data.Maybe (catMaybes, mapMaybe)
import Data.Function (on)
import Data.Ord (comparing)
import Data.List (foldl', sortBy)
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
  print nrTls
  putStrLn "all done"


example :: String
example = "ioxxoj[asdfgh]zxcvbn"
