module Main where

import Data.Maybe (catMaybes, mapMaybe)
import Data.List (intersect)

import Parser


data IP7Block
  = Supernet String
  | Hypernet String
  deriving Show

newtype IP7
  = IP7 [IP7Block]
  deriving Show


isTLS :: IP7 -> Bool
isTLS (IP7 blocks) =
  not (any containsABBA $ hypernets blocks)
  && (any containsABBA $ supernets blocks)


isSSL :: IP7 -> Bool
isSSL (IP7 blocks) =
  let abs = concatMap abas $ supernets blocks
      bas = concatMap babs $ hypernets blocks
  in not (null $ abs `intersect` bas)


hypernets :: [IP7Block] -> [String]
hypernets = mapMaybe pickHyper
  where pickHyper (Hypernet b) = Just b
        pickHyper _ = Nothing
  

supernets :: [IP7Block] -> [String]
supernets = mapMaybe pickSupers
  where pickSupers (Supernet b) = Just b
        pickSupers _ = Nothing
  

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
babs = map invert . abas
  where invert [a,b,_] = [b,a,b]


ip7Parser :: Parser IP7
ip7Parser = do
  blocks <- parseMany (parseEither hyperParser superParser)
  return $ IP7 blocks


hyperParser :: Parser IP7Block  
hyperParser = do
  _ <- parseChar (== '[')
  i <- parseAlphas
  _ <- parseChar (== ']')
  if null i
    then failParse
    else return $ Hypernet i


superParser :: Parser IP7Block  
superParser = do
  i <- parseAlphas
  if null i
    then failParse
    else return $ Supernet i


input :: IO [String]
input = lines <$> readFile "input.txt"


main :: IO ()
main = do
  addresses <- catMaybes . fmap (eval ip7Parser) <$> input
  
  putStr "number TLS: "
  let nrTLS = length $ filter isTLS addresses
  print nrTLS

  putStr "number SSL: "
  let nrSSL = length $ filter isSSL addresses
  print nrSSL
  
  putStrLn "all done"


example :: String
example = "ioxxoj[asdfgh]zxcvbn"


example' :: String
example' = "zazbz[bzb]cdb"
