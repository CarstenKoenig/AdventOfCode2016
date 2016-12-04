module Main where

import Data.List (foldl', concatMap, transpose)
import Data.Maybe (catMaybes)
import Data.Char (toLower, isDigit, isSpace)



data Triangle
  = Triangle Int Int Int
  deriving Show


parseDigits :: String -> Maybe (String, String)
parseDigits "" = Just ("", "")
parseDigits (c : remainder)
  | isDigit c = do
      (ds, rem) <- parseDigits remainder
      return (c:ds, rem)
  | otherwise = Just ("", c:remainder)


parseNumber :: String -> Maybe (Int, String)
parseNumber input = do
  ((), rem) <- parseWhiteSpace input
  (ds, rem') <- parseDigits rem
  ((), rem'') <- parseWhiteSpace rem'
  case ds of
    "" -> Nothing
    ds -> Just (read ds, rem'')


parseWhiteSpace :: String -> Maybe ((), String)
parseWhiteSpace "" = Just ((), "")
parseWhiteSpace input@(c : remainder)
  | isSpace c = parseWhiteSpace remainder
  | otherwise = Just ((), input)


parseTriangle :: String -> Maybe (Triangle, String)
parseTriangle input =
  case parseNumber input of
    Nothing -> Nothing
    Just (nr1, rem) ->
      case parseNumber rem of
        Nothing -> Nothing
        Just (nr2, rem2) ->
          case parseNumber rem2 of
            Nothing -> Nothing
            Just (nr3, rem3) ->
              Just (Triangle nr1 nr2 nr3, rem3)


readThreeNumbers :: String -> Maybe [Int]
readThreeNumbers input =
  case parseNumber input of
    Nothing -> Nothing
    Just (nr1, rem) ->
      case parseNumber rem of
        Nothing -> Nothing
        Just (nr2, rem2) ->
          case parseNumber rem2 of
            Nothing -> Nothing
            Just (nr3, rem3) ->
              Just [nr1, nr2, nr3]


justNumbers :: [String] -> [[Int]]
justNumbers = catMaybes . map readThreeNumbers


readBlocks :: [[Int]] -> [[[Int]]]
readBlocks (a:b:c:rest) = [a,b,c] : readBlocks rest
readBlocks rest = [rest]


toTriangles :: [[Int]] -> [Triangle]
toTriangles = map toTriangle
  where toTriangle [a,b,c] = Triangle a b c
  

getLines :: IO [String]
getLines = do
  content <- getContents
  return $ lines content


probableTriangle :: Triangle -> Bool
probableTriangle (Triangle a b c) =
  a+b > c && a+c > b && b+c > a


main :: IO ()
main = do
  lines <- getLines
  let blocks = readBlocks . justNumbers $ lines
  let triangles = concatMap toTriangles $ map transpose blocks
  let count = length $ filter probableTriangle triangles
  print count


testInput :: String
testInput = "    4   21  894"
