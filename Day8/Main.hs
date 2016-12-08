module Main where

import Data.List (transpose, foldl')
import Data.Maybe (catMaybes)

import Parser

newtype Screen = Screen [[Bool]]

emptyScreen :: Screen
emptyScreen =
  Screen $ replicate 6 (replicate 50 False)


instance Show Screen where
  show (Screen sc) = unlines . fmap (fmap showPixel) $ sc
    where showPixel True = '#'
          showPixel False = '.'


data Action
  = Rect (Int,Int)
  | RotateRow Int Int
  | RotateCol Int Int
  deriving Show


apply :: Screen -> Action -> Screen
apply (Screen sc) (Rect (nrX, nrY)) =
  Screen $ blockOn sc nrY nrX
apply (Screen sc) (RotateRow row nr) =
  Screen $ rotateRow row nr sc
apply (Screen sc) (RotateCol col nr) =
  Screen . transpose . rotateRow col nr . transpose $ sc


rotateRow :: Int -> Int -> [[Bool]] -> [[Bool]]
rotateRow r nr sc =
  let (before, row, after) = pickRow sc r
  in before ++ (rotate row nr) : after


rotate :: [a] -> Int -> [a]
rotate xs n =
  let l = length xs
  in take l . drop (l-n) $ cycle xs
  

pickRow :: [a] -> Int -> ( [a], a, [a] )
pickRow xss r =
  let before = take r xss
      (row:after) = drop r xss
  in (before,row,after)
  

blockOn :: [[Bool]] -> Int -> Int -> [[Bool]]
blockOn sc nrY nrX =
  fmap (turnOn nrX) (take nrY sc)
  ++ drop nrY sc
  

turnOn :: Int -> [Bool] -> [Bool]
turnOn nr pxs =
  replicate nr True ++ drop nr pxs

parseAction :: Parser Action
parseAction =
  parseEither parseRect parseRotate

parseRect :: Parser Action
parseRect = do
  parseString "rect "
  a <- parseNumber
  _ <- parseChar (== 'x')
  b <- parseNumber
  return $ Rect (a,b)


parseRotate :: Parser Action
parseRotate = do
  parseString "rotate "
  const <- parseType
  a <- parseNumber
  parseString "by"
  b <- parseNumber
  return $ const a b


parseType :: Parser (Int -> Int -> Action)
parseType = do
  parseEither parseCol parseRow
  where parseCol = parseString "column x=" >> pure RotateCol
        parseRow = parseString "row y=" >> pure RotateRow

input :: IO [String]
input = lines <$> readFile "input.txt"


main :: IO ()
main = do
  actions <- catMaybes . fmap (eval parseAction) <$> input
  let (Screen screen) = foldl' apply emptyScreen actions
  let numberOn = length . filter id $ concat screen
  putStrLn $ "there are " ++ show numberOn ++ " pixels on"
  print (Screen screen)
  putStrLn "all done"
