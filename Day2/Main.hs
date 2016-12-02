module Main where

import Data.List (foldl')
import Data.Char (toLower, isDigit)

data Move
  = MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight
  deriving Show

parseMove :: String -> Maybe (Move, String)
parseMove (c : remainder)
  | toLower c == 'l' = Just (MoveLeft, remainder)
  | toLower c == 'r' = Just (MoveRight, remainder)
  | toLower c == 'u' = Just (MoveUp, remainder)
  | toLower c == 'd' = Just (MoveDown, remainder)
  | otherwise = Nothing
parseMove "" = Nothing  


parseMoves :: String -> [Move]
parseMoves input =
  case parseMove input of
    Nothing -> []
    Just (cmd, remainder) -> cmd : parseMoves remainder


data Row = Row
  { left :: [Char]
  , current :: Char
  , right :: [Char]
  }
  deriving Show

data KeyPad = KeyPad
  { over :: [Row]
  , row :: Row
  , under :: [Row]
  }
  deriving Show

getNumber :: KeyPad -> Char
getNumber = current . row

moveRow :: Move -> Row -> Row
moveRow MoveLeft (Row (l:ls) c rs) =
  Row ls l (c:rs)
moveRow MoveRight (Row ls c (r:rs)) =
  Row (c:ls) r rs
moveRow _ r = r

move :: KeyPad -> Move -> KeyPad
move (KeyPad (o:os) r us) MoveUp
  | current o /= '0' = KeyPad os o (r:us)
move (KeyPad os r (u:us)) MoveDown
  | current u /= '0' = KeyPad (r:os) u us
move kp@(KeyPad os r us) mv =
  let r' = moveRow mv r
  in if current r' /= '0'
     then KeyPad (fmap (moveRow mv) os) r' (fmap (moveRow mv) us)
     else kp

normalPad :: KeyPad
normalPad = KeyPad [] (Row [] '1' "23") [Row [] '4' "56", Row [] '7' "89"]

fancyPad :: KeyPad
fancyPad = KeyPad
  []
  (Row "00" '1' "00")
  [ Row "20" '3' "40"
  , Row "65" '7' "89"
  , Row "A0" 'B' "C0"
  , Row "00" 'D' "00"
  ]

moves :: KeyPad -> [Move] -> KeyPad
moves s = foldl' move s

start5 :: KeyPad
start5 = moves fancyPad [MoveDown, MoveDown, MoveLeft, MoveLeft]

addNumber :: (KeyPad, String) -> [Move] -> (KeyPad, String)
addNumber (kp, acc) mvs =
  let kp' = moves kp mvs
  in (kp', getNumber kp' : acc)

findNumber :: [[Move]] -> String
findNumber mvss =
  let (_, nrs) = foldl' addNumber (start5, "") mvss
  in reverse nrs

getLines :: IO [String]
getLines = do
  content <- getContents
  return $ lines content

main :: IO ()
main = do
  numbers <- fmap parseMoves <$> getLines
  print (findNumber numbers)
