module Model
  ( Item (..)
  , Element, Floor
  , parseFloor
  ) where


import Parser
import Data.Maybe (catMaybes)


data Item
  = Generator Element
  | Microchip Element
  deriving (Show, Eq)


parseItems :: Parser [Item]
parseItems = do
  parseList sepP parseItem
  where
    sepP = parseString ", "


parseItem :: Parser Item
parseItem = do
  parseEither (parseString "and a ") (parseString "a ")
  parseEither parseGenerator parseMicrochip
    where
      
      parseGenerator = do
        el <- parseElement
        parseString " generator"
        return $ Generator el
        
      parseMicrochip = do
        el <- parseElement
        parseString "-compatible microchip"
        return $ Microchip el


type Element = String


parseElement :: Parser Element
parseElement = parseAlphas


type Floor = String


parseFloor :: Parser (Floor, [Item])
parseFloor = do
  floor <- parseFloorIntro
  items <- parseEither nothingP parseItems
  return (floor, items)
  where
    nothingP = parseString "nothing relevant." >> pure []


parseFloorIntro :: Parser Floor
parseFloorIntro = do
  parseString "The "
  floor <- parseAlphas
  parseString " floor contains "
  return floor
