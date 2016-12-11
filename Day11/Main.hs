module Main where

import Data.List (delete)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S

import Model
import Parser (eval)


data State =
  State { elevatorAt :: Int
        , floor1 :: Set Item
        , floor2 :: Set Item
        , floor3 :: Set Item
        , floor4 :: Set Item
        } deriving (Eq, Show)


data Direction
  = Up | Down
  deriving (Eq, Show, Ord, Enum)


move :: Direction -> Item -> Maybe Item -> State -> State
move dir item1 item2opt state =
  let start = elevatorAt state
      dest = floorChange dir start
      state' =
        changeFloor (removeItem item2opt . removeItem (Just item1)) start state
      state'' =
        changeFloor (addItem item2opt . addItem (Just item1)) dest state'
  in state''


validState :: State -> Bool
validState state =
  allChipsSafe (floor1 state)
  && allChipsSafe (floor2 state)
  && allChipsSafe (floor3 state)
  && allChipsSafe (floor4 state)


allChipsSafe :: Set Item -> Bool
allChipsSafe items =
  let gens = generators items
      chips = microchips items
      notShielded = chips `S.difference` gens
  in S.null notShielded || S.null gens


microchips :: Set Item -> Set Element
microchips = S.fromList . mapMaybe pickChips . S.toList
  where pickChips (Microchip el) = Just el
        pickChips _ = Nothing


generators :: Set Item -> Set Element
generators = S.fromList . mapMaybe pickGens . S.toList
  where pickGens (Generator el) = Just el
        pickGens _ = Nothing


changeFloor :: (Set Item -> Set Item) -> Int -> State -> State
changeFloor update floor state =
  let items = getFloorItems floor state
  in setFloorItems floor (update items) state


setFloorItems :: Int -> Set Item -> State -> State
setFloorItems 1 itms state = state { floor1 = itms }
setFloorItems 2 itms state = state { floor2 = itms }
setFloorItems 3 itms state = state { floor3 = itms }
setFloorItems 4 itms state = state { floor4 = itms }
setFloorItems _ _ state = state


getFloorItems :: Int -> State -> Set Item
getFloorItems 1 = floor1
getFloorItems 2 = floor2
getFloorItems 3 = floor3
getFloorItems 4 = floor4
getFloorItems _ = const (S.empty)


addItem :: Maybe Item -> Set Item -> Set Item
addItem Nothing = id
addItem (Just item) = S.insert item


removeItem :: Maybe Item -> Set Item -> Set Item
removeItem Nothing = id
removeItem (Just item) = S.delete item


floorChange :: Direction -> (Int -> Int)
floorChange Up = (+1)
floorChange Down = (+ (-1))


moveElevator :: Direction -> State -> State
moveElevator Up state = state { elevatorAt = elevatorAt state + 1 }
moveElevator Down state = state { elevatorAt = elevatorAt state - 1 }


directions :: State -> [Direction]
directions state =
  case elevatorAt state of
    1 -> [Up]
    4 -> [Down]
    _ -> [Up, Down]


input :: IO [String]
input = lines <$> readFile "input.txt"


puzzle :: IO State
puzzle = do
  [ (_, f1), (_, f2), (_, f3), (_, f4)] <-
    catMaybes . fmap (eval parseFloor) <$> input
  return $ State 1 f1 f2 f3 f4

main :: IO ()
main = do
  floorInputs <- input
  putStrLn "all done"
