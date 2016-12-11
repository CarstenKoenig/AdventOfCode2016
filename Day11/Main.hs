module Main where

import Data.List (delete, lookup, sortBy)
import Data.Function (on)
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
        } deriving (Eq, Show, Ord)


data Direction
  = Up | Down
  deriving (Eq, Show, Ord, Enum)


data EqState =
  EqState { eqElevator :: Int
          , eqFloor1 :: [EqItem]
          , eqFloor2 :: [EqItem]
          , eqFloor3 :: [EqItem]
          , eqFloor4 :: [EqItem]
          } deriving (Eq, Show, Ord)

data EqItem
  = Chip Int
  | Gen Int
  deriving (Eq, Show, Ord)


stateToEq :: State -> EqState
stateToEq state =
  let (eqF1, assocs1) = itemsToEq [] (S.toList $ floor1 state)
      (eqF2, assocs2) = itemsToEq assocs1 (S.toList $ floor2 state)
      (eqF3, assocs3) = itemsToEq assocs2 (S.toList $ floor3 state)
      (eqF4, _) = itemsToEq assocs3 (S.toList $ floor4 state)
  in EqState (elevatorAt state) eqF1 eqF2 eqF3 eqF4
  

itemsToEq :: [(Element, Int)] -> [Item] -> ([EqItem], [(Element, Int)])
itemsToEq assocs [] = ([], assocs)
itemsToEq assocs (i:is) =
  let (i', assocs') = itemToEq assocs i
      (is',assocs'') = itemsToEq assocs' is
  in (i':is',assocs'')


itemToEq :: [(Element, Int)] -> Item -> (EqItem, [(Element, Int)])
itemToEq assocs item =
  let el = element item
      next = length assocs
  in 
    case lookup el assocs of
      Nothing -> (toEq item next, (el,next):assocs)
      Just ind -> (toEq item ind, assocs)
  where toEq (Microchip _) i = Chip i
        toEq (Generator _) i = Gen i


element :: Item -> Element
element (Microchip el) = el
element (Generator el) = el


solutions :: State -> [(Integer, State)]
solutions state =
  findSolutions S.empty [(0,state)]


findSolutions :: Set EqState -> [(Integer, State)] -> [(Integer, State)]
findSolutions _ [] = []
findSolutions visited ((steps, state):nexts) =
  let visited' = S.insert (stateToEq state) visited
      nexts' = nexts ++ (map (\st -> (steps+1,st)) $ possibleFrom visited' state)
      sols = findSolutions visited' (prevereUp nexts')
  in if isSolution state
     then (steps, state) : sols
     else sols
  where
    prevereUp list = sortBy (compare `on` evaluate) list
    evaluate (cnt, state) =
      let pts =
            length (floor3 state)
            + 2 * length (floor2 state)
            + 4 * length (floor1 state)
      in (cnt, pts)



possibleFrom :: Set EqState -> State -> [State]
possibleFrom visited state =
  [ s | d <- directions state
      , (i1, i2o) <- pickItems d state
      , let s = move d i1 i2o state
      , validState s
      , not (S.member (stateToEq s) visited)
  ]


isSolution :: State -> Bool
isSolution state =
  null (floor1 state)
  && null (floor2 state)
  && null (floor3 state)


pickItems :: Direction -> State -> [ (Item, Maybe Item) ]
pickItems Up state =
  pickMaybeTwo (S.toList $ getFloorItems (elevatorAt state) state)
pickItems Down state =
  reverse $ pickMaybeTwo (S.toList $ getFloorItems (elevatorAt state) state)


pickMaybeTwo :: [a] -> [ (a, Maybe a)]
pickMaybeTwo items =
  [ (x, Just y) | (x,items') <- picks items, (y, _) <- picks items' ]
  ++ [ (x, Nothing) | (x,_) <- picks items ]


picks :: [a] -> [(a,[a])]
picks (x:xs) =
  (x,xs) : [ (y,ys) | (y,ys) <- picks xs ]
picks [] = []

  
move :: Direction -> Item -> Maybe Item -> State -> State
move dir item1 item2opt state =
  let start = elevatorAt state
      dest = floorChange dir start
      state' =
        changeFloor (removeItem item2opt . removeItem (Just item1)) start state
      state'' =
        changeFloor (addItem item2opt . addItem (Just item1)) dest state'
  in state'' { elevatorAt = dest }


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


solve :: State -> Maybe Integer
solve state =
  let sols = solutions state
  in case sols of
      [] -> Nothing
      ((steps,_):_) -> Just steps


solvePuzzle :: IO (Maybe Integer)
solvePuzzle = solve <$> puzzle


main :: IO ()
main = do
  steps <- solvePuzzle
  putStrLn $ "It took " ++ show steps ++ " steps"
  putStrLn "all done"


testPuzzle :: State
testPuzzle =
  State
    1
    (S.fromList [Microchip "H", Microchip "L"])
    (S.fromList [Generator "H"])
    (S.singleton (Generator "L"))
    S.empty


testPuzzle' :: State
testPuzzle' =
  State
    2
    (S.fromList [Microchip "L"])
    (S.fromList [Microchip "H"])
    (S.fromList [Generator "H", Generator "L"])
    S.empty


testPuzzle'' :: State
testPuzzle'' =
  State
    3
    (S.fromList [])
    (S.fromList [])
    (S.fromList [Microchip "H", Microchip "L"])
    (S.fromList [Generator "H", Generator "L"])
