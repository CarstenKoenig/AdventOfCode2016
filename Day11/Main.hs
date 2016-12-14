module Main where

import Data.List (delete, lookup, groupBy, sort)
import Data.Function (on)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S

import Model
import Parser (eval)
import Astar


aStarParams :: Parameter State EqState
aStarParams =
  Parameter heur neigh isSolution toEq
  where
    heur start =
      3 * length (floor1 start) +
      2 * length (floor2 start) +
      1 * length (floor3 start)
      
    neigh node =
      [ s | d <- directions node
      , (i1, i2o) <- pickItems d node
      , let s = move d i1 i2o node
      , validState node ]


solveFrom :: State -> [State]
solveFrom = aStar aStarParams


answerFrom :: State -> Int
answerFrom = (+  (-1)) . length . solveFrom


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


data EqState = EqState Int [(Int,Int)]
  deriving (Show, Eq, Ord)

data EqTyp = G | M
  deriving (Show, Eq, Ord)

toEq :: State -> EqState
toEq state = EqState (elevatorAt state) (pat state)
  where
    pat state =
      sort .
      map (\ [(_,a),(_,b)] -> (a,b)) .
      groupBy ((==) `on` fst) . sort $ elemFloor state


elemFloor :: State -> [(Element, Int)]
elemFloor state =
  concat [ col 1 (floor1 state)
         , col 2 (floor2 state)
         , col 3 (floor3 state)
         , col 4 (floor4 state)
         ]
  where col f its =
          map (\ i -> (element i, f)) $ S.toList its


element :: Item -> Element
element (Microchip el) = el
element (Generator el) = el


typ :: Item -> EqTyp
typ (Microchip _) = M
typ (Generator _) = G


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


extPuzzle :: IO State
extPuzzle = do
  pz <- puzzle
  let add =
        S.fromList
        [ Microchip "elerium", Generator "elerium"
        , Microchip "dilithium", Generator "dilithium"
        ]
  return $ pz { floor1 = S.union (floor1 pz) add }


solvePuzzle :: IO Int
solvePuzzle = answerFrom <$> puzzle


solvePuzzleExt :: IO Int
solvePuzzleExt = answerFrom <$> extPuzzle


main :: IO ()
main = do
  steps <- solvePuzzle
  putStrLn $ "Part 1: It took " ++ show steps ++ " steps"
  steps2 <- solvePuzzleExt
  putStrLn $ "Part 2: It took " ++ show steps2 ++ " steps"
  putStrLn "all done"


testPuzzle :: State
testPuzzle =
  State
    1
    (S.fromList [Microchip "H", Microchip "L"])
    (S.fromList [Generator "H"])
    (S.singleton (Generator "L"))
    S.empty
