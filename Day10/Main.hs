module Main where

import Parser
import Data.List (find)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Map (Map)
import qualified Data.Map as M


type BotNr = Int

data Bot
  = Bot
  { botNr :: BotNr
  , rule :: (Target, Target)
  , botInput :: [Int]
  }
  deriving (Eq, Show)


newtype Value
  = Value Int
  deriving (Eq, Show)

data Target
  = ToBot BotNr
  | Output Int
  deriving (Eq, Show)


data Instruction
  = BotRule Bot
  | Input Target Value
  deriving (Eq, Show)


type Bots = Map BotNr Bot
type Outputs = Map Int [Int]


feedIn :: Value -> Bot -> Bot
feedIn (Value val) bot =
  bot { botInput = botInput bot ++ [val] }


dispatch :: [(Target, Value)] -> Bots -> Bots
dispatch inps bots =
  foldr update bots inps
  where
    update (Output _, _) = id
    update (ToBot botNr, val) =
      M.adjust (feedIn val) botNr


output :: [(Target, Value)] -> Outputs -> Outputs
output inps outs =
  foldr update outs inps
  where
    update (Output nr, (Value val)) =
      M.alter (\ acc -> Just (val : fromMaybe [] acc)) nr
    update (ToBot _, _) = id


processBot :: Bot -> (Bot, [(Target, Value)])
processBot bot =
  case botInput bot of
    (a:b:rem) ->
      let (low,high) = rule bot
          out = [(low, Value $ min a b), (high, Value $ max a b)]
          bot' = bot { botInput = rem }
      in (bot', out)
    _ -> (bot, [])


process :: Bots -> ([(Target, Value)], Bots)
process = M.mapAccum
  (\acc bot ->
     let (bot', acc') = processBot bot
     in (acc ++ acc', bot'))
  []


step :: [(Target, Value)] -> Bots -> ([(Target, Value)], Bots)
step inps bots =
  let (out, bots') = process bots
  in (out, dispatch inps bots')


steps :: [(Target, Value)] -> Outputs ->  Bots -> [(Bots, Outputs)]
steps [] outs bots
  | allIdle bots = [(bots, outs)]
steps inps outs bots =
  let (out, bots') = step inps bots
      outs' = output out outs
  in (bots',outs') : steps out outs' bots'
  

isIdle :: Bot -> Bool
isIdle = null . botInput


allIdle :: Bots -> Bool
allIdle = all isIdle . M.elems


bots :: [Instruction] -> Bots
bots = foldr add M.empty . catMaybes . map getBot
  where
    add bot map = M.insert (botNr bot) bot map
    getBot (BotRule bot) = Just bot
    getBot _ = Nothing


inputs :: [Instruction] -> [(Target, Value)]
inputs = catMaybes . map getInput
  where getInput (Input botNr val) = Just (botNr, val)
        getInput _ = Nothing


parseInstruction :: Parser Instruction
parseInstruction =
  parseEither parseInput parseBotRule

  
parseInput :: Parser Instruction
parseInput = do
  val <- parseValue
  parseString "goes to "
  bot <- parseBotNr
  return $ Input (ToBot bot) val


parseBotRule :: Parser Instruction
parseBotRule = do
  bot <- parseBotNr
  parseString "gives low "
  low <- parseTarget
  parseString "and high "
  high <- parseTarget
  return . BotRule $ Bot bot (low, high) []
  

parseTarget :: Parser Target
parseTarget = do
  parseString "to "
  parseEither parseOutput (ToBot <$> parseBotNr)
  where
    parseOutput = do
      parseString "output "
      Output <$> parseNumber


parseBotNr :: Parser BotNr
parseBotNr = do
      parseString "bot "
      parseNumber


parseValue :: Parser Value
parseValue = do
      parseString "value "
      Value <$> parseNumber


input :: IO [Instruction]
input = catMaybes . map (eval parseInstruction) . lines <$> readFile "input.txt"


isJonny :: Bot -> Bool
isJonny bot =
  case botInput bot of
    (61 : 17 : _) -> True
    (17 : 61 : _) -> True
    _ -> False


getJonny :: Bots -> Maybe Bot
getJonny = find isJonny . M.elems


findJonny :: [Bots] -> Bot
findJonny (bots : botss) =
  case getJonny bots of
    Just jonny -> jonny
    Nothing -> findJonny botss


work = do
  instr <- input
  let agends = bots instr
  let inps = inputs instr
  return $ steps inps M.empty agends

main :: IO ()
main = do
  wrks <- work
  let jonny = findJonny . map fst $ wrks
  print jonny
  let finalOutputs = snd $ last wrks
  print (M.assocs finalOutputs)
  putStrLn "all done"


test :: String
test = "value 5 goes to bot 2"


test' :: String
test' = "bot 0 gives low to output 2 and high to output 0"
