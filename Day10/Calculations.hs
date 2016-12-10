module Calculations
  ( steps
  ) where

import Data.List (find)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Map (Map)
import qualified Data.Map as M

import Model


steps :: [(Target, Value)] -> Outputs ->  Bots -> [(Bots, Outputs)]
steps [] outs bots
  | allIdle bots = [(bots, outs)]
steps inps outs bots =
  let (out, bots') = step inps bots
      outs' = output out outs
  in (bots',outs') : steps out outs' bots'


step :: [(Target, Value)] -> Bots -> ([(Target, Value)], Bots)
step inps bots =
  let (out, bots') = process bots
  in (out, dispatch inps bots')


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
