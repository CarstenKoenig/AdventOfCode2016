module Main where

import Data.Maybe (catMaybes)
import qualified Data.Map as M

import Model
import Calculations
import Parser (eval)


input :: IO [Instruction]
input = catMaybes . map (eval parseInstruction) . lines <$> readFile "input.txt"


main :: IO ()
main = do
  
  wrks <- work
  let jonny = findJonny . map fst $ wrks
  print jonny
  let finalOutputs = snd $ last wrks
  print (M.assocs finalOutputs)
  putStrLn "all done"
  
  where
    
    work = do
      instr <- input
      let agends = bots instr
      let inps = inputs instr
      return $ steps inps M.empty agends



test :: String
test = "value 5 goes to bot 2"


test' :: String
test' = "bot 0 gives low to output 2 and high to output 0"
