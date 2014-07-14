module Dice (
  evaluate
) where


import Dice.AST
import Dice.Parser
import Dice.Evaluator
import System.Random

evaluate :: String -> Maybe (IO ([Int], Int))
evaluate input = case parseDice input of
        Left _ -> Nothing
        Right tree -> Just (do
            g <- newStdGen
            return $ evalTree roll g tree)

roll :: RandomGen a => a -> Int -> Int -> [Int]
roll g c d = take c (randomRs (1, d) g)
