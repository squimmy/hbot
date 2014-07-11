module Dice.Evaluator (
 evalTree
) where

import Data.List
import Dice.AST
import System.Random

type RollDice = Int -> Int -> [Int]

evalTree = evalSum

evalRoll :: RollDice -> Roll -> ([Int], Int)
evalRoll r (Roll x y) = case y of
    Just d -> (z, sum z) where z = (r x d)
    Nothing -> ([], x)

evalExpr :: RollDice -> Expr -> ([Int], Int)
evalExpr r e = case e of
    Expr x -> evalRoll r x
    Parens x -> evalSum r x

evalProd :: RollDice -> Product -> ([Int], Int)
evalProd r (Product x xs) = (foldr1 (&*) (map (evalExpr r) (x:xs)))
    where (ys, y) &* (zs, z) = (ys++zs, y*z)

evalSum :: RollDice -> Sum -> ([Int], Int)
evalSum r (Sum x xs) = do
    foldl1 (&-) ((foldr1 (&+) ((evalProd r x):p)):m)
    where (p, m) = tmap (map snd) (partition (isPlus . fst) (map (mapSnd (evalProd r)) xs))
          isPlus x = case x of
              Plus -> True
              _ -> False
          mapSnd f (a, b) = (a, f b)
          tmap f (a, b) = (f a, f b)
          (ys, y) &+ (zs, z) = (ys++zs, y+z)
          (ys, y) &- (zs, z) = (ys++zs, y-z)

