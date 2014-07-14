module Dice.Evaluator (
 evalTree
) where

import Data.List
import Dice.AST
import System.Random

type RollDice = RandomGen a => a -> Int -> Int -> [Int]

evalTree :: RandomGen a => RollDice -> a -> Sum -> ([Int], Int)
evalTree = evalSum

evalRoll :: RandomGen a => RollDice -> a -> Roll -> ([Int], Int)
evalRoll r g (Roll x y) = case y of
    Just d -> (z, sum z) where z = r g x d
    Nothing -> ([], x)

evalExpr :: RandomGen a => RollDice -> a -> Expr -> ([Int], Int)
evalExpr r g e = case e of
    Expr x -> evalRoll r g x
    Parens x -> evalSum r g x

evalProd :: RandomGen a => RollDice -> a -> Product -> ([Int], Int)
evalProd r g (Product x xs) = (foldr1 (&*) (map (uncurry (evalExpr r)) (zip (splitMany g) (x:xs))))
    where (ys, y) &* (zs, z) = (ys++zs, y*z)

evalSum :: RandomGen a => RollDice -> a -> Sum -> ([Int], Int)
evalSum r g (Sum x xs) = foldl apply (evalProd r g1 x) (zipWith (evalOp r) (splitMany g2) xs)
    where apply (ys, y) (op, (zs, z)) = case op of
              Plus -> (ys++zs, y+z)
              Minus -> (ys++zs, y-z)
          (g1, g2) = split g

evalOp :: RandomGen a => RollDice -> a -> (SumType, Product) -> (SumType, ([Int], Int))
evalOp r g (op, prod) = (op, evalProd r g prod)

splitMany :: RandomGen a => a -> [a]
splitMany g = case split g of (g1, g2) -> g1:(splitMany g2)
