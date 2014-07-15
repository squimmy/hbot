{-# LANGUAGE RankNTypes #-}

module Dice.Evaluator
( evalTree
, Die(Die)
) where

import Data.List
import Dice.AST
import System.Random

data Die = Die { sides :: Int
               , value :: Int
               } deriving Show

type RollDice = RandomGen a => a -> Int -> Int -> [Die]

evalTree :: RandomGen a => RollDice -> a -> Sum -> ([Die], Int)
evalTree = evalSum

evalRoll :: RandomGen a => RollDice -> a -> Roll -> ([Die], Int)
evalRoll r g (Roll x y) = case y of
    Just d -> (z, foldl add 0 z)
        where z = r g x d
              add n (Die _ v) = n + v
    Nothing -> ([], x)

evalExpr :: RandomGen a => RollDice -> a -> Expr -> ([Die], Int)
evalExpr r g e = case e of
    Expr x -> evalRoll r g x
    Parens x -> evalSum r g x

evalProd :: RandomGen a => RollDice -> a -> Product -> ([Die], Int)
evalProd r g (Product x xs) = (foldr1 (&*) (map (uncurry (evalExpr r)) (zip (splitMany g) (x:xs))))
    where (ys, y) &* (zs, z) = (ys++zs, y*z)

evalSum :: RandomGen a => RollDice -> a -> Sum -> ([Die], Int)
evalSum r g (Sum x xs) = foldl apply (evalProd r g1 x) (zipWith (evalOp r) (splitMany g2) xs)
    where apply (ys, y) (op, (zs, z)) = case op of
              Plus -> (ys++zs, y+z)
              Minus -> (ys++zs, y-z)
          (g1, g2) = split g

evalOp :: RandomGen a => RollDice -> a -> (SumType, Product) -> (SumType, ([Die], Int))
evalOp r g (op, prod) = (op, evalProd r g prod)

splitMany :: RandomGen a => a -> [a]
splitMany g = case split g of (g1, g2) -> g1:(splitMany g2)
