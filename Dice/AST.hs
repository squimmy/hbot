module Dice.AST (
  Roll(Roll)
, Expr(Expr,Parens)
, Product(Product)
, Sum(Sum)
, SumType(Plus,Minus)
) where

data Roll    = Roll Int (Maybe Int) deriving Show
data Expr    = Expr Roll
             | Parens Sum deriving Show
data Product = Product Expr [Expr] deriving Show
data Sum     = Sum Product [(SumType, Product)] deriving Show
data SumType = Plus
             | Minus deriving Show

{-
  Digit      = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
  Number     = Digit , { Digit }
  Roll       = Number , [ "d" , Number ]
  Expr       = Roll | "(" , Whitespace , Expr , Whitespace , ")"
  Product    = Expr , { "*" , Expr }
  Sum        = Product , { Whitespace , SumType , Whitespace , Product }
  SumType    = "+" | "-"
  Whitespace = { " " }
-}

