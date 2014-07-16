module Dice.Parser(
  parseDice
) where

import Data.Maybe
import Dice.AST
import Text.ParserCombinators.Parsec

parseDice :: String -> Either ParseError Sum
parseDice = parse parseComplete "Parse error"

parseComplete :: Parser Sum
parseComplete = do
    ignoreWhitespace
    s <- parseSum
    ignoreWhitespace
    eof
    return s

natural :: Parser Char
natural = oneOf "0123456789"

parseNumber :: Parser Int
parseNumber = do
    a <- natural
    b <- optionMaybe natural
    c <- optionMaybe natural
    d <- optionMaybe natural
    return $ read (a:(catMaybes (b:c:d:[])))

parseRoll :: Parser Roll
parseRoll = do
    x <- parseNumber
    y <- optionMaybe (char 'd' >> parseNumber)
    return $ Roll x y

parseExpr :: Parser Expr
parseExpr = do
    r <- parseRoll
    return $ Expr r
    <|> do
    char '('
    ignoreWhitespace
    x <- many parseSum
    ignoreWhitespace
    char ')'
    return $ Parens $ head x

parseProd :: Parser Product
parseProd = do
    x <- parseExpr
    ignoreWhitespace
    xs <- many (char '*' >> ignoreWhitespace >> parseExpr)
    return $ Product x xs

parseSumType :: Parser SumType
parseSumType = do
    p <- char '+'
    return Plus
    <|> do
    m <- char '-'
    return Minus

parseSum :: Parser Sum
parseSum = do
    x <- parseProd
    ignoreWhitespace
    xs <- many (do
        s <- parseSumType
        ignoreWhitespace
        p <- parseProd
        return (s, p))
    return (Sum x xs)


ignoreWhitespace :: Parser String
ignoreWhitespace = many (char ' ')
