module Dice.Parser(
  parseDice
) where

import Text.ParserCombinators.Parsec
import Dice.AST

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

parseNatural :: Parser String
parseNatural = many1 natural

parseRoll :: Parser Roll
parseRoll = do
    x <- parseNatural
    y <- optionMaybe (char 'd' >> parseNatural)
    return $ Roll (read x) (fmap read y)

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
