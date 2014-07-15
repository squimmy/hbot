{-# LANGUAGE FlexibleInstances #-}

module Irc.Formatting
( Colour(..)
, Font(..)
, format
) where

class Encode a where
    encode :: a -> String

data Colour = White
            | Black
            | Blue
            | Green
            | Red
            | Brown
            | Purple
            | Orange
            | Yellow
            | Lime
            | Teal
            | Cyan
            | LightBlue
            | Pink
            | Grey
            | Silver deriving Show

instance Encode Colour where
    encode c = "\ETX" ++ (getCode c)

instance Encode (Colour, Colour) where
    encode (c, c') = "\ETX" ++ (getCode c) ++ "," ++ (getCode c')

getCode :: Colour -> String
getCode c = case c of
    White -> "00"
    Black -> "01"
    Blue -> "02"
    Green -> "03"
    Red -> "04"
    Brown -> "05"
    Purple -> "06"
    Orange -> "07"
    Yellow -> "08"
    Lime -> "09"
    Teal -> "10"
    Cyan -> "11"
    LightBlue -> "12"
    Pink -> "13"
    Grey -> "14"
    Silver -> "15"

data Font = Bold
          | Italic
          | Underline deriving Show

instance Encode Font where
    encode f = case f of
        Bold -> "\STX"
        Italic -> "\SYN"
        Underline -> "\US"


format :: Encode a => a -> String -> String
format e x = (encode e) ++ x ++ "\SI"

