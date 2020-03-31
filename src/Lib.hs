{-# LANGUAGE LambdaCase #-}
module Lib where

import Data.Functor
import Data.Foldable
import Data.Char
import Control.Monad
import Control.Applicative

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype Parser a = P { parse :: String -> ParseResult a}

data ParseResult a = UnexpectedString String
                   | UnexpectedChar Char
                   | UnexpectedEof
                   | ExpectedEof
                   | Result a String

data JSON = JSNull
          | JSBool Bool
          | JSNumber Int
          | JSString String
          | JSArray [JSON]
          | JSObject [(String,JSON)]
          deriving (Show, Eq)

instance Functor Parser where
    f `fmap` P p = P (\s -> case p s of
                            Result a i -> Result (f a) i
                            UnexpectedChar x -> UnexpectedChar x
                            UnexpectedString x -> UnexpectedString x
                            UnexpectedEof -> UnexpectedEof
                            ExpectedEof -> ExpectedEof)

instance Show a => Show (ParseResult a) where
    show (Result x y) = "Result" ++ " >" ++ show x ++ "< " ++ y
    show (UnexpectedChar x) = "Unexpected Character: " ++ [x]
    show (UnexpectedString x) = "Unexpected String: " ++ x
    show UnexpectedEof = "Unexpected EOF"
    show ExpectedEof = "Expected EOF"

valueParser :: a -> Parser a
valueParser x = P (Result x)

instance Applicative Parser where
    pure = valueParser
    f <*> a = f >>= ( <$> a)


instance Monad Parser where
    return = pure
    P p >>= f = P f'
        where f' s = case p s of
                        Result a i -> parse (f a) i
                        UnexpectedChar x -> UnexpectedChar x
                        UnexpectedString x -> UnexpectedString x
                        UnexpectedEof -> UnexpectedEof
                        ExpectedEof -> ExpectedEof

onErrorResult :: Parser a -> String -> Bool
onErrorResult a b = case parse a b of Result _ _ -> False
                                      _          -> True

instance Alternative Parser where
    empty = P (const ExpectedEof)
    a <|> b = P f
        where f s = case parse a s of
                        Result x y -> Result x y
                        _          -> parse b s

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = P (\case [] -> UnexpectedEof
                     (x:xs) -> if f x
                                then Result x xs
                                else UnexpectedChar x)

is :: Char -> Parser Char
is = satisfy . (==)

list1 :: Parser a -> Parser [a]
list1 = some

list :: Parser a -> Parser [a]
list = many

space :: Parser Char
space = satisfy isSpace

spaces :: Parser String
spaces = list space

charTok :: Char -> Parser Char
charTok = (>>) spaces . is

choice :: [Parser a] -> Parser a
choice = asum

noneOf :: String -> Parser Char
noneOf x = satisfy (`notElem` x)
