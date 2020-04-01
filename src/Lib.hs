{-# LANGUAGE LambdaCase #-}
module Lib (jsonParser) where

import Data.Functor
import Data.Foldable
import Data.List
import Data.Char
import Numeric
import Control.Monad
import Control.Applicative
import System.IO

someFunc :: IO ()
someFunc = putStrLn "someFunc"

jsonParser :: IO ()
jsonParser = do
    putStr "Enter a file name: "
    hFlush stdout
    x <- getLine
    y <- readFile x
    print (parse json y)

newtype Parser a = P { parse :: String -> ParseResult a}

data ParseResult a = UnexpectedString String
                   | UnexpectedChar Char
                   | UnexpectedEof
                   | ExpectedEof
                   | Result a String

data JSON = JSNull
          | JSBool Bool
          | JSNumber Float
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

string :: String -> Parser String
string = mapM is

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

stringTok :: String -> Parser String
stringTok = (>>) spaces . string

choice :: [Parser a] -> Parser a
choice = asum

between :: Char -> Char -> Parser a -> Parser a
between a b c = charTok a *> c <* charTok b

sepBy :: Char -> Parser a -> Parser [a]
sepBy c a = (:) <$> a <*> list (charTok c >> spaces >> a)

betweenSepBy :: Char -> Char -> Char -> Parser a -> Parser [a]
betweenSepBy a b c x = between a b (sepBy c x)

noneOf :: String -> Parser Char
noneOf x = satisfy (`notElem` x)

satisfyAll :: [Char -> Bool] -> Parser Char
satisfyAll fs = satisfy (\x -> all ($ x) fs)

jsonNull :: Parser JSON
jsonNull = JSNull <$ string "null"

jsonBool :: Parser JSON
jsonBool = JSBool True <$ stringTok "true" <|> JSBool False <$ stringTok "false"
 
data SpecialCharacter =
    BackSpace
    | FormFeed
    | NewLine
    | CarriageReturn
    | Tab
    | VerticalTab
    | SingleQuote
    | DoubleQuote
    | Backslash
    deriving (Eq, Ord, Show)

fromSpecialCharacter ::
  SpecialCharacter
  -> Char
fromSpecialCharacter BackSpace =
  chr 0x08
fromSpecialCharacter FormFeed =
  chr 0x0C
fromSpecialCharacter NewLine =
  '\n'
fromSpecialCharacter CarriageReturn =
  '\r'
fromSpecialCharacter Tab =
  '\t'
fromSpecialCharacter VerticalTab =
  '\v'
fromSpecialCharacter SingleQuote =
  '\''
fromSpecialCharacter DoubleQuote =
  '"'
fromSpecialCharacter Backslash =
  '\\'

toSpecialCharacter ::
  Char
  -> Maybe SpecialCharacter
toSpecialCharacter c =
  let table = [('b', BackSpace),
              ('f', FormFeed),
              ('n', NewLine),
              ('r', CarriageReturn),
              ('t', Tab),
              ('v', VerticalTab),
              ('\'', SingleQuote),
              ('"' , DoubleQuote),
              ('\\', Backslash)]
  in snd <$> find ((==) c . fst) table

specialCharacter :: Parser SpecialCharacter
specialCharacter = is '\\' >> P f
  where f [] = UnexpectedEof
        f (x:xs) = case toSpecialCharacter x of
                      Nothing -> UnexpectedChar x
                      Just y -> Result y xs

hex ::
  Parser Char
hex = f <$> replicateM 4 (satisfy isHexDigit)
  where f x = case readHex x of
              [(a, _)] -> chr a
              _  -> chr 0

hexu ::
  Parser Char
hexu = is 'u' >> hex

jsonString_ ::
  Parser String
jsonString_ = between '"' '"' (list (specialParser <|> hexParser <|> nonSpecialCharacters))
  where specialParser = fromSpecialCharacter <$> specialCharacter
        hexParser = is '\\' >> hexu
        nonSpecialCharacters = satisfyAll [isPrint, (/='"'), (/='\\')]

jsonString :: Parser JSON
jsonString = JSString <$> jsonString_

jsonNumber :: Parser JSON
jsonNumber = JSNumber <$> P f
    where f x = case readSigned readFloat x of
                    [(a, y)] -> Result a y
                    _        -> UnexpectedString x

jsonArray :: Parser JSON
jsonArray = JSArray <$> betweenSepBy '[' ']' ',' (spaces *> json)

jsonObject :: Parser JSON
jsonObject = JSObject <$> betweenSepBy '{' '}' ',' kvs
    where kvs = (,) <$> (spaces *> jsonString_ <* spaces <* charTok ':') <*> json

json :: Parser JSON
json = choice [jsonNull, jsonBool, jsonString, jsonNumber, jsonArray, jsonObject]
