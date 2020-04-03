{-# LANGUAGE ScopedTypeVariables #-}
module LibSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.Char
import Lib

spec :: Spec
spec = do
    describe "valueParser" $ do
        prop "returns a parser that always a success and returns the same value" $
            \(x :: Int) -> show (parse (valueParser x) "abc") === "Result >" ++ show x ++ "< abc"
        prop "returns a parser that consumes no input" $
            \(x :: String) -> show (parse (valueParser ()) x) === "Result >()< " ++ x

    describe "satisfy" $ do
        it "returns a parser that checks for the given condition" $
            show (parse (satisfy isUpper) "ABC") `shouldBe` "Result >'A'< BC"
        it "returns an error on unexpected input" $
            onErrorResult (satisfy isUpper) "aBC" `shouldBe` True
    
    describe "is" $ do
        it "returns a parser that checks for the given character" $
            show (parse (is 'A') "ABC") `shouldBe` "Result >'A'< BC"
        it "returns an error on unexpected input" $
            onErrorResult (is 'A') "aBC" `shouldBe` True

    describe "string" $ do
        it "returns a parser that checks for the given string" $
            show (parse (string "ABC") "ABCDEF") `shouldBe` "Result >\"ABC\"< DEF"
        it "returns an error on unexpected input" $
            onErrorResult (string "ABC") "aBCDEF" `shouldBe` True

    describe "list1" $ do
        it "returns a parser that checks for one or more occurrences" $
            show (parse (list1 (is 'A')) "AAAa") `shouldBe` "Result >\"AAA\"< a"
        it "returns an error on no match" $
            onErrorResult (list1 (is 'A')) "aAAA" `shouldBe` True

    describe "list" $ do
        it "returns a parser that checks for zero or more occurrences" $
            show (parse (list (is 'A')) "AAAa") `shouldBe` "Result >\"AAA\"< a"
        it "returns a success even on no match" $
            onErrorResult (list (is 'A')) "aAAA" `shouldBe` False

    describe "space" $ do
        it "returns a parser that checks for a space character" $
            show (parse space " a") `shouldBe` "Result >' '< a"
        it "returns a parser that checks for a space character" $
            show (parse space "\na") `shouldBe` "Result >'\\n'< a"
        it "returns an error on no space character" $
            onErrorResult space "aAAA" `shouldBe` True

    describe "spaces" $ do
        it "returns a parser that checks for zero or more spaces" $
            show (parse spaces " \na") `shouldBe` "Result >\" \\n\"< a"
        it "returns no error when no space character is given" $
            onErrorResult spaces "aAAA" `shouldBe` False

    describe "charTok" $ do
        it "returns a parser that ignores the initial spaces and checks for the given character" $
            show (parse (charTok 'a') "  \na") `shouldBe` "Result >'a'< "
        it "returns an error if character is not found after zero or more spaces" $
            onErrorResult (charTok 'a') "  \nb" `shouldBe` True

    describe "stringTok" $ do
        it "returns a parser that ignores the initial spaces and checks for the given string" $
            show (parse (stringTok "abc") "  \nabc") `shouldBe` "Result >\"abc\"< "
        it "returns an error if string is not found after zero or more spaces" $
            onErrorResult (stringTok "abc") "  \nab" `shouldBe` True

    describe "choice" $ do
        it "returns a parser that matches input to any one of input parsers" $
            show (parse (choice [is 'c', space, is 'a']) "  \nabc") `shouldBe` "Result >' '<  \nabc"
        it "returns an error if none of the parsers match" $
            onErrorResult (choice [is 'A', is 'B']) "  \nab" `shouldBe` True

    describe "between" $ do
        it "returns a parser that matches the last parser to the string between the given characters while ignoring whitespace" $
            show (parse (between '{' '}' (is 'a')) " { a } ") `shouldBe` "Result >'a'<  "
        it "returns an error if the characters are not present" $
            onErrorResult (between '{' '}' (is 'a')) " { a " `shouldBe` True
        it "returns an error if the parser doesn't match the string in between" $
            onErrorResult (between '{' '}' (is 'a')) " { b } " `shouldBe` True

    describe "sepBy" $ do
        it "returns a parser that matches input repeatedly to a string with matching strings seperated by given character" $
            show (parse (sepBy ',' (is 'a')) "a, a, a, a") `shouldBe` "Result >\"aaaa\"< "
        it "returns an error if the beginning isn't matched by the parser given" $
            onErrorResult (sepBy ',' (is 'a')) " a, a, a, a" `shouldBe` True

    describe "betweenSepBy" $ do
        it "returns a parser that matches given parser to a string within given characters and separated by the given character" $
            show (parse (betweenSepBy '{' '}' ',' (is 'a')) "{a, a, a, a}") `shouldBe` "Result >\"aaaa\"< "
        it "returns an error if the beginning isn't matched by the parser given" $
            onErrorResult (betweenSepBy '{' '}' ',' (is 'a')) "{ b, a, a, a, a }" `shouldBe` True
        it "returns an error if one of the enclosing characters is missing" $
            onErrorResult (betweenSepBy '{' '}' ',' (is 'a')) "{ b, a, a, a, a " `shouldBe` True

    describe "noneOf" $ do
        it "returns a parser that matches input containing none of the characters from the given string" $
            show (parse (noneOf "abcd") "ABCD") `shouldBe` "Result >'A'< BCD"
        it "returns an error if the first character is present within the given list" $
            onErrorResult (noneOf "abcd") "aBCD" `shouldBe` True

    describe "satisfyAll" $ do
        it "returns a parser that matches input character which satisfies all given conditions" $
            show (parse (satisfyAll [isAlpha, isUpper]) "ABCD") `shouldBe` "Result >'A'< BCD"
        it "returns an error if the first character doesn't satisfy any condition" $
            onErrorResult (satisfyAll [isAlpha, isUpper]) "aBCD" `shouldBe` True
