{-# LANGUAGE ScopedTypeVariables #-}
module LibSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Lib

spec :: Spec
spec = do
    describe "valueParser" $ do
        prop "returns a parser that always a success and returns the same value" $
            \(x :: Int) -> show (parse (valueParser x) "abc") === "Result >" ++ show x ++ "< abc"
        prop "returns a parser that consumes no input" $
            \(x :: String) -> show (parse (valueParser ()) x) === "Result >()< " ++ x
