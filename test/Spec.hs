module Main where

import Test.Tasty
import Test.Tasty.HUnit
import CryptoLib
import CryptoLibTest
import ColoniesLibTest

main :: IO ()
main = defaultMain tests 

tests :: TestTree
tests = testGroup "Tests" [cryptoLibTests, coloniesLibTest]
