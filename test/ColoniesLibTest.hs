{-# LANGUAGE OverloadedStrings #-}

module ColoniesLibTest where

import Test.Tasty
import Test.Tasty.HUnit
import CryptoLib
import ColoniesLib 

import Data.Aeson
import Data.Text

coloniesLibTest = testGroup "ColoniesLib" [testColony]

testColony = testCase "colony" $ do
    let colony = Colony { colonyid = "name", name="last" }
    let c =  encode colony 
    print c 
    let l = 64 
    addColony
    createColony
    assertBool "expecting length prvkey == 64" $ l == 64
