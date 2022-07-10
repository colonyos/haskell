{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module ColoniesLibTest where

import Test.Tasty
import Test.Tasty.HUnit
import CryptoLib
import ColoniesLib 
import Data.Aeson
import Data.Text as T
import Data.Text.IO as T

serverPrvKey = "fcc79953d8a751bf41db661592dc34d30004b1a651ffa0725b03ac227641499d"
host = "http://localhost:50080"

coloniesLibTest = testGroup "ColoniesLib" [testAddColony, 
                                           testGetColony,
                                           testGetColonies]

testAddColony = testCase "addColony" $ do
    prvkey <- generateKey
    colonyId <- identity prvkey
    let colony = Colony { colonyid = T.pack colonyId, name="testcolony" }
    maybeAddedColony <- addColony colony host serverPrvKey
    let addedColony = maybe Colony { colonyid = "", name="" } id maybeAddedColony
    assertEqual "expecting added colony == colony" colony addedColony

testGetColony = testCase "getColony" $ do
    colonyPrvKey <- generateKey
    colonyId <- identity colonyPrvKey 
    let colony = Colony { colonyid = T.pack colonyId, name="testcolony" }
    addColony colony host serverPrvKey
  
    runtimePrvKey <- generateKey
    runtimeId <- identity runtimePrvKey 
    let runtime = createRuntime "testruntimetype" runtimeId colonyId
    addedRuntime <- addRuntime runtime host colonyPrvKey 
    err <- approveRuntime runtimeId host colonyPrvKey
    assertBool "expecting no error approving runtime" (err==Nothing)
    
    maybeAddedColony <- getColony colonyId host runtimePrvKey 
    let addedColony = maybe Colony { colonyid = "", name="" } id maybeAddedColony
    assertEqual "expecting added colony == colony" colony addedColony

testGetColonies = testCase "getColonies" $ do
    prvkey <- generateKey
    colonyId <- identity prvkey
    let colony = Colony { colonyid = T.pack colonyId, name="testcolony" }
    maybeAddedColony <- addColony colony host serverPrvKey
    
    prvkey <- generateKey
    colonyId <- identity prvkey
    let colony = Colony { colonyid = T.pack colonyId, name="testcolony" }
    maybeAddedColony <- addColony colony host serverPrvKey
   
    maybeColonies <- getColonies host serverPrvKey
    let colonies = maybe [] id maybeColonies
    let l = Prelude.length colonies 
    assertBool "expecting length colonies>2" (l > 2)
