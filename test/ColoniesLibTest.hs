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
                                           testGetColonies,
                                           testSubmit,
                                           testGetProcessId,
                                           testAssign,
                                           testClose,
                                           testFailed]

testAddColony = testCase "addColony" $ do
    prvkey <- generateKey
    colonyId <- identity prvkey
    let colony = createColony colonyId "test_colony"
    maybeAddedColony <- addColony colony host serverPrvKey
    let addedColony = maybe Colony { colonyid = "", name="" } id maybeAddedColony
    assertEqual "expecting added colony == colony" colony addedColony

testGetColony = testCase "getColony" $ do
    colonyPrvKey <- generateKey
    colonyId <- identity colonyPrvKey 
    let colony = createColony colonyId "test_colony"
    addColony colony host serverPrvKey
  
    executorPrvKey <- generateKey
    executorId <- identity executorPrvKey 
    let executor = createExecutor "test_executor_name1" "test_executor_type" executorId colonyId
    addedExecutor <- addExecutor executor host colonyPrvKey 
    err <- approveExecutor executorId host colonyPrvKey
    assertBool "expecting no error approving executor" (err==Nothing)
    
    maybeAddedColony <- getColony colonyId host executorPrvKey 
    let addedColony = maybe Colony { colonyid = "", name="" } id maybeAddedColony
    assertEqual "expecting added colony == colony" colony addedColony

testGetColonies = testCase "getColonies" $ do
    prvkey <- generateKey
    colonyId <- identity prvkey
    let colony = createColony colonyId "test_colony"
    maybeAddedColony <- addColony colony host serverPrvKey
    
    prvkey <- generateKey
    colonyId <- identity prvkey
    let colony = createColony colonyId "test_colony"
    maybeAddedColony <- addColony colony host serverPrvKey
   
    maybeColonies <- getColonies host serverPrvKey
    let colonies = maybe [] id maybeColonies
    let l = Prelude.length colonies 
    assertBool "expecting length colonies>2" (l > 2)

testSubmit = testCase "submit" $ do
    colonyPrvKey <- generateKey
    colonyId <- identity colonyPrvKey 
    let colony = createColony colonyId "test_colony"
    addColony colony host serverPrvKey
  
    executorPrvKey <- generateKey
    executorId <- identity executorPrvKey 
    let executor = createExecutor "test_executor_name2" "test_executor_type" executorId colonyId
    addedExecutor <- addExecutor executor host colonyPrvKey 
    err <- approveExecutor executorId host colonyPrvKey
    assertBool "expecting no error approving executor" (err==Nothing)

    let cond = createConditions colonyId "test_executor_type" []
    let spec = createProcessSpec "test_process_name" "test_func" ["test_arg1", "test_arg2"] (-1) 100 3 cond
    let specWithEnv = addEnv spec "test_key" "test_value" 
    maybeProcess <- submit specWithEnv host executorPrvKey
    assertBool "expecting process" (maybeProcess/=Nothing)

testGetProcessId = testCase "getProcessId" $ do
    colonyPrvKey <- generateKey
    colonyId <- identity colonyPrvKey
    let colony = createColony colonyId "test_colony"
    addColony colony host serverPrvKey
 
    executorPrvKey <- generateKey
    executorId <- identity executorPrvKey
    let executor = createExecutor "test_executor_name3" "test_executor_type" executorId colonyId
    addedExecutor <- addExecutor executor host colonyPrvKey
    err <- approveExecutor executorId host colonyPrvKey
    assertBool "expecting no error approving executor" (err==Nothing)

    let cond = createConditions colonyId "test_executor_type" []
    let spec = createProcessSpec "test_process_name" "test_func" ["test_arg1", "test_arg2"] (-1) 100 3 cond
    maybeSubmittedProcess <- submit spec host executorPrvKey
    let submittedProcess = maybe createEmptyProcess id maybeSubmittedProcess
    assertBool "expecting process" (maybeSubmittedProcess/=Nothing)
  
    maybeAssignedProcess <- assign colonyId 10 host executorPrvKey
    let assignedProcess = maybe createEmptyProcess id maybeAssignedProcess

    processId <- getProcessId assignedProcess
    maybeProcess <- getProcess processId host executorPrvKey
    assertBool "expecting process" (maybeProcess/=Nothing)
    let process = maybe createEmptyProcess id maybeProcess
    processId2 <- getProcessId assignedProcess

    assertBool "expecting same processId" (processId==processId2)

testAssign = testCase "assign" $ do
    colonyPrvKey <- generateKey
    colonyId <- identity colonyPrvKey
    let colony = createColony colonyId "test_colony"
    addColony colony host serverPrvKey
 
    executorPrvKey <- generateKey
    executorId <- identity executorPrvKey
    let executor = createExecutor "test_executor_name4" "test_executor_type" executorId colonyId
    addedExecutor <- addExecutor executor host colonyPrvKey
    err <- approveExecutor executorId host colonyPrvKey
    assertBool "expecting no error approving executor" (err==Nothing)

    let cond = createConditions colonyId "test_executor_type" []
    let spec = createProcessSpec "test_process_name" "test_func" ["test_arg1", "test_arg2"] (-1) 100 3 cond
    let specWithEnv = addEnv spec "test_key" "test_value" 
    maybeSubmittedProcess <- submit specWithEnv host executorPrvKey
    let submittedProcess = maybe createEmptyProcess id maybeSubmittedProcess
    assertBool "expecting process" (maybeSubmittedProcess/=Nothing)
  
    maybeAssignedProcess <- assign colonyId 10 host executorPrvKey
    let assignedProcess = maybe createEmptyProcess id maybeAssignedProcess
    f <- getFunc assignedProcess
    args <- getArgs assignedProcess
    assertEqual "expecting correct func" f "test_func"
    assertEqual "expecting correct args" args ["test_arg1", "test_arg2"]

testClose = testCase "close" $ do
    colonyPrvKey <- generateKey
    colonyId <- identity colonyPrvKey
    let colony = createColony colonyId "test_colony"
    addColony colony host serverPrvKey
 
    executorPrvKey <- generateKey
    executorId <- identity executorPrvKey
    let executor = createExecutor "test_executor_name5" "test_executor_type" executorId colonyId
    addedExecutor <- addExecutor executor host colonyPrvKey
    err <- approveExecutor executorId host colonyPrvKey
    assertBool "expecting no error approving executor" (err==Nothing)

    let cond = createConditions colonyId "test_executor_type" []
    let spec = createProcessSpec "test_process_name" "test_func" ["test_arg1", "test_arg2"] (-1) 100 3 cond
    maybeSubmittedProcess <- submit spec host executorPrvKey
    let submittedProcess = maybe createEmptyProcess id maybeSubmittedProcess
    assertBool "expecting process" (maybeSubmittedProcess/=Nothing)
  
    maybeAssignedProcess <- assign colonyId 10 host executorPrvKey
    let assignedProcess = maybe createEmptyProcess id maybeAssignedProcess

    err <- close assignedProcess host executorPrvKey
    assertBool "expecting no error closing process" (err==Nothing)

testFailed = testCase "failed" $ do
    colonyPrvKey <- generateKey
    colonyId <- identity colonyPrvKey
    let colony = createColony colonyId "test_colony"
    addColony colony host serverPrvKey
 
    executorPrvKey <- generateKey
    executorId <- identity executorPrvKey
    let executor = createExecutor "test_executor_name6" "test_executor_type" executorId colonyId
    addedExecutor <- addExecutor executor host colonyPrvKey
    err <- approveExecutor executorId host colonyPrvKey
    assertBool "expecting no error approving executor" (err==Nothing)

    let cond = createConditions colonyId "test_executor_type" []
    let spec = createProcessSpec "test_process_name" "test_func" ["test_arg1", "test_arg2"] (-1) 100 3 cond
    maybeSubmittedProcess <- submit spec host executorPrvKey
    let submittedProcess = maybe createEmptyProcess id maybeSubmittedProcess
    assertBool "expecting process" (maybeSubmittedProcess/=Nothing)
  
    maybeAssignedProcess <- assign colonyId 10 host executorPrvKey
    let assignedProcess = maybe createEmptyProcess id maybeAssignedProcess

    err <- failed assignedProcess host executorPrvKey
    assertBool "expecting no error closing process" (err==Nothing)
