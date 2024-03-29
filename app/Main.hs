module Main where

import CryptoLib
import ColoniesLib
import Control.Monad (forever)

colonyId = "4787a5071856a4acf702b2ffcea422e3237a679c681314113d86139461290cf4"
executorPrvKey = "ddf7f7791208083b6a9ed975a72684f6406a269cfa36f1b1c32045c0a71fff05"
host = "http://localhost:50080"

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

executor :: IO ()
executor = do
    -- Connect to the Colonies server and try to assign a process to execute from the job queue
    -- Wait max 5 seconds for an assignment 
    maybeProcess <- assign colonyId 100 host executorPrvKey
    if maybeProcess /= Nothing then do
        -- Parse process parameters
        let process = maybe createEmptyProcess id maybeProcess
        func <- getFunc process
        args <- getArgs process
        if func == "fibonacci" then do
            print "Got a Fibonacci func invocation"
            let n = read $ head args :: Integer
            let f = fib n 
            print $ "-> fib " ++ show n ++ "=" ++ show f
            -- Create a an attribute 
            let attr = createAttribute process "output" $ show f
            -- Connect to the Colnies server and add the add attribute to the process object
            -- Note: addAddtribute will return Nothing in case of error, else a Just Attribute
            maybeAddedAttr <- addAttribute attr host executorPrvKey
            close process host executorPrvKey 
            print "Done calculating Fibonacci"
        else
            print "Invalid func args"
    else 
        print "No process could be assigned"

main :: IO ()
main = forever executor 
