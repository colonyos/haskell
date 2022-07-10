module Main where

import CryptoLib
import ColoniesLib
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad (forever)

colonyId = "4787a5071856a4acf702b2ffcea422e3237a679c681314113d86139461290cf4"
runtimePrvKey = "ddf7f7791208083b6a9ed975a72684f6406a269cfa36f1b1c32045c0a71fff05"
host = "http://localhost:50080"

serve :: IO ()
serve = do 
    maybeProcess <- assign colonyId host runtimePrvKey
    print maybeProcess 
    if maybeProcess == Nothing then
        threadDelay 1000000 -- wait 1 seconds
    else do 
        let process = maybe createEmptyProcess id maybeProcess
        cmd <- getCmd process
        args <- getArgs process
        print cmd
        print args

main :: IO ()
main = forever serve
