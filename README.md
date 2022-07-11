# Introductions

This repo contains a Haskell implementation of the ColonyRuntime API, making it possible to implement Colonies applications/workers in Haskell.

## Example code
The code below assigns a Colonies process and calculates the last number in a Fibonacci series. 
```haskell
module Main where

import CryptoLib
import ColoniesLib
import Control.Concurrent (threadDelay)
import Control.Monad (forever)

colonyId = "4787a5071856a4acf702b2ffcea422e3237a679c681314113d86139461290cf4"
runtimePrvKey = "ddf7f7791208083b6a9ed975a72684f6406a269cfa36f1b1c32045c0a71fff05"
host = "http://localhost:50080"

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

worker :: IO ()
worker = do
    -- Connect to the Colonies server and try to assign a process to execute from the job queue
    maybeProcess <- assign colonyId host runtimePrvKey
    if maybeProcess == Nothing then
        threadDelay 1000000 -- Job queue is empty, wait 1 second and try again ...
    else do
        -- Parse process parameters
        let process = maybe createEmptyProcess id maybeProcess
        cmd <- getCmd process
        args <- getArgs process
        if cmd == "fibonacci" then do
            print "Got a Fibonacci task"
            let n = read $ head args :: Integer
            let f = fib n 
            print $ "-> fib " ++ show n ++ "=" ++ show f
            -- Create a an attribute 
            let attr = createAttribute process "output" $ show f
            -- Connect to the Colnies server and add the add attribute to the process object
            -- Note: addAddtribute will return Nothing in case of error, else a Just Attribute
            maybeAddedAttr <- addAttribute attr host runtimePrvKey
            close process host runtimePrvKey 
            print "Done calculating Fibonacci"
        else
            print "Invalid cmd"

main :: IO ()
main = forever worker 
```

## To test it ... 
### Start a Colonies dev server 
```console
colonies dev
```

Start a Haskell worker (the example code). Note, you need to first install Stack and clone the repo.
```console
stack run
```

### To fully use the colonies CLI, the following environmental variables needs to be added
```console
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export LC_CTYPE=UTF-8
export TZ=Europe/Stockholm
export COLONIES_TLS="false"
export COLONIES_SERVERHOST="localhost "
export COLONIES_SERVERPORT="50080"
export COLONIES_MONITORPORT="21120"
export COLONIES_MONITORINTERVALL="1"
export COLONIES_SERVERID="039231c7644e04b6895471dd5335cf332681c54e27f81fac54f9067b3f2c0103"
export COLONIES_SERVERPRVKEY="fcc79953d8a751bf41db661592dc34d30004b1a651ffa0725b03ac227641499d"
export COLONIES_DBHOST="localhost"
export COLONIES_DBUSER="postgres"
export COLONIES_DBPORT="50070"
expoer COLONIES_DBPASSWORD="rFcLGNkgsNtksg6Pgtn9CumL4xXBQ7"
export COLONIES_COLONYID="4787a5071856a4acf702b2ffcea422e3237a679c681314113d86139461290cf4"
export COLONIES_COLONYPRVKEY="ba949fa134981372d6da62b6a56f336ab4d843b22c02a4257dcf7d0d73097514"
export COLONIES_RUNTIMEID="3fc05cf3df4b494e95d6a3d297a34f19938f7daa7422ab0d4f794454133341ac"
export COLONIES_RUNTIMEPRVKEY="ddf7f7791208083b6a9ed975a72684f6406a269cfa36f1b1c32045c0a71fff05"
export COLONIES_RUNTIMETYPE="cli"
```

### Submit a process to calculate Fib(10)
```console
colonies process run --cmd fibonacci --args 10 --runtimetype cli
```
#### Output
```console
INFO[0000] Starting a Colonies client                    Insecure=true ServerHost=localhost ServerPort=50080
INFO[0000] Process submitted                             ProcessID=da41231a1f30039edb5677a8084e5f1c605bc54e3e8fdec565170c4e4e8d23be
```
### Lookup the result 
```console
colonies process get --processid da41231a1f30039edb5677a8084e5f1c605bc54e3e8fdec565170c4e4e8d23be
```

#### Output
```console
Process:
+-------------------+------------------------------------------------------------------+
| ID                | da41231a1f30039edb5677a8084e5f1c605bc54e3e8fdec565170c4e4e8d23be |
| IsAssigned        | True                                                             |
| AssignedRuntimeID | 3fc05cf3df4b494e95d6a3d297a34f19938f7daa7422ab0d4f794454133341ac |
| State             | Successful                                                       |
| Priority          | 0                                                                |
| SubmissionTime    | 2022-07-11 23:46:19                                              |
| StartTime         | 2022-07-11 23:46:19                                              |
| EndTime           | 2022-07-11 23:46:19                                              |
| Deadline          | 0001-01-01 01:12:12                                              |
| WaitingTime       | 473.1ms                                                          |
| ProcessingTime    | 9.766ms                                                          |
| Retries           | 0                                                                |
+-------------------+------------------------------------------------------------------+

ProcessSpec:
+-------------+-----------+
| Image       | None      |
| Cmd         | fibonacci |
| Args        | 10        |
| Volumes     | None      |
| Ports       | None      |
| MaxExecTime | -1        |
| MaxRetries  | -1        |
+-------------+-----------+

Conditions:
+-------------+------------------------------------------------------------------+
| ColonyID    | 4787a5071856a4acf702b2ffcea422e3237a679c681314113d86139461290cf4 |
| RuntimeIDs  | None                                                             |
| RuntimeType | cli                                                              |
| Memory      | 0                                                                |
| CPU Cores   | 0                                                                |
| GPUs        | 0                                                                |
+-------------+------------------------------------------------------------------+

Attributes:
+------------------------------------------------------------------+--------+-------+------+
|                                ID                                |  KEY   | VALUE | TYPE |
+------------------------------------------------------------------+--------+-------+------+
| a584cd5c51067bbdc492dba8361e4a362aacc39b0f9726d483741fd962d5b418 | output | 55    | Out  |
+------------------------------------------------------------------+--------+-------+------+
```

### Submit a process to calculate Fib(10) and wait for the result
```console
colonies  process run --cmd fibonacci --args 10 --runtimetype cli --wait
```

#### Output
```console
55⏎
```
