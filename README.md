# Introduction
This repo contains a Haskell implementation of the Colonies API, making it possible to implement Colonies executor and applications in Haskell.

## Example code
The code below assigns a Colonies process and calculates the last number in a Fibonacci series. 
```haskell
module Main where

import CryptoLib
import ColoniesLib
import Control.Concurrent (threadDelay)
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
    maybeProcess <- assign colonyId 5 host executorPrvKey
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
            print "Invalid Func arg"
    else 
        print "No process could be assigned"

main :: IO ()
main = forever executor 
```

## To test it ... 
First, install [Stack](https://docs.haskellstack.org/en/stable/README/). 

### Start a Colonies server 
You need to have access to a Colonies server. On Linux, run the commands below to start a server. See the [Colonies release page](https://github.com/colonyos/colonies/releases) for Windows and Mac binaries.

```console
git clone https://github.com/colonyos/colonies
cd colonies
source devenv
./bin/colonies dev
```

Start a Haskell executor (the example code). Note, you need to first install Stack and clone the repo.
```console
stack run
```

### Submit a process to calculate Fib(10)
```console
./bin/colonies process run --func fibonacci --args 10 --targettype cli
```
#### Output
```console
INFO[0000] Starting a Colonies client                    Insecure=true ServerHost=localhost ServerPort=50080
INFO[0000] Process submitted                             ProcessID=da41231a1f30039edb5677a8084e5f1c605bc54e3e8fdec565170c4e4e8d23be
```
### Lookup the result 
```console
./bin/colonies process get --processid da41231a1f30039edb5677a8084e5f1c605bc54e3e8fdec565170c4e4e8d23be
```

#### Output
```console
Process:
+--------------------+------------------------------------------------------------------+
| ID                 | da41231a1f30039edb5677a8084e5f1c605bc54e3e8fdec565170c4e4e8d23be |
| IsAssigned         | True                                                             |
| AssignedExecutorID | 3fc05cf3df4b494e95d6a3d297a34f19938f7daa7422ab0d4f794454133341ac |
| State              | Successful                                                       |
| Priority           | 0                                                                |
| SubmissionTime     | 2022-07-11 23:46:19                                              |
| StartTime          | 2022-07-11 23:46:19                                              |
| EndTime            | 2022-07-11 23:46:19                                              |
| Deadline           | 0001-01-01 01:12:12                                              |
| WaitingTime        | 473.1ms                                                          |
| ProcessingTime     | 9.766ms                                                          |
| Retries            | 0                                                                |
+--------------------+------------------------------------------------------------------+

FunctionSpec:
+-------------+-----------+
| Func        | fibonacci |
| Args        | 10        |
| MaxExecTime | -1        |
| MaxRetries  | -1        |
+-------------+-----------+

Conditions:
+--------------+------------------------------------------------------------------+
| ColonyID     | 4787a5071856a4acf702b2ffcea422e3237a679c681314113d86139461290cf4 |
| ExecutorIDs  | None                                                             |
| ExecutorType | cli                                                              |
+--------------+------------------------------------------------------------------+

Attributes:
+------------------------------------------------------------------+--------+-------+------+
|                                ID                                |  KEY   | VALUE | TYPE |
+------------------------------------------------------------------+--------+-------+------+
| a584cd5c51067bbdc492dba8361e4a362aacc39b0f9726d483741fd962d5b418 | output | 55    | Out  |
+------------------------------------------------------------------+--------+-------+------+
```

### Submit a process to calculate Fib(10) and wait for the result
```console
./bin/colonies function exec --func fibonacci --args 10 --executortype cli --wait
```

#### Output
```console
55⏎
```
