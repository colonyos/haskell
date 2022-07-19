{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}                                                                                                                                                         
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module ColoniesLib  (
    Colony (..), 
    Runtime (..),
    Conditions (..),
    ProcessSpec (..),
    createColony,
    addColony,
    getColony,
    getColonies,
    createRuntime,
    addRuntime,
    approveRuntime,
    createConditions,
    createProcessSpec,
    createEmptyProcess,
    addEnv,
    submit,
    getProcess,
    assign,
    getFunc,
    getArgs,
    getProcessId,
    addAttribute,
    createAttribute,
    close,
    failed
  ) where 

import Data.Aeson as JSON
import GHC.Generics
import Network.HTTP.Simple
import Data.ByteString.Base64 as Base64
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Internal as BLI
import Data.Text as T
import Data.Text.IO as T
import Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding as TL
import Data.Text.Lazy.IO as TL
import Data.Map as M
import Data.Typeable
import Control.Exception (try)
import Prelude hiding (error) 
import GHC.Records (getField)
import CryptoLib

-- Data types declaration
data RPCMsg = RPCMsg { signature   :: T.Text,
                       payloadtype :: T.Text,
                       payload     :: T.Text
                     } deriving (Show, Generic)
instance FromJSON RPCMsg
instance ToJSON RPCMsg

data RPCReplyMsg = RPCReplyMsg { error       :: Bool,
                                 payloadtype :: T.Text,
                                 payload     :: T.Text
                               } deriving (Show, Generic)
instance FromJSON RPCReplyMsg
instance ToJSON RPCReplyMsg

data Err = Err deriving (Show, Eq)

data RPCOperations = AddColonyRPCMsg { colony  :: Colony, msgtype :: T.Text } 
                   | GetColonyRPCMsg { colonyid :: T.Text, msgtype :: T.Text }
                   | GetColoniesRPCMsg { msgtype :: T.Text }
                   | AddRuntimeRPCMsg { runtime :: Runtime, msgtype :: T.Text }
                   | ApproveRuntimeRPCMsg { runtimeid :: T.Text, msgtype :: T.Text }
                   | SubmitProcessSpecRPCMsg { spec :: ProcessSpec, msgtype :: T.Text }
                   | GetProcessRPCMsg { processid :: T.Text, msgtype :: T.Text }
                   | AssignProcessRPCMsg { colonyid :: T.Text, timeout :: Int, msgtype :: T.Text }
                   | AddAttributeRPCMsg { attribute :: Attribute, msgtype :: T.Text }
                   | CloseSuccessfulRPCMsg { processid :: T.Text, msgtype :: T.Text }
                   | CloseFailedRPCMsg { processid :: T.Text, msgtype :: T.Text }
                       deriving (Show, Generic)
instance FromJSON RPCOperations 
instance ToJSON RPCOperations 

data Colony = Colony { colonyid :: T.Text,
                       name :: T.Text } deriving (Show, Generic, Eq)
instance FromJSON Colony
instance ToJSON Colony

data Runtime = Runtime { runtimeid :: T.Text,
                         runtimetype :: T.Text,
                         name :: T.Text,
                         colonyid :: T.Text,
                         cpu :: T.Text,
                         cores :: Int, 
                         mem :: Int,
                         gpu :: T.Text,
                         gpus :: Int,
                         state :: Int,
                         commissiontime :: T.Text,
                         lastheardfromtime :: T.Text } deriving (Show, Generic, Eq)
instance FromJSON Runtime 
instance ToJSON Runtime 

data Conditions = Conditions { colonyid :: T.Text, 
                               runtimeids :: [T.Text],
                               runtimetype :: T.Text,
                               mem :: Int, 
                               cores :: Int,
                               gpus :: Int,
                               dependencies :: [T.Text] } deriving (Show, Generic, Eq)
instance FromJSON Conditions 
instance ToJSON Conditions 

data ProcessSpec = ProcessSpec { name :: T.Text,
                                 image :: T.Text,
                                 func :: T.Text,
                                 args :: [T.Text],
                                 priority :: Int, 
                                 maxwaittime :: Int,
                                 maxexectime :: Int,
                                 maxretries :: Int,
                                 conditions :: Conditions,
                                 env :: Map T.Text T.Text } deriving (Show, Generic, Eq)
instance FromJSON ProcessSpec 
instance ToJSON ProcessSpec 

data Attribute = Attribute { attributeid :: T.Text,
                             targetid :: T.Text,
                             targetcolonyid :: T.Text,
                             targetprocessgraphid :: T.Text,
                             attributetype :: Int,
                             key :: T.Text,
                             value :: T.Text } deriving (Show, Generic, Eq)
instance FromJSON Attribute 
instance ToJSON Attribute 

data Process = Process { processid :: T.Text,
                         assignedruntimeid :: T.Text,
                         isassigned :: Bool,
                         state :: Int,
                         submissiontime :: T.Text,
                         starttime :: T.Text,
                         endtime :: T.Text, 
                         waitdeadline :: T.Text,
                         execdeadline :: T.Text,
                         errormsg :: T.Text,
                         retries :: Int,
                         attributes :: [Attribute],
                         spec :: ProcessSpec,
                         waitforparents :: Bool,
                         parents :: [T.Text],
                         children :: [T.Text],
                         processgraphid :: T.Text} deriving (Show, Generic, Eq)
instance FromJSON Process 
instance ToJSON Process

-- RPC functions  
checkError :: Either HttpException (Response BLI.ByteString) -> Maybe Err 
checkError e = case e of
    Left e -> Just Err
    Right resp -> Nothing 
    
sendRPCMsg :: RPCOperations -> String -> String -> IO (Either HttpException (Response BLI.ByteString))
sendRPCMsg rpcMsg host key = do
    let jsonEncoded = JSON.encode rpcMsg 
    let base64Encoded = Base64.encode $ BL.toStrict jsonEncoded
    signature <- sign (BI.unpackChars base64Encoded) key
    let payloadType = getField @"msgtype" rpcMsg
    let rpcMsg = RPCMsg { signature = T.pack (signature), 
                          payload = T.pack (BI.unpackChars (base64Encoded)), 
                          payloadtype = payloadType } 
    let jsonEncodedRPCMsg =  JSON.encode rpcMsg

    request' <- parseRequest host 
    let request
            = setRequestMethod "POST"
            $ setRequestPath "/api"
            $ setRequestBodyLBS jsonEncodedRPCMsg
            $ setRequestSecure False 
            $ request'
    try $ httpLBS request

-- Parser functions
parsePayload :: T.Text -> Either a BI.ByteString -> Maybe (T.Text, BLI.ByteString)
parsePayload payloadType jsonEncoded = do
    case jsonEncoded of
        Left e -> Nothing 
        Right j -> do let json = j
                      let lazyJSON = BL.fromStrict json
                      Just (payloadType, lazyJSON)

parseResponse :: Either HttpException (Response BLI.ByteString) -> Maybe (T.Text, BLI.ByteString)
parseResponse e = case e of
    Left e -> Nothing
    Right resp -> do let json = getResponseBody resp
                     let rpcReplyMsg = JSON.eitherDecode json :: Either String RPCReplyMsg
                     case rpcReplyMsg of
                          Left e -> Nothing
                          Right p -> do let payload = getField @"payload" p 
                                        let payloadType = getField @"payloadtype" p
                                        let jsonEncoded = Base64.decode $ BI.packChars $ T.unpack payload
                                        parsePayload payloadType jsonEncoded

parseRuntime :: Maybe (T.Text, BLI.ByteString) -> Maybe Runtime 
parseRuntime res = do  
    case res of
        Nothing -> Nothing
        Just tuple -> do let payloadType = fst tuple
                         let json = snd tuple 
                         JSON.decode json :: Maybe Runtime 

parseColony :: Maybe (T.Text, BLI.ByteString) -> Maybe Colony
parseColony res = do  
    case res of
        Nothing -> Nothing
        Just tuple -> do let payloadType = fst tuple
                         let json = snd tuple 
                         JSON.decode json :: Maybe Colony

parseColonies :: Maybe (T.Text, BLI.ByteString) -> Maybe [Colony]
parseColonies res = do  
    case res of
        Nothing -> Nothing
        Just tuple -> do let payloadType = fst tuple
                         let json = snd tuple 
                         JSON.decode json :: Maybe [Colony]

parseProcess :: Maybe (T.Text, BLI.ByteString) -> Maybe Process
parseProcess res = do  
    case res of
        Nothing -> Nothing
        Just tuple -> do let payloadType = fst tuple
                         let json = snd tuple 
                         JSON.decode json :: Maybe Process 

parseAttribute :: Maybe (T.Text, BLI.ByteString) -> Maybe Attribute 
parseAttribute res = do  
    case res of
        Nothing -> Nothing
        Just tuple -> do let payloadType = fst tuple
                         let json = snd tuple 
                         JSON.decode json :: Maybe Attribute 

-- API functions 
createColony :: String -> String -> Colony
createColony colonyid name = 
    Colony { colonyid = T.pack colonyid, name = T.pack name }

addColony :: Colony -> String -> String -> IO (Maybe Colony)
addColony colony host key = do
    resp <- sendRPCMsg AddColonyRPCMsg { colony = colony, msgtype = "addcolonymsg" } host key 
    return $ parseColony $ parseResponse resp 

getColony :: String -> String -> String -> IO (Maybe Colony) 
getColony colonyId host key = do
    resp <- sendRPCMsg GetColonyRPCMsg { colonyid = T.pack colonyId, msgtype = "getcolonymsg" } host key 
    return $ parseColony $ parseResponse resp 

getColonies :: String -> String -> IO (Maybe [Colony]) 
getColonies host key = do
    resp <- sendRPCMsg GetColoniesRPCMsg { msgtype = "getcoloniesmsg" } host key 
    return $ parseColonies $ parseResponse resp 

createRuntime :: String -> String -> String -> Runtime 
createRuntime runtimeType runtimeId colonyId = 
    Runtime { runtimeid = T.pack runtimeId, 
              runtimetype = T.pack runtimeType, 
              name ="test_name", 
              colonyid = T.pack colonyId,
              cpu = "",
              cores = 0,
              mem = 0,
              gpu = "",
              gpus = 0,
              state = 0,
              commissiontime = "2022-07-10T13:32:17.117545582+02:00",
              lastheardfromtime = "2022-07-10T13:32:17.117545582+02:00"}

addRuntime :: Runtime -> String -> String -> IO (Maybe Runtime)
addRuntime runtime host key = do
    resp <- sendRPCMsg AddRuntimeRPCMsg { runtime = runtime, msgtype = "addruntimemsg" } host key 
    return $ parseRuntime $ parseResponse resp 

approveRuntime :: String -> String -> String -> IO (Maybe Err)
approveRuntime runtimeId host key = do
    resp <- sendRPCMsg ApproveRuntimeRPCMsg { runtimeid = T.pack runtimeId, msgtype = "approveruntimemsg" } host key 
    return $ checkError resp

createConditions :: String -> String -> [String] -> Conditions 
createConditions colonyId runtimeType dependencies =  
    Conditions { colonyid = T.pack colonyId,
                            runtimeids = [],
                            runtimetype = T.pack runtimeType,
                            mem = 0, 
                            cores = 0,
                            gpus = 0,
                            dependencies = fmap (T.pack) dependencies }

createProcessSpec :: String -> String -> [String] -> Int -> Int -> Int -> Conditions -> ProcessSpec 
createProcessSpec name func args maxwaittime maxexectime maxretries conditions =
  ProcessSpec { name = T.pack name,
                image = "",
                func = T.pack func,
                args = fmap (T.pack) args,
                priority = 0,
                maxwaittime = maxwaittime,
                maxexectime = maxexectime,
                maxretries = maxretries,
                conditions = conditions,
                env = M.empty } 

createProcessSpecWithEnv :: String -> String -> [String] -> Int -> Int -> Int -> Map T.Text T.Text -> Conditions -> ProcessSpec 
createProcessSpecWithEnv name func args maxwaittime maxexectime maxretries env conditions =
  ProcessSpec { name = T.pack name,
                image = "",
                func = T.pack func,
                args = fmap (T.pack) args,
                priority = 0,
                maxwaittime = maxwaittime,
                maxexectime = maxexectime,
                maxretries = maxretries,
                conditions = conditions,
                env = env} 

addEnv :: ProcessSpec -> String -> String -> ProcessSpec
addEnv spec key value = do let name = T.unpack (getField @"name" spec)
                           let func = T.unpack (getField @"func" spec)
                           let args = fmap (T.unpack) (getField @"args" spec)
                           let maxwaittime = getField @"maxwaittime" spec
                           let maxexectime = getField @"maxexectime" spec
                           let maxretries = getField @"maxretries" spec
                           let conditions = getField @"conditions" spec
                           let env = getField @"env" spec
                           let textKey = T.pack key
                           let textValue = T.pack value
                           let newEnv = M.insert textKey textValue env
                           createProcessSpecWithEnv name func args maxwaittime maxexectime maxretries newEnv conditions  

createEmptyProcess :: Process
createEmptyProcess = 
    Process { processid = "",
              assignedruntimeid  = "",
              isassigned = False,
              state = -1,
              submissiontime = "",
              starttime = "",
              endtime = "", 
              waitdeadline = "",
              execdeadline = "",
              errormsg = "",
              retries = -1,
              attributes = [],
              spec = ProcessSpec { name = "",
                                   image = "",
                                   func = "",
                                   args = [],
                                   priority = 0,
                                   maxwaittime = -1,
                                   maxexectime = -1,
                                   maxretries = -1,
                                   conditions = Conditions { colonyid = "",
                                                             runtimeids = [],
                                                             runtimetype = "",
                                                             mem = 0, 
                                                             cores = 0,
                                                             gpus = 0,
                                                             dependencies = [] },
                                   env = M.empty },
              waitforparents = False,
              parents = [],
              children = [],
              processgraphid  = ""}

submit :: ProcessSpec -> String -> String -> IO (Maybe Process)
submit spec host key = do 
    resp <- sendRPCMsg SubmitProcessSpecRPCMsg { spec = spec, msgtype = "submitprocessespecmsg" } host key 
    return $ parseProcess $ parseResponse resp 

getProcess :: String -> String -> String -> IO (Maybe Process)
getProcess processId host key = do 
    resp <- sendRPCMsg GetProcessRPCMsg { processid = T.pack processId, msgtype = "getprocessmsg" } host key 
    return $ parseProcess $ parseResponse resp 

assign :: String -> String -> String -> IO (Maybe Process)
assign colonyId host key = do 
    resp <- sendRPCMsg AssignProcessRPCMsg { colonyid = T.pack colonyId, timeout = -1, msgtype = "assignprocessmsg" } host key 
    return $ parseProcess $ parseResponse resp 

getFunc :: Process -> IO String
getFunc process = do 
    let spec = getField @"spec" process 
    return $ T.unpack $ getField @"func" spec 

getArgs :: Process -> IO [String]
getArgs process = do 
    let spec = getField @"spec" process 
    return $ fmap (T.unpack) $ getField @"args" spec

getProcessId :: Process -> IO String
getProcessId process = do 
    let processId = getField @"processid" process 
    return $ T.unpack processId 

createAttribute :: Process -> String -> String -> Attribute 
createAttribute process key value = 
    Attribute { attributeid = "",
                targetid = getField @"processid" process,
                targetcolonyid = getField @"colonyid" $ getField @"conditions" $ getField @"spec" process,
                targetprocessgraphid = "",
                attributetype = 1,
                key = T.pack key,
                value = T.pack value } 

addAttribute :: Attribute -> String -> String -> IO (Maybe Attribute)
addAttribute attr host key = do
    resp <- sendRPCMsg AddAttributeRPCMsg { attribute = attr, msgtype = "addattributemsg" } host key 
    return $ parseAttribute $ parseResponse resp 

close :: Process -> String -> String -> IO (Maybe Err)
close process host key = do
    resp <- sendRPCMsg CloseSuccessfulRPCMsg { processid = getField @"processid" process, msgtype = "closesuccessfulmsg" } host key 
    return $ checkError resp 

failed :: Process -> String -> String -> IO (Maybe Err)
failed process host key = do
    resp <- sendRPCMsg CloseFailedRPCMsg { processid = getField @"processid" process, msgtype = "closefailedmsg" } host key 
    return $ checkError resp 
