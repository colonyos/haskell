{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}                                                                                                                                                         
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module ColoniesLib  (
    Colony (..), 
    Runtime (..), 
    addColony,
    getColony,
    getColonies,
    createRuntime,
    addRuntime,
    approveRuntime
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
import Data.Typeable
import Control.Exception (try)
import Prelude hiding (error) 
import GHC.Records (getField)
import CryptoLib

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

data RPCOperations = AddColonyRPCMsg { colony  :: Colony, msgtype :: T.Text } 
                   | GetColonyRPCMsg { colonyid :: T.Text, msgtype :: T.Text }
                   | GetColoniesRPCMsg { msgtype :: T.Text }
                   | AddRuntimeRPCMsg { runtime :: Runtime, msgtype :: T.Text }
                   | ApproveRuntimeRPCMsg { runtimeid :: T.Text, msgtype :: T.Text }
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

addColony :: Colony -> String -> String -> IO (Maybe Colony)
addColony colony host key = do
    resp <- sendRPCMsg AddColonyRPCMsg { colony = colony, msgtype="addcolonymsg" } host key 
    return $ parseColony $ parseResponse resp 

getColony :: String -> String -> String -> IO (Maybe Colony) 
getColony colonyId host key = do
    resp <- sendRPCMsg GetColonyRPCMsg { colonyid = T.pack colonyId, msgtype="getcolonymsg" } host key 
    return $ parseColony $ parseResponse resp 

getColonies :: String -> String -> IO (Maybe [Colony]) 
getColonies host key = do
    resp <- sendRPCMsg GetColoniesRPCMsg { msgtype="getcoloniesmsg" } host key 
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
    resp <- sendRPCMsg AddRuntimeRPCMsg { runtime = runtime, msgtype="addruntimemsg" } host key 
    return $ parseRuntime $ parseResponse resp 

approveRuntime :: String -> String -> String -> IO (Maybe Err)
approveRuntime runtimeId host key = do
    resp <- sendRPCMsg ApproveRuntimeRPCMsg { runtimeid = T.pack runtimeId, msgtype="approveruntimemsg" } host key 
    return $ checkError resp
