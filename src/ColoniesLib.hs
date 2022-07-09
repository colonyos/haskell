{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}                                                                                                                                                         
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module ColoniesLib  (
    Colony (..), 
    createColony,
    addColony
  ) where 

import Data.Aeson as JSON
import Data.Text as T
import GHC.Generics
import Network.HTTP.Simple
import Data.ByteString.Base64 as Base64
import qualified Data.ByteString               as B
import qualified Data.ByteString.Internal      as BI
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Internal as BLI
import Data.Typeable
import Control.Exception (try)
import CryptoLib
import Prelude hiding (error) 
import GHC.Records (getField)
--import Data.Text.Encoding as T
import Data.Text.IO               as T
--import Control.Monad.IO.Class (liftIO)
import Debug.Trace

import Data.Text.Lazy             as TL
import Data.Text.Lazy.Encoding    as TL
import Data.Text.Lazy.IO          as TL

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

-- data Colony = Colony { colonyid :: T.Text, name :: T.Text }
--             | Runtime { runtimeid :: T.Text }
--             | Err { status :: Int, message :: T.Text }
--                   deriving (Show, Generic)

data Colony = Colony { colonyid :: T.Text,
                       name :: T.Text } deriving (Show, Generic)
instance FromJSON Colony
instance ToJSON Colony

data AddColonyRPCMsg = AddColonyRPCMsg { colony  :: Colony, msgtype :: T.Text } 
                     | GetColoniesRPCMsg { msgtype :: T.Text }
                          deriving (Show, Generic)
instance FromJSON AddColonyRPCMsg
instance ToJSON AddColonyRPCMsg

parsePayload :: T.Text -> Either a BI.ByteString -> Maybe Colony
parsePayload payloadtype jsonEncoded = do
    case jsonEncoded of
        Left e -> Nothing  -- do Just Err { status = 500, message = "err" }
        Right j -> do let json = j
                      let lazyJSON = BL.fromStrict json
                      traceM "adding colony"
                      case payloadtype of
                          "addcolonymsg" -> JSON.decode lazyJSON :: Maybe Colony
                          --"getcoloniesmsg" -> JSON.decode lazyJSON :: Maybe [Colony]
                          -- "getcoloniesmsg" -> Just [Err { status = 500, message = T.pack $ BLI.unpackChars lazyJSON }]
                          _ -> Nothing -- Just Err { status = 500, message = "invalid payloadtype"}

parseResponse :: Either HttpException (Response BLI.ByteString) -> Maybe Colony
parseResponse eresp = case eresp of
    Left e -> Nothing -- Just Err { status = 500, message = "network error" }
    Right resp -> do let json = getResponseBody resp
                     let rpcReplyMsg = JSON.eitherDecode json :: Either String RPCReplyMsg
                     case rpcReplyMsg of
                          Left e -> do traceM "ERROR"
                                       traceM e
                                       Nothing -- Just Err { status = 500, message = "failed to decode json" }
                          Right p -> do let payload = getField @"payload" p 
                                        let payloadType = getField @"payloadtype" p
                                        let jsonEncoded = Base64.decode $ BI.packChars $ T.unpack payload
                                        parsePayload payloadType jsonEncoded 

parsePayload2 :: T.Text -> Either a BI.ByteString -> Maybe [Colony]
parsePayload2 payloadtype jsonEncoded = do
    case jsonEncoded of
        Left e -> do Nothing -- Just [Err { status = 500, message = "err" }]
        Right j -> do let json = j
                      let lazyJSON = BL.fromStrict json
                      case payloadtype of
                          --"addcolonymsg" -> JSON.decode lazyJSON :: Maybe Colony
                          "getcoloniesmsg" -> JSON.decode lazyJSON :: Maybe [Colony]
                          -- "getcoloniesmsg" -> Nothing -- Just [Err { status = 500, message = T.pack $ BLI.unpackChars lazyJSON }]
                          _ -> Nothing -- Just [Err { status = 500, message = "invalid payloadtype"}]

parseResponse2 :: Either HttpException (Response BLI.ByteString) -> Maybe [Colony]
parseResponse2 eresp = case eresp of
    Left e -> Nothing -- Just [Err { status = 500, message = "network error" }]
    Right resp -> do let json = getResponseBody resp
                     let rpcReplyMsg = JSON.decode json :: Maybe RPCReplyMsg
                     case rpcReplyMsg of
                          Nothing -> Nothing -- Just [Err { status = 500, message = "failed to decode json" }]
                          Just p -> do let payload = getField @"payload" p
                                       let payloadType = getField @"payloadtype" p
                                       let jsonEncoded = Base64.decode $ BI.packChars $ T.unpack payload
                                       parsePayload2 payloadType jsonEncoded


createColony :: IO () 
createColony = do
    prvkey <- prvkey
    colonyId <- identity prvkey
    let getColoniesRPCMsg = GetColoniesRPCMsg { msgtype="getcoloniesmsg"}
    let jsonEncoded = JSON.encode getColoniesRPCMsg 
    let base64Encoded = Base64.encode $ BL.toStrict jsonEncoded
    let serverPrvKey = "fcc79953d8a751bf41db661592dc34d30004b1a651ffa0725b03ac227641499d"
    signature <- sign (BI.unpackChars base64Encoded) serverPrvKey
    let rpcMsg = RPCMsg { signature = T.pack (signature), 
                          payload = T.pack (BI.unpackChars (base64Encoded)), 
                          payloadtype = "getcoloniesmsg" } 
    let jsonEncodedRPCMsg =  JSON.encode rpcMsg

    request' <- parseRequest "http://localhost:50080"
    let request
            = setRequestMethod "POST"
            $ setRequestPath "/api"
            $ setRequestBodyLBS jsonEncodedRPCMsg
            $ setRequestSecure False 
            $ request'
    eresponse <- try $ httpLBS request 
  
    --print eresponse

    let colonies = parseResponse2 eresponse
    print "----------------"
    print colonies 
    print "----------------"

addColony :: IO () 
addColony = do
    prvkey <- prvkey
    colonyId <- identity prvkey
    let colony = Colony { colonyid = T.pack colonyId, name="last" }
    let addColonyRPCMsg = AddColonyRPCMsg { colony = colony, msgtype="addcolonymsg"}
    let jsonEncoded = JSON.encode addColonyRPCMsg 
    let base64Encoded = Base64.encode $ BL.toStrict jsonEncoded
    let serverPrvKey = "fcc79953d8a751bf41db661592dc34d30004b1a651ffa0725b03ac227641499d"
    signature <- sign (BI.unpackChars base64Encoded) serverPrvKey
    let rpcMsg = RPCMsg { signature = T.pack (signature), 
                          payload = T.pack (BI.unpackChars (base64Encoded)), 
                          payloadtype = "addcolonymsg" } 
    let jsonEncodedRPCMsg =  JSON.encode rpcMsg

    request' <- parseRequest "http://localhost:50080"
    let request
            = setRequestMethod "POST"
            $ setRequestPath "/api"
            $ setRequestBodyLBS jsonEncodedRPCMsg
            $ setRequestSecure False 
            $ request'
    eresponse <- try $ httpLBS request 
   
    let colony = parseResponse eresponse
    print "" 
