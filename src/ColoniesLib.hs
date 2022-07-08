{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module ColoniesLib  (
    Colony (..), 
    createColony
  ) where 

import Data.Aeson as JSON
import Data.Text
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

data RPCMsg = RPCMsg { signature   :: Text,
                       payloadtype :: Text,
                       payload     :: Text
                     } deriving (Show, Generic)
instance FromJSON RPCMsg
instance ToJSON RPCMsg

data RPCReplyMsg = RPCReplyMsg { error       :: Bool,
                                 payloadtype :: Text,
                                 replyPayload     :: Text
                               }
                               deriving (Show)
-- instance FromJSON RPCReplyMsg
-- instance ToJSON RPCReplyMsg

instance FromJSON RPCReplyMsg where
    parseJSON (Object v) = RPCReplyMsg <$> v .: "error" <*> v .: "payloadtype" <*> v .: "payload"

instance ToJSON RPCReplyMsg where 
    toJSON (RPCReplyMsg error payloadtype replyPayload) = object ["error" .= error, "payloadtype" .= payloadtype, "payload" .= replyPayload]

data Colony = Colony { colonyid :: Text,
                       name     :: Text
                     } deriving (Show, Generic)
instance FromJSON Colony
instance ToJSON Colony

data AddColonyRPCMsg = AddColonyRPCMsg { colony  :: Colony,
                                         msgtype :: Text
                                       } deriving (Show, Generic)
instance FromJSON AddColonyRPCMsg
instance ToJSON AddColonyRPCMsg

createColony :: IO () 
createColony = do
    prvkey <- prvkey
    colonyId <- identity prvkey
    let colony = Colony { colonyid = pack colonyId, name="last" }
    let addColonyRPCMsg = AddColonyRPCMsg { colony = colony, msgtype="addcolonymsg"}
    let jsonEncoded = JSON.encode addColonyRPCMsg 
    let base64Encoded = Base64.encode $ BL.toStrict jsonEncoded
    let serverPrvKey = "fcc79953d8a751bf41db661592dc34d30004b1a651ffa0725b03ac227641499d"
    signature <- sign (BI.unpackChars base64Encoded) serverPrvKey
    let rpcMsg = RPCMsg { signature = pack (signature), payload = pack (BI.unpackChars (base64Encoded)), payloadtype = "addcolonymsg" } 
    let jsonEncodedRPCMsg =  JSON.encode rpcMsg

    request' <- parseRequest "http://localhost:50080"
    let request
            = setRequestMethod "POST"
            $ setRequestPath "/api"
            $ setRequestBodyLBS jsonEncodedRPCMsg
            $ setRequestSecure False 
            $ request'
    eresponse <- try $ httpLBS request

    case eresponse of 
        Left e -> print (e :: HttpException)
        Right response -> do let json = getResponseBody response
                             let rpcReplyMsg = JSON.decode json :: Maybe RPCReplyMsg
                             case rpcReplyMsg of 
                                  Nothing -> print "nothing"
                                  Just p -> do let payload = replyPayload p 
                                               let jsonEncoded = Base64.decode (BI.packChars (unpack payload))
                                               case jsonEncoded of 
                                                    Left e -> print "nothing"
                                                    Right j -> do let colonyJSON = j
                                                                  -- let colony = JSON.decode $ BL.fromStrict j
                                                                  let lazyColonyJSON = BL.fromStrict colonyJSON
                                                                  let colony = JSON.decode lazyColonyJSON :: Maybe Colony 
                                                                  print (colony)
                                                                  print "" 
                                               print "" 

                             print ""

    print ""
