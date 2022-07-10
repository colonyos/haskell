module CryptoLib
    ( generateKey, identity, sign, hash, recoverid
    ) where

{-# LANGUAGE ForeignFunctionInterface #-}
import Foreign
import Foreign.C.Types
import Foreign.C.String

foreign import ccall "prvkey" c_prvkey :: IO CString
foreign import ccall "id" c_identity :: CString -> IO CString
foreign import ccall "sign" c_sign :: CString -> CString -> IO CString
foreign import ccall "hash" c_hash :: CString -> IO CString
foreign import ccall "recoverid" c_recoverid:: CString -> CString -> IO CString

generateKey :: IO String
generateKey = do k <- c_prvkey
                 str <- peekCString k
                 _ <- free k
                 return str 

identity :: String -> IO String
identity prvkey = do prvkeyc <- newCString prvkey 
                     identityc <- c_identity prvkeyc 
                     identity <- peekCString identityc 
                     _ <- free identityc 
                     _ <- free prvkeyc 
                     return identity 

sign :: String -> String -> IO String
sign msg prvkey = do msgc <- newCString msg
                     prvkeyc <- newCString prvkey
                     sigc <- c_sign msgc prvkeyc
                     sig <- peekCString sigc
                     _ <- free sigc
                     _ <- free msgc
                     _ <- free prvkeyc
                     return sig

hash :: String -> IO String
hash msg = do msgc <- newCString msg
              hashc <- c_hash msgc
              hash <- peekCString hashc 
              _ <- free hashc
              _ <- free msgc
              return hash 

recoverid :: String -> String -> IO String
recoverid msg sig = do msgc <- newCString msg
                       sigc <- newCString sig
                       identityc <- c_recoverid msgc sigc
                       identity <- peekCString identityc 
                       _ <- free identityc 
                       _ <- free msgc
                       _ <- free sigc
                       return identity 
