module CryptoLib
    ( print_prvkey 
    ) where

{-# LANGUAGE ForeignFunctionInterface #-}
import Foreign
import Foreign.C.Types
import Foreign.C.String

foreign import ccall "prvkey" c_prvkey :: IO CString

prvkey = do k <- c_prvkey
            res <- peekCString k
            _ <- free k
            return res

print_prvkey :: IO ()
print_prvkey = do k <- prvkey
                  print $ k
