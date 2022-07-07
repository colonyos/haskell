module Main where

import CryptoLib

main :: IO ()
main = do k <- prvkey
          i <- identity "d6eb959e9aec2e6fdc44b5862b269e987b8a4d6f2baca542d8acaa97ee5e74f6"
          h <- hash "hello"
          s <- sign h k
          s2 <- sign "hello" "d6eb959e9aec2e6fdc44b5862b269e987b8a4d6f2baca542d8acaa97ee5e74f6"
          print i
          print k
          print h
          print s
          print s2
