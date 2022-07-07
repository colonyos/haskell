module Main where

import Test.Tasty
import Test.Tasty.HUnit
import CryptoLib

main :: IO ()
main = do
    defaultMain (testGroup "CryptoLib" [testPrvkey,
                                        testIdentity,
                                        testSignature,
                                        testHash,
                                        testRecoverid])

testPrvkey = testCase "prvkey" $ do
    key <- prvkey
    let l = length key 
    assertBool "expecting length prvkey == 64" $ l == 64

testIdentity = testCase "identity" $ do
    let prvkey = "d6eb959e9aec2e6fdc44b5862b269e987b8a4d6f2baca542d8acaa97ee5e74f6" 
    identity <- identity prvkey
    let expectedIdentity = "5d6568f883451ae2e407d1a0a7992e414f2a67b69d0e6e9176d353b98f06f696"
    assertEqual ("expecting identity == " ++ expectedIdentity) identity expectedIdentity 

testSignature = testCase "sign" $ do
    let prvkey = "d6eb959e9aec2e6fdc44b5862b269e987b8a4d6f2baca542d8acaa97ee5e74f6" 
    signature <- sign "hello" prvkey
    let expectedSignature = "e713a1bb015fecabb5a084b0fe6d6e7271fca6f79525a634183cfdb175fe69241f4da161779d8e6b761200e1cf93766010a19072fa778f9643363e2cfadd640900"
    assertEqual ("expecting signature == " ++ expectedSignature) signature expectedSignature

testHash = testCase "hash" $ do
    digest <- hash "hello"
    let expectedDigest = "3338be694f50c5f338814986cdf0686453a888b84f424d792af4b9202398f392"
    assertEqual ("expecting digest == " ++ expectedDigest) digest expectedDigest

testRecoverid = testCase "recoverid" $ do
    let prvkey = "d6eb959e9aec2e6fdc44b5862b269e987b8a4d6f2baca542d8acaa97ee5e74f6" 
    let signature = "e713a1bb015fecabb5a084b0fe6d6e7271fca6f79525a634183cfdb175fe69241f4da161779d8e6b761200e1cf93766010a19072fa778f9643363e2cfadd640900"
    identity <- recoverid "hello" signature 
    let expectedIdentity = "5d6568f883451ae2e407d1a0a7992e414f2a67b69d0e6e9176d353b98f06f696"
    assertEqual ("expecting identity == " ++ expectedIdentity) identity expectedIdentity
