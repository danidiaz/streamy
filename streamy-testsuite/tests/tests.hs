module Main where

import Test.Tasty

import Test.Pipes.Common
import Test.Streaming.Common
import Test.Conduit.Common


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = 
    testGroup "All" 
    [ testGroup "Common" 
      [ testGroup "Pipes" Test.Pipes.Common.common
      , testGroup "Streaming" Test.Streaming.Common.common
      , testGroup "Conduit" Test.Conduit.Common.common
      ]
    ]

