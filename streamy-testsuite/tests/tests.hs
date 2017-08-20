module Main where

import Test.Tasty

import Test.Pipes.Common
import Test.Pipes.PipeStreaming

import Test.Streaming.Common
import Test.Streaming.PipeStreaming

import Test.Conduit.Common


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = 
    testGroup "All" 
    [ testGroup "Pipes" 
      [ testGroup "Common"         Test.Pipes.Common.suite
      , testGroup "PipesStreaming" Test.Pipes.PipeStreaming.suite
      ]
    , testGroup "Streaming" 
      [ testGroup "Common"         Test.Streaming.Common.suite
      , testGroup "PipesStreaming" Test.Streaming.PipeStreaming.suite
      ]
    , testGroup "Conduit" 
      [ testGroup "Common"         Test.Conduit.Common.suite
      ]
    ]
