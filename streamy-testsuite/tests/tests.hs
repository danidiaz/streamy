module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Test.Pipes.Common

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = 
    testGroup "Tests" 
    []


