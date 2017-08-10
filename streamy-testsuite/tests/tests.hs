module Main where

import Test.Tasty
import Test.Tasty.HUnit

import PipeTests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = 
    testGroup "Tests" 
    []


