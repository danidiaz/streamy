module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Streamy.Test.Pipes

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = 
    testGroup "Tests" 
    []


