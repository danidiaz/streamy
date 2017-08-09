module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.Trans.Stream.Test.Pipes

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = 
    testGroup "Tests" 
    []


