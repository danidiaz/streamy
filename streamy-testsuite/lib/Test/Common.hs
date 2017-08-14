module Test.Common (common) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase,Assertion,assertEqual,assertBool)

import Test.Common.Streamy (Stream)
import qualified Test.Common.Streamy as Y

import Control.Monad.IO.Class
import Control.Concurrent.MVar

common :: [TestTree]
common = 
    [ testCase "yield-chain-effects" basic 
    , testCase "each-toList" eachToList
    , testCase "each-toList_" eachToList_
    ]

basic :: Assertion
basic = do
   ref <- newMVar ""
   let addChar c = modifyMVar_ ref (return . (c:))
       stream = Y.yield 'a' *> Y.yield 'b' *> Y.yield 'c' *> liftIO (addChar 'd') *> return True
   b <- Y.effects . Y.chain addChar $ stream 
   acc <- readMVar ref
   assertBool "Stream returned unexpectd value." b
   assertEqual "Wrong accumulator value" "abcd" (reverse acc)

eachToList :: Assertion
eachToList = do
    let msg = "this is a test"
    (msg',b) <- Y.toList $ Y.each msg *> return True
    assertBool "Stream returned unexpectd value." b
    assertEqual "Wrong list value" msg msg'

eachToList_ :: Assertion
eachToList_ = do
    let msg = "this is a test"
    msg' <- Y.toList_ $ Y.each msg
    assertEqual "Wrong list value" msg msg'


