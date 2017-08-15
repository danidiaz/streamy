{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Common (common) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase,Assertion,assertEqual,assertBool)

import Test.Common.Streamy (Stream)
import qualified Test.Common.Streamy as Y

import Data.Foldable hiding (concat)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer
import Control.Concurrent.MVar

common :: [TestTree]
common = 
    [ testCase "yield-chain-effects" basic 
    , testCase "each-toList" eachToList
    , testCase "each-toList_" eachToList_
    , testCase "concat" testConcat
    , testCase "for" testFor
    , testCase "repeat-take" testRepeatTake
    , testCase "repeatM-take" testRepeatMTake
    , testCase "map" testMap
    , testCase "mapM" testMapM
    , testCase "mapM_" testMapM_
    , testCase "drop" testDrop
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

testConcat :: Assertion
testConcat = do
    let cs = [['t','h'],['i','s']]
    msg' <- Y.toList_ . Y.concat . Y.each $ cs
    assertEqual "" (join cs) msg'

testFor :: Assertion
testFor = do
    let msg = "bmx"
    msg' <- Y.toList_ $ Y.for (Y.each msg) $ \c -> Y.each [succ c,pred c]
    assertEqual "" "canlyw" msg'
    msg'' <- Y.toList_ $ Y.for (Y.each msg) $ \_ -> return ()
    assertEqual "nooutput" "" msg''

testRepeatTake :: Assertion
testRepeatTake = do
    msg' <- Y.toList_ . Y.take 3 $ Y.repeat 'a'
    assertEqual "" "aaa" msg'

testRepeatMTake :: Assertion
testRepeatMTake = do
    (msg',acc) <- runWriterT $ Y.toList_ . Y.take 3 $ Y.repeatM (tell "z" >> return 'a')
    assertEqual "msg" "aaa" msg'
    assertEqual "acc" "zzz" acc

testMap :: Assertion
testMap = do
    msg' <- Y.toList_ . Y.map succ $ Y.each "amx"
    assertEqual "msg" "bny" msg'

testMapM :: Assertion
testMapM = do
    (msg',acc) <- runWriterT $ Y.toList_ . Y.mapM (\c -> tell "z" >> return (succ c)) $ Y.each "amx"
    assertEqual "msg" "bny" msg'
    assertEqual "acc" "zzz" acc

testMapM_ :: Assertion
testMapM_ = do
    let msg = "abc"
    (_,acc) <- runWriterT $ Y.effects . Y.mapM (\c -> tell [c]) $ Y.each msg
    assertEqual "acc" msg acc

testDrop :: Assertion
testDrop = do
    let msg = "abcd"
    msg' <- Y.toList_ . Y.drop 2 $ Y.each msg
    assertEqual "acc" "cd" msg'

