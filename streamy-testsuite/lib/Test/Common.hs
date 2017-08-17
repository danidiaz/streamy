{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Common (common) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase,Assertion,assertEqual,assertBool)

import Test.Common.Streamy (Stream)
import qualified Test.Common.Streamy as Y

import Data.Foldable hiding (concat)
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Control.Concurrent.MVar
import Data.IORef

common :: [TestTree]
common = 
    [ testCase "yield-chain-effects" basic 
    , testCase "each-toList" eachToList
    , testCase "each-toList_" eachToList_
    , testCase "concat" testConcat
    , testCase "for" testFor
    , testCase "repeat-take" testRepeatTake
    , testCase "repeatM-take" testRepeatMTake
    , testCase "takeWhile" testTakeWhile
    , testCase "map" testMap
    , testCase "mapM" testMapM
    , testCase "mapM_" testMapM_
    , testCase "drop" testDrop
    , testCase "dropWhile" testDropWhile
    , testCase "filter" testFilter
    , testCase "filterM" testFilterM
    , testCase "replicate" testReplicate
    , testCase "replicateM" testReplicateM
    , testCase "all_" testAll_
    , testCase "any_" testAny_
    , testCase "fold" testFold
    , testCase "fold_" testFold_
    , testCase "foldM" testFoldM
    , testCase "foldM_" testFoldM_
    , testCase "scan" testScan
    , testCase "scanM" testScanM
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

testTakeWhile :: Assertion
testTakeWhile = do
    msg' <- Y.toList_ . Y.takeWhile (< 3) $ Y.each [1::Int,2,3,4,5]
    assertEqual "" [1,2] msg'

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

testDropWhile :: Assertion
testDropWhile = do
    let msg = "aacca"
    msg' <- Y.toList_ . Y.dropWhile (=='a') $ Y.each msg
    assertEqual "acc" "cca" msg'

testFilter :: Assertion
testFilter = do
    let msg = "abacada"
    msg' <- Y.toList_ . Y.filter (/='a') $ Y.each msg
    assertEqual "acc" "bcd" msg'

testFilterM :: Assertion
testFilterM = do
    let msg = "abacada"
    msg' <- Y.toList_ . Y.filterM (return . (/='a')) $ Y.each msg
    assertEqual "acc" "bcd" msg'

testReplicate :: Assertion
testReplicate = do
    msg' <- Y.toList_ $ Y.replicate 5 'a'
    assertEqual "acc" "aaaaa" msg'

testReplicateM :: Assertion
testReplicateM = do
    msg' <- Y.toList_ $ Y.replicateM 5 (pure 'a')
    assertEqual "acc" "aaaaa" msg'

testAll_ :: Assertion
testAll_ = do
    ref <- newIORef True
    res <- Y.all_ (<5) $ Y.yield (1::Int) 
                         *> 
                         Y.yield 2 
                         *> 
                         Y.yield 7 
                         *> 
                         lift (writeIORef ref False) 
                         *> 
                         Y.yield 8
    ref' <- readIORef ref
    assertEqual "result" False res
    assertEqual "ref" True ref'

testAny_ :: Assertion
testAny_ = do
    ref <- newIORef True
    res <- Y.any_ (<5) $ Y.yield (8::Int) 
                         *> 
                         Y.yield 9 
                         *> 
                         Y.yield 2 
                         *> 
                         lift (writeIORef ref False) 
                         *> 
                         Y.yield 8
    ref' <- readIORef ref
    assertEqual "result" True res
    assertEqual "ref" True ref'

testFold :: Assertion
testFold = do
    (b,r) <- Y.fold (+) 7 negate $ Y.each [1::Int,2,3] *> return True
    assertEqual "foldresult" (negate 13) b
    assertBool "streamresult" r
    
testFold_ :: Assertion
testFold_ = do
    b <- Y.fold_ (+) 7 negate $ Y.each [1::Int,2,3] 
    assertEqual "foldresult" (negate 13) b
    
testFoldM :: Assertion
testFoldM = do
    ref <- newIORef False
    (b,r) <- Y.foldM (\x i -> pure $ x + i) (pure 7) (\acc -> writeIORef ref True *> pure (negate acc)) $ 
                Y.each [1::Int,2,3] *> return True
    assertEqual "foldresult" (negate 13) b
    assertBool "streamresult" r
    ref' <- readIORef ref
    assertBool "effectreamresult" ref'

testFoldM_ :: Assertion
testFoldM_ = do
    ref <- newIORef False
    b <- Y.foldM_ (\x i -> pure $ x + i) (pure 7) (\acc -> writeIORef ref True *> pure (negate acc)) $ 
                Y.each [1::Int,2,3]
    assertEqual "foldresult" (negate 13) b
    ref' <- readIORef ref
    assertBool "effectreamresult" ref'

testScan :: Assertion
testScan = do
    res <- Y.toList_ . Y.scan (flip (:)) [] (map (*3)) $ Y.each [1::Int,2,3]
    assertEqual "foldresult" [[],[3],[6,3],[9,6,3]] res

testScanM :: Assertion
testScanM = do
    res <- Y.toList_ . Y.scanM (\x i -> pure $ i : x) (pure []) (pure . map (*3)) $ Y.each [1::Int,2,3]
    assertEqual "foldresult" [[],[3],[6,3],[9,6,3]] res

