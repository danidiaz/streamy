{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.PipesStreaming (suite) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase,Assertion,assertEqual,assertBool)

import Test.PipesStreaming.Streamy (Stream,Groups)
import qualified Test.PipesStreaming.Streamy as Y
import qualified Test.PipesStreaming.Streamy.Bytes as YB

import Data.Foldable hiding (concat)
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Control.Concurrent.MVar
import Data.IORef

suite :: [TestTree]
suite = 
    [ testCase "group-map-concats" basic 
    , testCase "groupBy" testGroupBy
    , testCase "chunksOf" testChunksOf
    , testCase "intercalates" testIntercalates
    , testCase "yields" testYields
    , testCase "takes" testTakes
    , testCase "folds" testFolds
    , testCase "foldsM" testFoldsM
    , testCase "splitAt" testSplitAt
    , testCase "span" testSpan
    ]

basic :: Assertion
basic = do
   r <- Y.toList_ . Y.concats . Y.maps (\s -> Y.yield '<' *> s <* Y.yield '>') . Y.group $ Y.each "aabbcc"
   assertEqual "" "<aa><bb><cc>" r

testGroupBy :: Assertion
testGroupBy = do
   r <- Y.toList_ . Y.concats . Y.maps (\s -> Y.yield '<' *> s <* Y.yield '>') . Y.groupBy (==) $ Y.each "aabbcc"
   assertEqual "" "<aa><bb><cc>" r

testChunksOf :: Assertion
testChunksOf = do
   r <- Y.toList_ . Y.concats . Y.maps (\s -> Y.yield '<' *> s <* Y.yield '>') . Y.chunksOf 3 $ Y.each "aabbccd"
   assertEqual "" "<aab><bcc><d>" r

testIntercalates :: Assertion
testIntercalates = do
   r <- Y.toList_ . Y.intercalates (Y.each "||") . Y.group $ Y.each "aabbcc"
   assertEqual "" "aa||bb||cc" r

testYields :: Assertion
testYields = do
   r <- Y.toList_ . Y.concats . Y.yields $ Y.each "aaa" *> Y.each "bbb"
   assertEqual "" "aaabbb" r

testTakes :: Assertion
testTakes = do
   r <- Y.toList_ . Y.concats . Y.takes 2 . Y.groupBy (==) $ Y.each "aabbcc"
   assertEqual "" "aabb" r

testSplitAt :: Assertion
testSplitAt = do
   (str1,r) <- Y.toList . Y.splitAt 3 $ Y.each "aabbcc"
   str2 <- Y.toList_ r
   assertEqual "" "aab" str1
   assertEqual "" "bcc" str2

testSpan :: Assertion
testSpan = do
   (str1,r) <- Y.toList . Y.span (< 'd') $ Y.each "abcdefg"
   str2 <- Y.toList_ r
   assertEqual "" "abc" str1
   assertEqual "" "defg" str2

testFolds :: Assertion
testFolds = do
   r <- Y.toList_ . Y.folds (flip (:)) [] id . Y.group $ Y.each "aabbcc"
   assertEqual "" ["aa","bb","cc"] r
   
testFoldsM :: Assertion
testFoldsM = do
   ref <- newIORef False
   r <- Y.toList_ 
      . Y.foldsM (\x i -> pure (i:x)) (return []) (\x -> writeIORef ref True *> pure x) 
      . Y.group 
      $ Y.each "aabbcc"
   assertEqual "" ["aa","bb","cc"] r
   ref' <- readIORef ref
   assertBool "effect" ref'
