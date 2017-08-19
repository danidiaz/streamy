{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Grouping (grouping) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase,Assertion,assertEqual,assertBool)

import Test.Grouping.Streamy (Stream,Groups)
import qualified Test.Grouping.Streamy as Y

import Data.Foldable hiding (concat)
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Control.Concurrent.MVar
import Data.IORef

grouping :: [TestTree]
grouping = 
    [ testCase "group-map-concats" basic 
    , testCase "groupBy" testGroupBy
    , testCase "intercalates" testIntercalates
    , testCase "yields" testYields
    ]

basic :: Assertion
basic = do
   r <- Y.toList_ . Y.concats . Y.maps (\s -> Y.yield '<' *> s <* Y.yield '>') . Y.group $ Y.each "aabbcc"
   assertEqual "" "<aa><bb><cc>" r

testGroupBy :: Assertion
testGroupBy = do
   r <- Y.toList_ . Y.concats . Y.maps (\s -> Y.yield '<' *> s <* Y.yield '>') . Y.groupBy (==) $ Y.each "aabbcc"
   assertEqual "" "<aa><bb><cc>" r

testIntercalates :: Assertion
testIntercalates = do
   r <- Y.toList_ . Y.intercalates (Y.each "||") . Y.group $ Y.each "aabbcc"
   assertEqual "" "aa||bb||cc" r

testYields :: Assertion
testYields = do
   r <- Y.toList_ . Y.concats . Y.yields $ Y.each "aaa" *> Y.each "bbb"
   assertEqual "" "aaabbb" r

