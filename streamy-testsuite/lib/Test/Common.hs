module Test.Common (common) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase,Assertion,assertEqual,assertBool)

import Test.Common.Streamy (Stream)
import qualified Test.Common.Streamy as Y

import Control.Monad.IO.Class
import Control.Concurrent.MVar

common :: [TestTree]
common = 
    [ testCase "basic" basic 
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





