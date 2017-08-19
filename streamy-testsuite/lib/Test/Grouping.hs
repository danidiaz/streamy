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
    [
    ]

