{-# language GeneralizedNewtypeDeriving #-}
module Streamy.Streaming (Stream,yield,chain,effects,S(..)) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Morph

import Streaming (Of(..))
import qualified Streaming as Q
import qualified Streaming.Prelude as Q

type Stream = S

-- This newtype is necessary to avoid a
-- "Illegal parameterized type synonym in implementation of abstract data." error.
newtype S o m r = S { unS :: Q.Stream (Of o) m r } 
        deriving (Functor,Applicative,Monad,MonadIO,MonadTrans,MFunctor)

yield :: Monad m => o -> S o m ()
yield x = S (Q.yield x)

chain :: Monad m => (o -> m ()) -> Stream o m r -> Stream o m r
chain f (S s1) = S (Q.chain f s1)

effects :: Monad m => S o m r -> m r
effects (S s) = Q.effects s


