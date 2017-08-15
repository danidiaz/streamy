{-# language GeneralizedNewtypeDeriving #-}
module Streamy.Streaming (
          Stream
        , WrappedStream(..)
        , yield
        , each
        , toList
        , toList_
        , chain
        , effects
        , Streamy.Streaming.concat
    ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Morph

import Streaming (Of(..))
import qualified Streaming as Q
import qualified Streaming.Prelude as Q

type Stream = WrappedStream

-- This newtype is necessary to avoid a
-- "Illegal parameterized type synonym in implementation of abstract data." error.
newtype WrappedStream o m r = Stream { getStream :: Q.Stream (Of o) m r } 
        deriving (Functor,Applicative,Monad,MonadIO,MonadTrans,MFunctor)

yield :: Monad m => o -> Stream o m ()
yield x = Stream (Q.yield x)

each :: (Monad m, Foldable f) => f a -> Stream a m ()
each x = Stream (Q.each x) 

toList :: Monad m => Stream a m r -> m ([a],r)
toList (Stream s) = fmap (\(as :> r) -> (as,r)) $ Q.toList s

toList_ :: Monad m => Stream a m () -> m [a]
toList_ (Stream s) = Q.toList_ s

chain :: Monad m => (o -> m ()) -> Stream o m r -> Stream o m r
chain f (Stream s1) = Stream (Q.chain f s1)

effects :: Monad m => Stream o m r -> m r
effects (Stream s) = Q.effects s

concat :: (Monad m, Foldable f) => Stream (f a) m r -> Stream a m r
concat (Stream s) = Stream (Q.concat s)  


