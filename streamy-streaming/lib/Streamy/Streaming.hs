{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Streamy.Streaming (
          Stream
        , WrappedStream(..)
        , Groups
        , WrappedGroups(..)
        , yield
        , each
        , toList
        , toList_
        , chain
        , effects
        , Streamy.Streaming.concat
        , for
        , Streamy.Streaming.repeat
        , Streamy.Streaming.repeatM
        , Streamy.Streaming.take
        , Streamy.Streaming.takeWhile
        , Streamy.Streaming.map
        , Streamy.Streaming.mapM
        , Streamy.Streaming.mapM_
        , Streamy.Streaming.drop
        , Streamy.Streaming.dropWhile
        , Streamy.Streaming.filter
        , Streamy.Streaming.filterM
        , Streamy.Streaming.replicate
        , Streamy.Streaming.replicateM
        , Streamy.Streaming.all_
        , Streamy.Streaming.any_
        , Streamy.Streaming.fold
        , Streamy.Streaming.fold_
        , Streamy.Streaming.foldM
        , Streamy.Streaming.foldM_
        , Streamy.Streaming.scan
        , Streamy.Streaming.scanM
        , Streamy.Streaming.group
        , Streamy.Streaming.groupBy
        , Streamy.Streaming.chunksOf
        , Streamy.Streaming.maps
        , Streamy.Streaming.concats
        , Streamy.Streaming.intercalates
        , Streamy.Streaming.yields
        , Streamy.Streaming.takes
        , Streamy.Streaming.splitAt
        , Streamy.Streaming.span
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

type Groups = WrappedGroups

-- This newtype is necessary to avoid a
-- "Illegal parameterized type synonym in implementation of abstract data." error.
newtype WrappedGroups o m r = Groups { getGroups :: Q.Stream (Q.Stream (Of o) m) m r } 
        deriving (Functor,Applicative,Monad,MonadIO)

instance MonadTrans (WrappedGroups o) where
    lift x = Groups (lift x)

yield :: Monad m => o -> Stream o m ()
yield x = Stream (Q.yield x)

each :: (Monad m, Foldable f) => f a -> Stream a m ()
each x = Stream (Q.each x) 

toList :: Monad m => Stream a m r -> m ([a],r)
toList (Stream s) = toTup <$> Q.toList s

toList_ :: Monad m => Stream a m () -> m [a]
toList_ (Stream s) = Q.toList_ s

chain :: Monad m => (o -> m ()) -> Stream o m r -> Stream o m r
chain f (Stream s1) = Stream (Q.chain f s1)

effects :: Monad m => Stream o m r -> m r
effects (Stream s) = Q.effects s

concat :: (Monad m, Foldable f) => Stream (f a) m r -> Stream a m r
concat (Stream s) = Stream (Q.concat s)  

for :: Monad m => Stream a m r -> (a -> Stream b m ()) -> Stream b m r
for (Stream s) f = Stream (Q.for s (getStream . f))    

repeat :: Monad m => a -> Stream a m r
repeat = Stream . Q.repeat

repeatM :: Monad m => m a -> Stream a m r
repeatM = Stream . Q.repeatM

take :: Monad m => Int -> Stream o m r -> Stream o m () 
take i (Stream s) = Stream (Q.take i s)

takeWhile :: Monad m => (a -> Bool) -> Stream a m r -> Stream a m ()
takeWhile f (Stream s) = Stream (Q.takeWhile f s)

map :: Monad m => (a -> b) -> Stream a m r -> Stream b m r 
map f (Stream s) = Stream (Q.map f s)

mapM :: Monad m => (a -> m b) -> Stream a m r -> Stream b m r
mapM f (Stream s) = Stream (Q.mapM f s)  

mapM_ :: Monad m => (a -> m b) -> Stream a m r -> m r
mapM_ f (Stream s) = Q.mapM_ f s  

drop :: Monad m => Int -> Stream a m r -> Stream a m r
drop i (Stream s) = (Stream $ Q.drop i s)

dropWhile :: Monad m => (a -> Bool) -> Stream a m r -> Stream a m r
dropWhile f (Stream s) = (Stream $ Q.dropWhile f s)

filter :: Monad m => (a -> Bool) -> Stream a m r -> Stream a m r
filter f (Stream s) = (Stream $ Q.filter f s)  

filterM :: Monad m => (a -> m Bool) -> Stream a m r -> Stream a m r
filterM f (Stream s) = (Stream $ Q.filterM f s)  

replicate :: Monad m => Int -> a -> Stream a m ()
replicate i a = Stream $ Q.replicate i a

replicateM :: Monad m => Int -> m a -> Stream a m ()
replicateM i a = Stream $ Q.replicateM i a

all_ :: Monad m => (a -> Bool) -> Stream a m () -> m Bool
all_ f (Stream s) = Q.all_ f s

any_ :: Monad m => (a -> Bool) -> Stream a m () -> m Bool
any_ f (Stream s) = Q.any_ f s

fold :: Monad m => (x -> a -> x) -> x -> (x -> b) -> Stream a m r -> m (b,r)
fold step begin done (Stream s) = toTup <$> Q.fold step begin done s 

fold_ :: Monad m => (x -> a -> x) -> x -> (x -> b) -> Stream a m () -> m b
fold_ step begin done (Stream s) = Q.fold_ step begin done s 

foldM :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> Stream a m r -> m (b,r)
foldM step begin done (Stream s) = toTup <$> Q.foldM step begin done s 

foldM_ :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> Stream a m () -> m b
foldM_ step begin done (Stream s) = Q.foldM_ step begin done s 

scan :: Monad m => (x -> a -> x) -> x -> (x -> b) -> Stream a m r -> Stream b m r
scan step begin done (Stream s) = Stream $ Q.scan step begin done s

scanM :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> Stream a m r -> Stream b m r
scanM step begin done (Stream s) = Stream $ Q.scanM step begin done s

group :: (Monad m, Eq a) => Stream a m r -> Groups a m r
group (Stream s) = Groups (Q.group s)

groupBy :: Monad m => (a -> a -> Bool) -> Stream a m r -> Groups a m r
groupBy f (Stream s) = Groups $ Q.groupBy f s 

chunksOf :: Monad m => Int -> Stream a m r -> Groups a m r 
chunksOf i (Stream s) = Groups $ Q.chunksOf i s

maps :: Monad m => (forall x. Stream a m x -> Stream b m x) -> Groups a m r -> Groups b m r
maps f (Groups gs) = Groups $ Q.maps (getStream . f . Stream) gs

concats :: Monad m => Groups a m r -> Stream a m r
concats (Groups gs) = Stream $ Q.concats gs

intercalates :: Monad m => Stream a m () -> Groups a m r -> Stream a m r 
intercalates (Stream s) (Groups gs) = Stream $ Q.intercalates s gs

yields :: Monad m => Stream a m r -> Groups a m r
yields (Stream s) = Groups $ Q.yields s

takes :: Monad m => Int -> Groups a m () -> Groups a m ()
takes i (Groups gs) = Groups $ Q.takes i gs 

splitAt :: Monad m => Int -> Stream a m r -> Stream a m (Stream a m r)
splitAt i (Stream s) = Stream <$> Stream (Q.splitAt i s)

span :: Monad m => (a -> Bool) -> Stream a m r -> Stream a m (Stream a m r)
span f (Stream s) = Stream <$> Stream (Q.span f s)

--
toTup :: Of a r -> (a,r)
toTup = \(a :> r) -> (a,r)

