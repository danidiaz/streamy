{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
module Streamy.Pipes (
          Stream
        , Groups
        , WrappedGroups(..)
        , Index
        , yield
        , toList
        , toList_
        , each
        , chain
        , effects
        , Streamy.Pipes.concat
        , for
        , Streamy.Pipes.repeat
        , Streamy.Pipes.repeatM
        , Streamy.Pipes.take
        , Streamy.Pipes.takeWhile
        , Streamy.Pipes.map
        , Streamy.Pipes.mapM
        , Streamy.Pipes.mapM_
        , Streamy.Pipes.drop
        , Streamy.Pipes.dropWhile
        , Streamy.Pipes.filter
        , Streamy.Pipes.filterM
        , Streamy.Pipes.replicate
        , Streamy.Pipes.replicateM
        , Streamy.Pipes.any_
        , Streamy.Pipes.all_
        , Streamy.Pipes.fold
        , Streamy.Pipes.fold_
        , Streamy.Pipes.foldM
        , Streamy.Pipes.foldM_
        , Streamy.Pipes.scan
        , Streamy.Pipes.scanM
        , Streamy.Pipes.group
        , Streamy.Pipes.groupBy
        , Streamy.Pipes.chunksOf
        , Streamy.Pipes.maps
        , Streamy.Pipes.concats
        , Streamy.Pipes.intercalates
        , Streamy.Pipes.yields
        , Streamy.Pipes.takes
        , Streamy.Pipes.folds
        , Streamy.Pipes.foldsM
        , Streamy.Pipes.splitAt
        , Streamy.Pipes.span
        , delimit 
    ) where

import qualified Data.List
import Data.List.NonEmpty (NonEmpty(..))
import Pipes (Proxy,X,(>->))
import qualified Pipes as P
import qualified Pipes.Prelude as PP
import qualified Pipes.Group as PG
import qualified Pipes.Parse

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Free (liftF)
import Control.Monad.IO.Class

import Lens.Micro.Extras (view)

type Stream = Proxy X () ()

type Groups = WrappedGroups

-- This newtype is necessary to avoid a
-- "Illegal parameterized type synonym in implementation of abstract data." error.
newtype WrappedGroups o m r = Groups { getGroups :: PG.FreeT (P.Producer o m) m r } 
        deriving (Functor,Applicative,Monad,MonadIO)

instance MonadTrans (WrappedGroups o) where
    lift x = Groups (lift x)

type Index = Int

yield :: Monad m => o -> Stream o m ()
yield = P.yield

each :: (Monad m, Foldable f) => f a -> Stream a m ()
each = P.each

chain :: Monad m => (o -> m ()) -> Stream o m r -> Stream o m r
chain action producer = producer >-> PP.chain action

effects :: Monad m => Stream o m r -> m r
effects producer = P.runEffect $ producer >-> PP.drain   

toList :: Monad m => Stream a m r -> m ([a],r)
toList = PP.toListM'

toList_ :: Monad m => Stream a m () -> m [a]
toList_ = PP.toListM

concat :: (Monad m, Foldable f) => Stream (f a) m r -> Stream a m r
concat producer = P.for producer each

for :: Monad m => Stream a m r -> (a -> Stream b m ()) -> Stream b m r
for = P.for

repeat :: Monad m => a -> Stream a m r
repeat = PP.repeatM . return

repeatM :: Monad m => m a -> Stream a m r
repeatM = PP.repeatM

take :: Monad m => Int -> Stream o m r -> Stream o m () 
take i producer = P.void producer >-> PP.take i

takeWhile :: Monad m => (a -> Bool) -> Stream a m r -> Stream a m ()
takeWhile f producer = P.void producer >-> PP.takeWhile f

map :: Monad m => (a -> b) -> Stream a m r -> Stream b m r 
map f producer = producer >-> PP.map f

mapM :: Monad m => (a -> m b) -> Stream a m r -> Stream b m r
mapM f producer = producer >-> PP.mapM f

mapM_ :: Monad m => (a -> m b) -> Stream a m r -> m r
mapM_ f producer = P.runEffect $ producer >-> PP.mapM_ (P.void . f)

drop :: Monad m => Int -> Stream a m r -> Stream a m r
drop i producer = producer >-> PP.drop i

dropWhile :: Monad m => (a -> Bool) -> Stream a m r -> Stream a m r
dropWhile f producer = producer >-> PP.dropWhile f

filter :: Monad m => (a -> Bool) -> Stream a m r -> Stream a m r
filter f producer = producer >-> PP.filter f

filterM :: Monad m => (a -> m Bool) -> Stream a m r -> Stream a m r
filterM f producer = producer >-> PP.filterM f

replicate :: Monad m => Int -> a -> Stream a m ()
replicate i a = P.each $ Data.List.replicate i a  

replicateM :: Monad m => Int -> m a -> Stream a m ()
replicateM i a = PP.replicateM i a 

all_ :: Monad m => (a -> Bool) -> Stream a m () -> m Bool
all_ = PP.all

any_ :: Monad m => (a -> Bool) -> Stream a m () -> m Bool
any_ = PP.any

fold :: Monad m => (x -> a -> x) -> x -> (x -> b) -> Stream a m r -> m (b,r)
fold = PP.fold'

fold_ :: Monad m => (x -> a -> x) -> x -> (x -> b) -> Stream a m () -> m b
fold_ = PP.fold

foldM :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> Stream a m r -> m (b,r)
foldM = PP.foldM'

foldM_ :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> Stream a m () -> m b
foldM_ = PP.foldM

scan :: Monad m => (x -> a -> x) -> x -> (x -> b) -> Stream a m r -> Stream b m r
scan step begin done producer = producer >-> PP.scan step begin done

scanM :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> Stream a m r -> Stream b m r
scanM step begin done producer = producer >-> PP.scanM step begin done

group :: (Monad m, Eq a) => Stream a m r -> Groups a m r
group producer = Groups (view PG.groups producer)

groupBy :: Monad m => (a -> a -> Bool) -> Stream a m r -> Groups a m r
groupBy f producer = Groups (view (PG.groupsBy f) producer)

chunksOf :: Monad m => Int -> Stream a m r -> Groups a m r 
chunksOf i producer = Groups (view (PG.chunksOf i) producer)

maps :: Monad m => (forall x. Stream a m x -> Stream b m x) -> Groups a m r -> Groups b m r
maps f (Groups gs) = Groups $ PG.maps f gs

concats :: Monad m => Groups a m r -> Stream a m r
concats (Groups gs) = PG.concats gs

intercalates :: Monad m => Stream a m () -> Groups a m r -> Stream a m r 
intercalates producer (Groups gs) = PG.intercalates producer gs

yields :: Monad m => Stream a m r -> Groups a m r
yields producer = Groups $ liftF producer

takes :: Monad m => Int -> Groups a m () -> Groups a m ()
takes i (Groups gs) = Groups $ PG.takes i gs 

folds :: Monad m => (x -> a -> x) -> x -> (x -> b) -> Groups a m r -> Stream b m r
folds step begin done (Groups gs) = PG.folds step begin done gs

foldsM :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> Groups a m r -> Stream b m r
foldsM step begin done (Groups gs) = PG.foldsM step begin done gs

splitAt :: Monad m => Int -> Stream a m r -> Stream a m (Stream a m r)
splitAt i producer = view (Pipes.Parse.splitAt i) producer

span :: Monad m => (a -> Bool) -> Stream a m r -> Stream a m (Stream a m r)
span f producer = view (Pipes.Parse.span f) producer

delimit :: Monad m => (x -> a -> (x, NonEmpty [b])) -> (x -> NonEmpty [b]) -> x -> Stream a m r -> Groups b m r
delimit step done state0 stream0 = Groups initial
  where 
    initial = do
        r <- lift $ P.runEffect $ advance state0 stream0 >-> PP.drain
        r
    layered = flip $ foldr $ \x c -> P.yield x *> c
    divided = flip $ foldr $ \xs c -> return . PG.FreeT . return . PG.Free $ xs `layered` c
    advance state stream = do
        r <- lift $ PG.next stream -- use the effect constructor here?
        case r of
            Left r -> do
                let (!headlist :| rest) = done state
                headlist `layered` (rest `divided` (return (return r)))
            Right (a,stream') -> do
                let (!state', !headlist :| rest) = step state a
                headlist `layered` (rest `divided` advance state' stream')

