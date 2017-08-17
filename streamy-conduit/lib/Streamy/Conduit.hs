{-# LANGUAGE RankNTypes #-}
module Streamy.Conduit (
          Stream
        , yield
        , each
        , toList
        , toList_
        , chain
        , effects
        , Streamy.Conduit.concat
        , for
        , Streamy.Conduit.repeat
        , Streamy.Conduit.repeatM
        , Streamy.Conduit.take
        , Streamy.Conduit.takeWhile
        , Streamy.Conduit.map
        , Streamy.Conduit.mapM
        , Streamy.Conduit.mapM_
        , Streamy.Conduit.drop
        , Streamy.Conduit.dropWhile
        , Streamy.Conduit.filter
        , Streamy.Conduit.filterM
        , Streamy.Conduit.replicate
        , Streamy.Conduit.replicateM
        , Streamy.Conduit.any_
        , Streamy.Conduit.all_
        , Streamy.Conduit.fold
        , Streamy.Conduit.fold_
        , Streamy.Conduit.foldM
        , Streamy.Conduit.foldM_
    ) where

import qualified Conduit as C
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL

import Data.Foldable (Foldable)
import qualified Data.Foldable 
import Data.Functor (void)
import Data.Tuple (swap)
import Control.Monad.Trans.Class

type Stream = C.ConduitM ()

yield :: Monad m => o -> Stream o m ()
yield = C.yield

each :: (Monad m, Foldable f) => f o -> Stream o m ()
each = CL.sourceList . Data.Foldable.toList

toList :: Monad m => Stream a m r -> m ([a],r)
toList s = fmap swap $ C.runConduit $ C.fuseBoth s C.sinkList

toList_ :: Monad m => Stream a m () -> m [a]
toList_ s = C.connect s C.sinkList

chain :: Monad m => (o -> m ()) -> Stream o m r -> Stream o m r
chain f s = C.fuseUpstream s (C.mapMC (\i -> f i *> pure i))

effects :: Monad m => Stream o m r -> m r
effects s = C.runConduit $ C.fuseUpstream s C.sinkNull

concat :: (Monad m, Foldable f) => Stream (f a) m r -> Stream a m r
concat s = C.fuseUpstream s CL.concat

for :: Monad m => Stream a m r -> (a -> Stream b m ()) -> Stream b m r
for s f = do
    -- Is there a more succint way of doing this?
    let go = do
            x <- C.await
            case x of
                Just v -> do _ <- C.fuse (CC.map (const ())) (f v)
                             go 
                Nothing -> return ()
    C.fuseUpstream s go

repeat :: Monad m => a -> Stream a m r
repeat a = fmap (\() -> error "never happens") $ CC.repeat a

repeatM :: Monad m => m a -> Stream a m r
repeatM a = fmap (\() -> error "never happens") $ CC.repeatM a

take :: Monad m => Int -> Stream o m r -> Stream o m () 
take i c = C.fuse (fmap (const ()) c) $ CC.take i

takeWhile :: Monad m => (a -> Bool) -> Stream a m r -> Stream a m ()
takeWhile f c = C.fuse (void c) $ CC.takeWhile f

map :: Monad m => (a -> b) -> Stream a m r -> Stream b m r 
map f c = C.fuseUpstream c (CC.map f)

mapM :: Monad m => (a -> m b) -> Stream a m r -> Stream b m r
mapM f c = C.fuseUpstream c (CC.mapM f)

mapM_ :: Monad m => (a -> m b) -> Stream a m r -> m r
mapM_ f c = C.runConduit $ C.fuseUpstream c (CC.mapM_ (void . f))

drop :: Monad m => Int -> Stream a m r -> Stream a m r
drop i c = 
    -- https://stackoverflow.com/questions/10834773/how-to-use-the-conduit-drop-function-in-a-pipeline
    C.fuseUpstream c (C.dropC i >> CL.map id)

dropWhile :: Monad m => (a -> Bool) -> Stream a m r -> Stream a m r
dropWhile f c = 
    -- https://stackoverflow.com/questions/10834773/how-to-use-the-conduit-drop-function-in-a-pipeline
    C.fuseUpstream c (C.dropWhileC f >> CL.map id)

filter :: Monad m => (a -> Bool) -> Stream a m r -> Stream a m r
filter f c = C.fuseUpstream c (CC.filter f)

filterM :: Monad m => (a -> m Bool) -> Stream a m r -> Stream a m r
filterM f c = C.fuseUpstream c (CC.filterM f)

replicate :: Monad m => Int -> a -> Stream a m ()
replicate i a = CC.replicate i a

replicateM :: Monad m => Int -> m a -> Stream a m ()
replicateM i a = CC.replicateM i a

all_ :: Monad m => (a -> Bool) -> Stream a m () -> m Bool
all_ f c = C.connect c (CC.all f)

any_ :: Monad m => (a -> Bool) -> Stream a m () -> m Bool
any_ f c = C.connect c (CC.any f)

fold :: Monad m => (x -> a -> x) -> x -> (x -> b) -> Stream a m r -> m (b,r)
fold step begin done c = fmap swap $ C.runConduit $ C.fuseBoth c (sinkFold step begin done) 

fold_ :: Monad m => (x -> a -> x) -> x -> (x -> b) -> Stream a m () -> m b
fold_ step begin done c = C.connect c (sinkFold step begin done)

foldM :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> Stream a m r -> m (b,r)
foldM step begin done c = fmap swap $ C.runConduit $ C.fuseBoth c (sinkFoldM step begin done) 

foldM_ :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> Stream a m () -> m b
foldM_ step begin done c = C.connect c (sinkFoldM step begin done)

-- Copied from conduit-extra
-- http://hackage.haskell.org/package/conduit-extra-1.1.16/docs/src/Data-Conduit-Foldl.html#sinkFold 
sinkFold :: Monad m => (x -> a -> x) -> x -> (x -> b) -> C.Consumer a m b
sinkFold combine seed extract = fmap extract (CL.fold combine seed)

-- Copied from conduit-extra
-- http://hackage.haskell.org/package/conduit-extra-1.1.16/docs/Data-Conduit-Foldl.html#v:sinkFoldM
sinkFoldM :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> C.Consumer a m b
sinkFoldM combine seed extract =
  lift . extract =<< CL.foldM combine =<< lift seed
