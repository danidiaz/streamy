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
        , Streamy.Conduit.map
        , Streamy.Conduit.mapM
        , Streamy.Conduit.mapM_
        , Streamy.Conduit.drop
    ) where

import qualified Conduit as C
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL

import Data.Foldable (Foldable)
import qualified Data.Foldable 
import Data.Functor (void)
import Data.Tuple (swap)

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

map :: Monad m => (a -> b) -> Stream a m r -> Stream b m r 
map f c = C.fuseUpstream c (CC.map f)

mapM :: Monad m => (a -> m b) -> Stream a m r -> Stream b m r
mapM f c = C.fuseUpstream c (CC.mapM f)

mapM_ :: Monad m => (a -> m b) -> Stream a m r -> m r
mapM_ f c = C.runConduit $ C.fuseUpstream c (CC.mapM_ (void . f))

drop :: Monad m => Int -> Stream a m r -> Stream a m r
drop i c = C.fuseUpstream c (C.dropC i)

