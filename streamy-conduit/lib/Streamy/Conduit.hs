module Streamy.Conduit (
          Stream
        , yield
        , each
        , toList
        , toList_
        , chain
        , effects
    ) where

import qualified Conduit as C
import qualified Data.Conduit.List as CL

import Data.Foldable (Foldable)
import qualified Data.Foldable 
import Data.Tuple (swap)

type Stream = C.ConduitM ()

yield :: Monad m => o -> Stream o m ()
yield = C.yield

each :: (Monad m, Foldable f) => f o -> Stream o m ()
each = CL.sourceList . Data.Foldable.toList

chain :: Monad m => (o -> m ()) -> Stream o m r -> Stream o m r
chain f s = C.fuseUpstream s (C.mapMC (\i -> f i *> pure i))

effects :: Monad m => Stream o m r -> m r
effects s = C.runConduit $ C.fuseUpstream s C.sinkNull

toList :: Monad m => Stream a m r -> m ([a],r)
toList s = fmap swap $ C.runConduit $ C.fuseBoth s C.sinkList

toList_ :: Monad m => Stream a m () -> m [a]
toList_ s = C.connect s C.sinkList

