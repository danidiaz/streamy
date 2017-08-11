module Streamy.Conduit (Stream,yield,chain,effects) where

import qualified Conduit as C

type Stream = C.ConduitM ()

yield :: Monad m => o -> Stream o m ()
yield = C.yield

chain :: Monad m => (o -> m ()) -> Stream o m r -> Stream o m r
chain f s = C.fuseUpstream s (C.mapMC (\i -> f i *> pure i))

effects :: Monad m => Stream o m r -> m r
effects s = C.runConduit $ C.fuseUpstream s C.sinkNull


