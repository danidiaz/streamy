module Streamy.Pipes (Stream,yield,chain) where

import Pipes (Proxy,X,(>->))
import qualified Pipes as P
import qualified Pipes.Prelude as PP

type Stream = Proxy X () ()

yield :: Monad m => o -> Stream o m ()
yield = P.yield

chain :: Monad m => (o -> m ()) -> Stream o m r -> Stream o m r
chain action producer = producer >-> PP.chain action
