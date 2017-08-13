module Streamy.Pipes (Stream,yield,chain,effects) where

import Pipes (Proxy,X,(>->))
import qualified Pipes as P
import qualified Pipes.Prelude as PP

type Stream = Proxy X () ()

yield :: Monad m => o -> Stream o m ()
yield = P.yield

chain :: Monad m => (o -> m ()) -> Stream o m r -> Stream o m r
chain action producer = producer >-> PP.chain action

effects :: Monad m => Stream o m r -> m r
effects producer = P.runEffect $ producer >-> PP.drain   

toList :: Monad m => Stream a m r -> m ([a],r)
toList = PP.toListM'

toList_ :: Monad m => Stream a m () -> m [a]
toList_ = PP.toListM

