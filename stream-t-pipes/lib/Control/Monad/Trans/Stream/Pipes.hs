module Control.Monad.Trans.Stream.Pipes () where

import Pipes (Producer)
import qualified Pipes as P

type Stream o m r = Producer o m r

yield :: Monad m => o -> Stream o m ()
yield = P.yield


