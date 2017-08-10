module Streamy.Pipes (Stream,yield) where

import Pipes (Proxy,X)
import qualified Pipes as P

type Stream = Proxy X () ()

yield :: Monad m => o -> Stream o m ()
yield = P.yield


