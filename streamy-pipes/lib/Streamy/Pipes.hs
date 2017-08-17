module Streamy.Pipes (
          Stream
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
    ) where

import qualified Data.List
import Pipes (Proxy,X,(>->))
import qualified Pipes as P
import qualified Pipes.Prelude as PP

type Stream = Proxy X () ()

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


