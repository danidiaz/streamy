# stuff to tackle next

- Functions
    - for :: (Monad m, Functor f) => Stream (Of a) m r -> (a -> Stream f m x) -> Stream f m r
    - sequence :: Monad m => Pipe (m a) a m r
    - iterate :: Monad m => (a -> a) -> a -> Stream (Of a) m r 
    - iterateM :: Monad m => (a -> m a) -> m a -> Stream (Of a) m r
    - repeat :: Monad m => a -> Stream (Of a) m r
    - repeatM :: Monad m => m a -> Stream (Of a) m r
    - replicate :: Monad m => Int -> a -> Stream (Of a) m ()
    - replicateM :: Monad m => Int -> m a -> Stream (Of a) m ()
    - enumFrom :: (Monad m, Enum n) => n -> Stream (Of n) m r
    - enumFromThen :: (Monad m, Enum a) => a -> a -> Stream (Of a) m r
    - mapM_ :: Monad m => (a -> m b) -> Stream (Of a) m r -> m r
    - map :: Monad m => (a -> b) -> Stream (Of a) m r -> Stream (Of b) m r 
    - mapM :: Monad m => (a -> m b) -> Stream (Of a) m r -> Stream (Of b) m r
    - filter :: Monad m => (a -> Bool) -> Stream (Of a) m r -> Stream (Of a) m r
    - filterM :: Monad m => (a -> m Bool) -> Stream (Of a) m r -> Stream (Of a) m r
    - take :: (Monad m, Functor f) => Int -> Stream f m r -> Stream f m () 
    - takeWhile :: Monad m => (a -> Bool) -> Stream (Of a) m r -> Stream (Of a) m ()
    - takeWhileM :: Monad m => (a -> m Bool) -> Stream (Of a) m r -> Stream (Of a) m ()
    - drop :: Monad m => Int -> Stream (Of a) m r -> Stream (Of a) m r
    - dropWhile :: Monad m => (a -> Bool) -> Stream (Of a) m r -> Stream (Of a) m r
    - all_
    - any_

- Splitting and grouping functions (Streaming + Pipes only)
    - splitAt :: (Monad m, Functor f) => Int -> Stream f m r -> Stream f m (Stream f m r)
    - split :: (Eq a, Monad m) => a -> Stream (Of a) m r -> Stream (Stream (Of a) m) m r
    - breaks :: Monad m => (a -> Bool) -> Stream (Of a) m r -> Stream (Stream (Of a) m) m r
    - break :: Monad m => (a -> Bool) -> Stream (Of a) m r -> Stream (Of a) m (Stream (Of a) m r)
    - breakWhen :: Monad m => (x -> a -> x) -> x -> (x -> b) -> (b -> Bool) -> Stream (Of a) m r -> Stream (Of a) m (Stream (Of a) m r)
    - group :: (Monad m, Eq a) => Stream (Of a) m r -> Stream (Stream (Of a) m) m r
    - groupBy :: Monad m => (a -> a -> Bool) -> Stream (Of a) m r -> Stream (Stream (Of a) m) m r

- Applicative Sinks

- Grouping operations (one level tops)

- Streamy.Bytestring
    - fromHandle, toHandle
    - fromChunks, toChunks
    - grouped bytes
    
