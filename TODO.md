# stuff to tackle next

- Functions
    - drop :: Monad m => Int -> Stream (Of a) m r -> Stream (Of a) m r
    - sequence :: Monad m => Pipe (m a) a m r
    - iterate :: Monad m => (a -> a) -> a -> Stream (Of a) m r 
    - iterateM :: Monad m => (a -> m a) -> m a -> Stream (Of a) m r
    - replicate :: Monad m => Int -> a -> Stream (Of a) m ()
    - replicateM :: Monad m => Int -> m a -> Stream (Of a) m ()
    - enumFrom :: (Monad m, Enum n) => n -> Stream (Of n) m r
    - enumFromThen :: (Monad m, Enum a) => a -> a -> Stream (Of a) m r
    - filter :: Monad m => (a -> Bool) -> Stream (Of a) m r -> Stream (Of a) m r
    - filterM :: Monad m => (a -> m Bool) -> Stream (Of a) m r -> Stream (Of a) m r
    - take :: (Monad m, Functor f) => Int -> Stream f m r -> Stream f m () 
    - takeWhile :: Monad m => (a -> Bool) -> Stream (Of a) m r -> Stream (Of a) m ()
    - takeWhileM :: Monad m => (a -> m Bool) -> Stream (Of a) m r -> Stream (Of a) m ()
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
    
