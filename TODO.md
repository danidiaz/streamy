# stuff to tackle next

- Make the Int parameters abstract.

- Functions
    - iterate :: Monad m => (a -> a) -> a -> Stream (Of a) m r 
    - iterateM :: Monad m => (a -> m a) -> m a -> Stream (Of a) m r
    - enumFrom :: (Monad m, Enum n) => n -> Stream (Of n) m r
    - enumFromThen :: (Monad m, Enum a) => a -> a -> Stream (Of a) m r
    - sequence :: Monad m => Pipe (m a) a m r
    - takeWhileM :: Monad m => (a -> m Bool) -> Stream (Of a) m r -> Stream (Of a) m ()

- Splitting and grouping functions (Streaming + Pipes only)
    - chunksOf :: (Monad m, Functor f) => Int -> Stream f m r -> Stream (Stream f m) m r
    - folds :: Monad m => (x -> a -> x) -> x -> (x -> b) -> FreeT (Producer a m) m r -> Producer b m r
    - foldsM :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> FreeT (Producer a m) m r -> Producer b m r
    - splitAt :: (Monad m, Functor f) => Int -> Stream f m r -> Stream f m (Stream f m r)
    - split :: (Eq a, Monad m) => a -> Stream (Of a) m r -> Stream (Stream (Of a) m) m r
    - breaks :: Monad m => (a -> Bool) -> Stream (Of a) m r -> Stream (Stream (Of a) m) m r
    - break :: Monad m => (a -> Bool) -> Stream (Of a) m r -> Stream (Of a) m (Stream (Of a) m r)
    - breakWhen :: Monad m => (x -> a -> x) -> x -> (x -> b) -> (b -> Bool) -> Stream (Of a) m r -> Stream (Of a) m (Stream (Of a) m r)
    - takes :: (Monad m, Functor f) => Int -> Stream f m r -> Stream f m ()
    - wrap :: (Monad m, Functor f) => f (Stream f m r) -> Stream f m r

- Streamy.Bytestring
    - fromHandle, toHandle
    - fromChunks, toChunks
    - grouped bytes
    
## Does the following make sense?

delimit :: (x -> a -> (x,NonEmpty [b])) 
        -> x 
        -> (x -> NonEmpty [b])
        -> Stream (Of a) m r 
        -> Stream (Stream (Of b) m) m r

delimitM :: (x -> a -> Stream (Of b) m (Stream (Stream (Of b) m) m x))
         -> m x 
         -> (x -> Stream (Of b) (Stream m (Stream (Of b) m) m ()))
         -> Stream (Of a) m r 
         -> Stream (Stream (Of b) m) m r

