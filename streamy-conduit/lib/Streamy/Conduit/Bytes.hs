{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Streamy.Conduit.Bytes (
          SingleByte
        , Bytes
        , ByteStream
        , ByteIndex
        , Streamy.Conduit.Bytes.empty 
        , Streamy.Conduit.Bytes.singleton 
        , Streamy.Conduit.Bytes.pack 
        , Streamy.Conduit.Bytes.unpack 
        , Streamy.Conduit.Bytes.fromChunks 
        , Streamy.Conduit.Bytes.toChunks 
        , Streamy.Conduit.Bytes.fromStrict 
        , Streamy.Conduit.Bytes.toStrict 
        , Streamy.Conduit.Bytes.toStrict_ 
--        , Streamy.Conduit.Bytes.splitAt 
        , Streamy.Conduit.Bytes.take 
        , Streamy.Conduit.Bytes.fromHandle 
        , Streamy.Conduit.Bytes.toHandle 
    ) where

import qualified Streamy.Conduit
import Streamy.Conduit (Stream)

import qualified Conduit as C
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL

import Data.Word
import Data.Int
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import Data.Foldable (Foldable)
import qualified Data.Foldable 
import Data.Functor (void)
import Data.Tuple (swap)
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import System.IO (Handle)

type SingleByte = Word8

type Bytes = B.ByteString

type ByteStream = C.ConduitM () Bytes

type ByteIndex = Int

empty :: Monad m => ByteStream m ()
empty = return ()

singleton :: Monad m => SingleByte -> ByteStream m ()
singleton = fromStrict . B.singleton

pack :: Monad m => Stream SingleByte m r -> ByteStream m r
pack = Streamy.Conduit.map B.singleton

unpack :: Monad m => ByteStream m r -> Stream SingleByte m r
unpack c = C.fuseUpstream c CC.concat

fromChunks :: Monad m => Stream Bytes m r -> ByteStream m r
fromChunks = id

toChunks :: Monad m => ByteStream m r -> Stream Bytes m r
toChunks = id

fromStrict :: Monad m => Bytes -> ByteStream m ()
fromStrict = Streamy.Conduit.yield

toStrict :: Monad m => ByteStream m r -> m (Bytes,r)
toStrict bs = fmap (\(r,lbs) -> (BL.toStrict lbs,r))  
                   (C.runConduit (C.fuseBoth bs CC.sinkLazy))

toStrict_ :: Monad m => ByteStream m () -> m Bytes
toStrict_ = fmap (\(bytes,_) -> bytes) . toStrict

-- splitAt :: Monad m => ByteIndex -> ByteStream m r -> ByteStream m (ByteStream m r)

take :: Monad m => ByteIndex -> ByteStream m r -> ByteStream m ()
take i c = C.fuse (void c) (CC.takeE i)

fromHandle :: MonadIO m => Handle -> ByteStream m ()
fromHandle = CC.sourceHandle

toHandle :: MonadIO m => Handle -> ByteStream m r -> m r
toHandle h c = C.runConduit $ C.fuseUpstream c (CC.sinkHandle h)

