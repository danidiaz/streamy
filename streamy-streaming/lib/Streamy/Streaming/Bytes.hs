{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Streamy.Streaming.Bytes (
          SingleByte
        , Bytes
        , ByteStream
        , ByteIndex
        , Streamy.Streaming.Bytes.empty 
        , Streamy.Streaming.Bytes.singleton 
        , Streamy.Streaming.Bytes.pack 
        , Streamy.Streaming.Bytes.unpack 
        , Streamy.Streaming.Bytes.fromChunks 
        , Streamy.Streaming.Bytes.toChunks 
        , Streamy.Streaming.Bytes.fromStrict 
        , Streamy.Streaming.Bytes.toStrict 
        , Streamy.Streaming.Bytes.toStrict_ 
        , Streamy.Streaming.Bytes.splitAt 
        , Streamy.Streaming.Bytes.take 
        , Streamy.Streaming.Bytes.fromHandle 
        , Streamy.Streaming.Bytes.toHandle 
    ) where

import Streamy.Streaming (Stream,WrappedStream(..))

import Data.Word
import Data.Int
import qualified Data.ByteString as B
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Morph
import System.IO (Handle)

import Streaming (Of(..))
import qualified Streaming as Q
import qualified Streaming.Prelude as Q
import qualified Data.ByteString.Streaming as QB

type SingleByte = Word8

type Bytes = B.ByteString

type ByteStream = QB.ByteString

type ByteIndex = Int64

empty :: ByteStream m ()
empty = QB.empty

singleton :: Monad m => SingleByte -> ByteStream m ()
singleton = QB.singleton

pack :: Monad m => Stream SingleByte m r -> ByteStream m r
pack (Stream s) = QB.pack s

unpack :: Monad m => ByteStream m r -> Stream SingleByte m r
unpack = Stream . QB.unpack

fromChunks :: Monad m => Stream Bytes m r -> ByteStream m r
fromChunks (Stream s) = QB.fromChunks s

toChunks :: Monad m => ByteStream m r -> Stream Bytes m r
toChunks bs = Stream (QB.toChunks bs)

fromStrict :: Bytes -> ByteStream m ()
fromStrict = QB.fromStrict

toStrict :: Monad m => ByteStream m r -> m (Bytes,r)
toStrict bs = Q.lazily <$> QB.toStrict bs

toStrict_ :: Monad m => ByteStream m () -> m Bytes
toStrict_ = QB.toStrict_

splitAt :: Monad m => ByteIndex -> ByteStream m r -> ByteStream m (ByteStream m r)
splitAt = QB.splitAt

take :: Monad m => ByteIndex -> ByteStream m r -> ByteStream m ()
take = QB.take

fromHandle :: MonadIO m => Handle -> ByteStream m ()
fromHandle = QB.fromHandle

toHandle :: MonadIO m => Handle -> ByteStream m r -> m r
toHandle = QB.toHandle

