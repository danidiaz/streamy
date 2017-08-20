{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Streamy.Pipes.Bytes (
          SingleByte
        , Bytes
        , ByteStream
        , ByteIndex
        , Streamy.Pipes.Bytes.empty 
        , Streamy.Pipes.Bytes.singleton 
        , Streamy.Pipes.Bytes.pack 
        , Streamy.Pipes.Bytes.unpack 
        , Streamy.Pipes.Bytes.fromChunks 
        , Streamy.Pipes.Bytes.toChunks 
        , Streamy.Pipes.Bytes.fromStrict 
        , Streamy.Pipes.Bytes.toStrict 
        , Streamy.Pipes.Bytes.toStrict_ 
        , Streamy.Pipes.Bytes.splitAt 
        , Streamy.Pipes.Bytes.take 
        , Streamy.Pipes.Bytes.fromHandle 
        , Streamy.Pipes.Bytes.toHandle 
    ) where

import qualified Streamy.Pipes
import Streamy.Pipes (Stream)

import Data.Word
import Data.Int
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB

import qualified Data.List
import Pipes (Proxy,X,(>->))
import qualified Pipes as P
import qualified Pipes.Prelude as PP
import qualified Pipes.Group as PG
import qualified Pipes.ByteString as PB
import qualified Pipes.Parse

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import System.IO (Handle)

import Lens.Micro.Extras (view)

type SingleByte = Word8

type Bytes = B.ByteString

type ByteStream = Proxy X () () Bytes

type ByteIndex = Int64

empty :: Monad m => ByteStream m ()
empty = return ()

singleton :: Monad m => SingleByte -> ByteStream m ()
singleton = fromStrict . B.singleton

pack :: Monad m => Stream SingleByte m r -> ByteStream m r
pack = view PB.pack

unpack :: Monad m => ByteStream m r -> Stream SingleByte m r
unpack = view PB.unpack

fromChunks :: Monad m => Stream Bytes m r -> ByteStream m r
fromChunks = id

toChunks :: Monad m => ByteStream m r -> Stream Bytes m r
toChunks = id

fromStrict :: Monad m => Bytes -> ByteStream m ()
fromStrict = Streamy.Pipes.yield

toStrict :: Monad m => ByteStream m r -> m (Bytes,r)
toStrict bs = do
    (chunks,r) <- Streamy.Pipes.toList bs
    return (BL.toStrict . BB.toLazyByteString . foldMap BB.byteString $ chunks,r)

toStrict_ :: Monad m => ByteStream m () -> m Bytes
toStrict_ = fmap (\(bytes,_) -> bytes) . toStrict

splitAt :: Monad m => ByteIndex -> ByteStream m r -> ByteStream m (ByteStream m r)
splitAt i producer = view (PB.splitAt i) producer 

take :: Monad m => ByteIndex -> ByteStream m r -> ByteStream m ()
take i producer = void producer >-> PB.take i

fromHandle :: MonadIO m => Handle -> ByteStream m ()
fromHandle = PB.fromHandle

toHandle :: MonadIO m => Handle -> ByteStream m r -> m r
toHandle h producer = P.runEffect $ producer >-> PB.toHandle h

