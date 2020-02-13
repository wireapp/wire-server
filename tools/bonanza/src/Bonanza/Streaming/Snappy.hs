{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}

module Bonanza.Streaming.Snappy
  ( Chunk (..),
    ParseError (..),
    SnappyError (..),
    encode,
    decode,
    bytes,
  )
where

import Bonanza.Streaming.Binary (ParseError (..))
import qualified Bonanza.Streaming.Binary as SB
import qualified Codec.Compression.Snappy as Snappy
import Codec.Compression.Snappy.Framing (Chunk (..))
import qualified Codec.Compression.Snappy.Framing as Framing
import Control.Monad.Catch
import Data.Binary (get)
import Data.Bitraversable
import Data.Conduit
import Imports

data SnappyError
  = ChecksumMismatch Chunk
  | UnskippableEncountered Chunk
  deriving (Show, Typeable)

instance Exception SnappyError

encode :: Monad m => ConduitT ByteString Chunk m ()
encode = yield Framing.StreamIdentifier *> awaitForever go
  where
    go = void . bitraverse yield (maybe (pure ()) leftover) . Framing.encode'

decode :: MonadThrow m => ConduitT ByteString Chunk m ()
decode = SB.decode get

bytes :: MonadThrow m => ConduitT Chunk ByteString m ()
bytes = loop
  where
    loop = await >>= maybe (return ()) go
    go c@(Uncompressed chk bs)
      | chk == checksum bs = yield bs *> loop
      | otherwise = throwM $ ChecksumMismatch c
    go c@(Compressed chk (Snappy.decompress -> bs))
      | chk == checksum bs = yield bs *> loop
      | otherwise = throwM $ ChecksumMismatch c
    go c@Unskippable {} = throwM $ UnskippableEncountered c
    go StreamIdentifier = loop
    go Skippable {} = loop
    checksum = Framing.checksum
