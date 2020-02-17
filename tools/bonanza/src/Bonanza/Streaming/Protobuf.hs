module Bonanza.Streaming.Protobuf
  ( Decoder,
    Decoded (..),
    decoder,
    decode,
  )
where

import Bonanza.Types (ToLogEvent (..))
import Control.Monad.Catch (MonadThrow)
import qualified Data.ByteString as BS
import Data.Conduit (ConduitT)
import Data.Conduit.Cereal (conduitGet2)
import Data.ProtocolBuffers (Decode, decodeMessage)
import Data.ProtocolBuffers.Internal
import Data.Serialize.Get
import Imports

newtype Decoder a = Decoder (Get (Decoded a))

decoder :: Decode a => Decoder a
decoder = Decoder decodeLengthPrefixedMessage

decode :: MonadThrow m => Decoder a -> ConduitT ByteString (Decoded a) m ()
decode (Decoder d) = conduitGet2 d

data Decoded a
  = Decoded !a
  | Truncated !a

instance ToLogEvent a => ToLogEvent (Decoded a) where
  toLogEvent (Decoded a) = toLogEvent a
  toLogEvent (Truncated a) = toLogEvent a

decodeLengthPrefixedMessage :: Decode a => Get (Decoded a)
decodeLengthPrefixedMessage = do
  len <- getVarInt
  bs <- getBytes $ fromIntegral (len :: Int64)
  case runGetState decodeMessage bs 0 of
    Right (a, bs')
      | BS.null bs' -> return (Decoded a)
      | otherwise -> return (Truncated a)
    Left e -> fail e
{-# INLINE decodeLengthPrefixedMessage #-}
