-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

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
