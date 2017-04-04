module Data.Journal
    ( encodeOne
    , decodeOne
    )
where

import qualified Data.ByteString.Lazy as Lazy
import           Data.ProtocolBuffers
import           Data.Serialize


encodeOne :: Encode a => a -> Lazy.ByteString
encodeOne = runPutLazy . encodeMessage
{-# INLINE encodeOne #-}

decodeOne :: Decode a => Lazy.ByteString -> Either String a
decodeOne = runGetLazy decodeMessage
{-# INLINE decodeOne #-}
