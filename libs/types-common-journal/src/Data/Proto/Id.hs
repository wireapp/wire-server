module Data.Proto.Id where

import Data.ByteString
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Id

import qualified Data.UUID as UUID

toByteString :: Id a -> ByteString
toByteString = toStrict . UUID.toByteString . toUUID

fromByteString :: ByteString -> Maybe (Id a)
fromByteString = fmap Id . UUID.fromByteString . fromStrict
