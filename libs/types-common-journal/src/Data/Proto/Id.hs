module Data.Proto.Id where

import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Id
import qualified Data.UUID as UUID
import Imports

toBytes :: Id a -> ByteString
toBytes = toStrict . UUID.toByteString . toUUID

fromBytes :: ByteString -> Maybe (Id a)
fromBytes = fmap Id . UUID.fromByteString . fromStrict
