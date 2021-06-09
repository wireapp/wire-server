module Wire.API.ServantProto where

import Servant
import Imports
import Network.HTTP.Media ((//))
import Data.List.NonEmpty (NonEmpty(..))

-- | Type to tell servant that it should unrender request body or render
-- response body with Protobuf
data Proto

-- | We do not use 'Data.ProtocolBuffers.Decode' so we get a little freedom in
-- defining separate data types which match one to one with the protobuf and the
-- data types which we actually use in business logic. Eventually we should
-- think of better ways of doing this, perhaps using mu-schema or proto-lens as
-- it is fairly difficult to keep our custom data type, e.g. in
-- Wire.API.Message.Proto in sync with the proto files.
class FromProto a where
  fromProto :: LByteString -> Either String a

instance Accept Proto where
  contentTypes _ = ("application" // "x-protobuf") :| []

instance FromProto a => MimeUnrender Proto a where
  mimeUnrender _ bs = fromProto bs
