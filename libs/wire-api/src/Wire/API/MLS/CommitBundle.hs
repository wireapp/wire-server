-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.MLS.CommitBundle where

import Control.Lens (view, (.~))
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.ProtoLens (decodeMessage, encodeMessage)
import qualified Data.ProtoLens (Message (defMessage))
import qualified Data.Swagger as S
import qualified Data.Text as T
import Imports
import qualified Proto.Mls
import qualified Proto.Mls_Fields as Proto.Mls
import Wire.API.ConverProtoLens
import qualified Wire.API.ConverProtoLens as CP
import Wire.API.MLS.GroupInfoBundle
import Wire.API.MLS.Message
import Wire.API.MLS.Serialisation
import Wire.API.MLS.Welcome

data CommitBundle = CommitBundle
  { cbCommitMsg :: RawMLS (Message 'MLSPlainText),
    cbWelcome :: Maybe (RawMLS Welcome),
    cbGroupInfoBundle :: GroupInfoBundle
  }
  deriving (Eq, Show)

instance ConvertProtoLens Proto.Mls.CommitBundle CommitBundle where
  fromProtolens protoBundle = CP.label "CommitBundle" $ do
    CommitBundle
      <$> CP.label "commit" (decodeMLS' (view Proto.Mls.commit protoBundle))
      <*> CP.label
        "welcome"
        ( let bs = view Proto.Mls.welcome protoBundle
           in if BS.length bs == 0
                then pure Nothing
                else Just <$> decodeMLS' bs
        )
      <*> CP.label "group_info_bundle" (fromProtolens (view Proto.Mls.groupInfoBundle protoBundle))
  toProtolens bundle =
    let commitData = rmRaw (cbCommitMsg bundle)
        welcomeData = maybe mempty rmRaw (cbWelcome bundle)
        groupInfoData = toProtolens (cbGroupInfoBundle bundle)
     in ( Data.ProtoLens.defMessage
            & Proto.Mls.commit .~ commitData
            & Proto.Mls.welcome .~ welcomeData
            & Proto.Mls.groupInfoBundle .~ groupInfoData
        )

instance ParseMLS CommitBundle where
  parseMLS = CommitBundle <$> parseMLS <*> parseMLSOptional parseMLS <*> parseMLS

instance S.ToSchema CommitBundle where
  declareNamedSchema _ = pure (mlsSwagger "CommitBundle")

-- TODO: remove this
instance SerialiseMLS CommitBundle where
  serialiseMLS (CommitBundle commit welcome gi) = do
    serialiseMLS commit
    serialiseMLSOptional serialiseMLS welcome
    serialiseMLS gi

deserializeCommitBundle :: ByteString -> Either Text CommitBundle
deserializeCommitBundle b = do
  protoCommitBundle :: Proto.Mls.CommitBundle <- first (("Parsing protobuf failed: " <>) . T.pack) (decodeMessage b)
  first ("Converting from protobuf failed: " <>) (fromProtolens protoCommitBundle)

serializeCommitBundle :: CommitBundle -> ByteString
serializeCommitBundle = encodeMessage . (toProtolens @Proto.Mls.CommitBundle @CommitBundle)
