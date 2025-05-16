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

module Wire.API.MLS.Group.Serialisation
  ( GroupIdVersion (..),
    GroupIdParts (..),
    groupIdParts,
    convToGroupId,
    groupIdToConv,
    newGroupId,
    nextGenGroupId,
  )
where

import Data.Bifunctor
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Conversion
import Data.ByteString.Lazy qualified as L
import Data.Domain
import Data.Id
import Data.Qualified
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.UUID qualified as UUID
import Imports
import Web.HttpApiData (FromHttpApiData (parseHeader))
import Wire.API.Conversation
import Wire.API.MLS.Group
import Wire.API.MLS.SubConversation
import Wire.Arbitrary

data GroupIdVersion = GroupIdVersion1 | GroupIdVersion2
  deriving (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via GenericUniform GroupIdVersion

groupIdVersionNumber :: GroupIdVersion -> Word16
groupIdVersionNumber GroupIdVersion1 = 1
groupIdVersionNumber GroupIdVersion2 = 2

getVersion :: Get GroupIdVersion
getVersion = do
  n <- getWord16be
  case n of
    1 -> pure GroupIdVersion1
    2 -> pure GroupIdVersion2
    _ -> fail ("unsupported GroupId version " <> show n)

data GroupIdParts = GroupIdParts
  { convType :: ConvType,
    qConvId :: Qualified ConvOrSubConvId,
    gidGen :: GroupIdGen
  }
  deriving (Show, Eq)

groupIdParts :: ConvType -> Word32 -> Qualified ConvOrSubConvId -> GroupIdParts
groupIdParts ct gen qcs =
  GroupIdParts
    { convType = ct,
      qConvId = qcs,
      gidGen = GroupIdGen gen
    }

-- | Return the group ID associated to a conversation ID. Note that is not
-- assumed to be stable over time or even consistent among different backends.
convToGroupId :: GroupIdVersion -> GroupIdParts -> GroupId
convToGroupId v parts = GroupId . L.toStrict . runPut $ do
  let cs = qUnqualified parts.qConvId
      subId = foldMap unSubConvId cs.subconv
  putWord16be (groupIdVersionNumber v)
  putWord16be (fromIntegral $ fromEnum parts.convType)
  putLazyByteString . UUID.toByteString . toUUID $ cs.conv
  putWord8 $ fromIntegral (T.length subId)
  putByteString $ T.encodeUtf8 subId
  when (v > GroupIdVersion1 || isJust cs.subconv) $
    putWord32be (unGroupIdGen parts.gidGen)
  putLazyByteString . toByteString $ qDomain parts.qConvId

groupIdToConv :: GroupId -> Either String (GroupIdVersion, GroupIdParts)
groupIdToConv gid = do
  (rem', _, (v, ct, conv, gen)) <- first (\(_, _, msg) -> msg) $ runGetOrFail readConv (L.fromStrict (unGroupId gid))
  domain <- first displayException . T.decodeUtf8' . L.toStrict $ rem'
  pure
    ( v,
      GroupIdParts
        { convType = toEnum $ fromIntegral ct,
          qConvId = Qualified conv (Domain domain),
          gidGen = gen
        }
    )
  where
    readConv = do
      version <- getVersion
      ct <- getWord16be
      mUUID <- UUID.fromByteString . L.fromStrict <$> getByteString 16
      uuid <- maybe (fail "invalid conversation UUID in groupId") pure mUUID
      n <- getWord8
      if n == 0
        then do
          gen <- if version == GroupIdVersion1 then pure 0 else getWord32be
          pure $ (version, ct, Conv (Id uuid), GroupIdGen gen)
        else do
          subConvIdBS <- getByteString $ fromIntegral n
          subConvId <- either (fail . T.unpack) pure $ parseHeader subConvIdBS
          gen <- getWord32be
          pure $ (version, ct, SubConv (Id uuid) (SubConvId subConvId), GroupIdGen gen)

newGroupId :: ConvType -> Qualified ConvOrSubConvId -> GroupId
newGroupId ctype qcs = convToGroupId GroupIdVersion1 (groupIdParts ctype 0 qcs)

nextGenGroupId :: GroupId -> Either String GroupId
nextGenGroupId gid = convToGroupId GroupIdVersion2 . succGen . snd <$> groupIdToConv gid
  where
    succGen parts =
      parts
        { gidGen = GroupIdGen (succ $ unGroupIdGen parts.gidGen)
        }
