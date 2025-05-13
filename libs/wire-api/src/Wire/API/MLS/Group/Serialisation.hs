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
  ( GroupIdParts (..),
    groupIdParts,
    convToGroupId,
    groupIdToConv,
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

data GroupIdParts = GroupIdParts
  { convType :: ConvType,
    qConvId :: Qualified ConvOrSubConvId,
    gidGen :: GroupIdGen
  }
  deriving (Show, Eq)

groupIdParts :: ConvType -> Qualified ConvOrSubConvId -> GroupIdParts
groupIdParts ct qcs =
  GroupIdParts
    { convType = ct,
      qConvId = qcs,
      gidGen = GroupIdGen 0
    }

-- | Return the group ID associated to a conversation ID. Note that is not
-- assumed to be stable over time or even consistent among different backends.
convToGroupId :: GroupIdParts -> GroupId
convToGroupId parts = GroupId . L.toStrict . runPut $ do
  let cs = qUnqualified parts.qConvId
      subId = foldMap unSubConvId cs.subconv
  putWord16be 1 -- Version 1 of the GroupId format
  putWord16be (fromIntegral $ fromEnum parts.convType)
  putLazyByteString . UUID.toByteString . toUUID $ cs.conv
  putWord8 $ fromIntegral (T.length subId)
  putByteString $ T.encodeUtf8 subId
  maybe (pure ()) (const $ putWord32be (unGroupIdGen parts.gidGen)) cs.subconv
  putLazyByteString . toByteString $ qDomain parts.qConvId

groupIdToConv :: GroupId -> Either String GroupIdParts
groupIdToConv gid = do
  (rem', _, (ct, conv, gen)) <- first (\(_, _, msg) -> msg) $ runGetOrFail readConv (L.fromStrict (unGroupId gid))
  domain <- first displayException . T.decodeUtf8' . L.toStrict $ rem'
  pure
    GroupIdParts
      { convType = toEnum $ fromIntegral ct,
        qConvId = Qualified conv (Domain domain),
        gidGen = gen
      }
  where
    readConv = do
      version <- getWord16be
      ct <- getWord16be
      unless (version == 1) $ fail "unsupported groupId version"
      mUUID <- UUID.fromByteString . L.fromStrict <$> getByteString 16
      uuid <- maybe (fail "invalid conversation UUID in groupId") pure mUUID
      n <- getWord8
      if n == 0
        then pure $ (ct, Conv (Id uuid), GroupIdGen 0)
        else do
          subConvIdBS <- getByteString $ fromIntegral n
          subConvId <- either (fail . T.unpack) pure $ parseHeader subConvIdBS
          gen <- getWord32be
          pure $ (ct, SubConv (Id uuid) (SubConvId subConvId), GroupIdGen gen)

nextGenGroupId :: GroupId -> Either String GroupId
nextGenGroupId gid = convToGroupId . succGen <$> groupIdToConv gid
  where
    succGen parts =
      parts
        { gidGen = GroupIdGen (succ $ unGroupIdGen parts.gidGen)
        }
