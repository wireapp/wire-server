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

import Control.Monad.Trans.Maybe
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Conversion
import Data.ByteString.Lazy qualified as L
import Data.Default
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

getVersion :: Get (Maybe GroupIdVersion)
getVersion = do
  n <- getWord16be
  case n of
    1 -> pure (Just GroupIdVersion1)
    2 -> pure (Just GroupIdVersion2)
    _ -> pure Nothing

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

  let domain = toByteString (qDomain parts.qConvId)
  when (v > GroupIdVersion1) $
    putWord16be (fromIntegral (L.length domain))
  putLazyByteString domain

groupIdToConv :: GroupId -> Either String (Maybe GroupIdVersion, GroupIdParts)
groupIdToConv gid = case runGetOrFail getParts ((L.fromStrict (unGroupId gid))) of
  Left (_, _, e) -> Left e
  Right (_, _, x) -> pure x

getParts :: Get (Maybe GroupIdVersion, GroupIdParts)
getParts = do
  mv <- getVersion
  convType <- getConvType
  mUUID <- UUID.fromByteString . L.fromStrict <$> getByteString 16
  convId <- maybe (fail "invalid conversation UUID in GroupId") (pure . Id) mUUID
  convOrSub <- maybe (Conv convId) (SubConv convId) <$> getSubConvId
  parts <- case mv of
    -- version 1 has a fixed format, with the domain as a trailing string
    Just GroupIdVersion1 -> do
      gidGen <- case convOrSub of
        Conv _ -> pure def
        SubConv _ _ -> getGroupIdGen
      eDomain <-
        T.decodeUtf8' . L.toStrict
          <$> getRemainingLazyByteString
      domain <- either (fail . displayException) pure eDomain
      pure
        GroupIdParts
          { convType,
            qConvId = Qualified convOrSub (Domain domain),
            gidGen
          }
    -- Version 2 and above are extensible. Parse the known fields, and ignore any extensions.
    _ -> do
      gidGen <- getGroupIdGen
      domain <- getDomain
      pure GroupIdParts {convType, qConvId = Qualified convOrSub domain, gidGen}
  pure (mv, parts)

getConvType :: Get ConvType
getConvType = toEnum . fromIntegral <$> getWord16be

getSubConvId :: Get (Maybe SubConvId)
getSubConvId = runMaybeT $ do
  n <- lift getWord8
  guard $ n > 0
  bs <- lift $ getByteString (fromIntegral n)
  either (fail . T.unpack) (pure . SubConvId) $ parseHeader bs

getGroupIdGen :: Get GroupIdGen
getGroupIdGen = GroupIdGen <$> getWord32be

getDomain :: Get Domain
getDomain = do
  len <- fromIntegral <$> getWord16be
  domain <- T.decodeUtf8' <$> getByteString len
  case domain of
    Left e -> fail (displayException e)
    Right d -> pure (Domain d)

newGroupId :: ConvType -> Qualified ConvOrSubConvId -> GroupId
newGroupId ctype qcs = convToGroupId GroupIdVersion1 (groupIdParts ctype 0 qcs)

nextGenGroupId :: GroupId -> Either String GroupId
nextGenGroupId gid = convToGroupId GroupIdVersion2 . succGen . snd <$> groupIdToConv gid
  where
    succGen parts =
      parts
        { gidGen = GroupIdGen (succ $ unGroupIdGen parts.gidGen)
        }
