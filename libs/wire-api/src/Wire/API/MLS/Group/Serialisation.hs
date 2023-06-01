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
  ( convToGroupId,
    convToGroupId',
    groupIdToConv,
    nextGenGroupId,
  )
where

import Data.Bifunctor
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy as L
import Data.Domain
import Data.Id
import Data.Qualified
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.UUID as UUID
import Imports hiding (cs)
import Web.HttpApiData (FromHttpApiData (parseHeader))
import Wire.API.MLS.Group
import Wire.API.MLS.SubConversation

-- | Return the group ID associated to a conversation ID. Note that is not
-- assumed to be stable over time or even consistent among different backends.
convToGroupId :: Qualified ConvOrSubConvId -> GroupIdGen -> GroupId
convToGroupId qcs gen = GroupId . L.toStrict . runPut $ do
  let cs = qUnqualified qcs
      subId = foldMap unSubConvId cs.subconv
  putWord64be 1 -- Version 1 of the GroupId format
  putLazyByteString . UUID.toByteString . toUUID $ cs.conv
  putWord8 $ fromIntegral (T.length subId)
  putByteString $ T.encodeUtf8 subId
  maybe (pure ()) (const $ putWord32be (unGroupIdGen gen)) cs.subconv
  putLazyByteString . toByteString $ qDomain qcs

convToGroupId' :: Qualified ConvOrSubConvId -> GroupId
convToGroupId' = flip convToGroupId (GroupIdGen 0)

groupIdToConv :: GroupId -> Either String (Qualified ConvOrSubConvId, GroupIdGen)
groupIdToConv gid = do
  (rem', _, (conv, gen)) <- first (\(_, _, msg) -> msg) $ runGetOrFail readConv (L.fromStrict (unGroupId gid))
  domain <- first displayException . T.decodeUtf8' . L.toStrict $ rem'
  pure $ (Qualified conv (Domain domain), gen)
  where
    readConv = do
      version <- getWord64be
      unless (version == 1) $ fail "unsupported groupId version"
      mUUID <- UUID.fromByteString . L.fromStrict <$> getByteString 16
      uuid <- maybe (fail "invalid conversation UUID in groupId") pure mUUID
      n <- getWord8
      if n == 0
        then pure $ (Conv (Id uuid), GroupIdGen 0)
        else do
          subConvIdBS <- getByteString $ fromIntegral n
          subConvId <- either (fail . T.unpack) pure $ parseHeader subConvIdBS
          gen <- getWord32be
          pure $ (SubConv (Id uuid) (SubConvId subConvId), GroupIdGen gen)

nextGenGroupId :: GroupId -> Either String GroupId
nextGenGroupId gid =
  uncurry convToGroupId
    . second (GroupIdGen . succ . unGroupIdGen)
    <$> groupIdToConv gid
