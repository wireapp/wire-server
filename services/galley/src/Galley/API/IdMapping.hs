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

module Galley.API.IdMapping where

import Control.Monad.Catch (throwM)
import Data.Aeson ((.:), FromJSON (parseJSON), withObject)
import qualified Data.ByteString as BS
import Data.Domain (domainText)
import qualified Data.Id as Id
import Data.Id (ConvId, Id (Id, toUUID), MappedConvId, MappedUserId, OpaqueConvId, OpaqueUserId, RemoteUserId, UserId)
import Data.IdMapping (IdMapping (IdMapping), MappedOrLocalId (Local, Mapped))
import Data.Qualified (Qualified (Qualified, _qDomain, _qLocalPart))
import qualified Data.Text.Encoding as Text.E
import qualified Data.UUID.V5 as UUID.V5
import Galley.API.Error (federationNotImplemented')
import Galley.API.Util (JSON, isFederationEnabled)
import Galley.App (Galley, fromJsonBody)
import qualified Galley.Data.IdMapping as Data (getIdMapping, insertIdMapping)
import Imports
import Network.Wai (Response)
import Network.Wai.Predicate ((:::) ((:::)))
import Network.Wai.Utilities (JsonRequest, json)

-- | We just pick 'UserId' here, but the table is the same as for 'ConvId'.
getIdMappingH :: OpaqueUserId ::: JSON -> Galley Response
getIdMappingH (opaqueId ::: _) =
  json <$> resolveOpaqueUserId opaqueId

data PostIdMappingRequest = PostIdMappingRequest
  { reqQualifiedId :: !(Qualified RemoteUserId)
  }
  deriving (Show)

instance FromJSON PostIdMappingRequest where
  parseJSON = withObject "PostIdMappingRequest" $ \o ->
    PostIdMappingRequest <$> o .: "qualified-id"

-- | We just pick 'UserId' here, but the table is the same as for 'ConvId'.
postIdMappingH ::
  JsonRequest PostIdMappingRequest ::: JSON ->
  Galley Response
postIdMappingH (req ::: _) = do
  qualifiedId <- reqQualifiedId <$> fromJsonBody req
  json <$> createUserIdMapping qualifiedId

--------------------------------------------------------------------------------
-- helpers

-- FUTUREWORK(federation, #1178): implement function to resolve IDs in batch

-- | this exists as a shim to find and mark places where we need to handle 'OpaqueUserId's.
resolveOpaqueUserId :: OpaqueUserId -> Galley (MappedOrLocalId Id.U)
resolveOpaqueUserId opaqueUserId = do
  isFederationEnabled >>= \case
    False ->
      -- don't check the ID mapping, just assume it's local
      pure $ Local assumedLocalUserId
    True -> do
      -- TODO(mheinzel): should we first check if the user exists locally?
      Data.getIdMapping assumedMappedUserId <&> \case
        Just idMapping -> Mapped idMapping
        Nothing -> Local assumedLocalUserId
  where
    assumedMappedUserId = Id (toUUID opaqueUserId) :: MappedUserId
    assumedLocalUserId = Id (toUUID opaqueUserId) :: UserId

-- | this exists as a shim to find and mark places where we need to handle 'OpaqueConvId's.
resolveOpaqueConvId :: OpaqueConvId -> Galley (MappedOrLocalId Id.C)
resolveOpaqueConvId opaqueConvId = do
  isFederationEnabled >>= \case
    False ->
      -- don't check the ID mapping, just assume it's local
      pure $ Local assumedLocalConvId
    True ->
      -- TODO(mheinzel): should we first check if the user exists locally?
      Data.getIdMapping assumedMappedConvId <&> \case
        Just idMapping -> Mapped idMapping
        Nothing -> Local assumedLocalConvId
  where
    assumedMappedConvId = Id (toUUID opaqueConvId) :: MappedConvId
    assumedLocalConvId = Id (toUUID opaqueConvId) :: ConvId

createUserIdMapping :: Qualified (Id (Id.Remote Id.U)) -> Galley (IdMapping Id.U)
createUserIdMapping qualifiedUserId = do
  isFederationEnabled >>= \case
    False ->
      -- TODO: different error "federation-not-enabled"?
      throwM . federationNotImplemented' . pure $ (Nothing, qualifiedUserId)
    True -> do
      let mappedId = hashQualifiedId qualifiedUserId
      let idMapping = IdMapping mappedId qualifiedUserId
      -- The mapping is deterministic, so we don't bother reading existing values.
      -- We just need the entry for the reverse direction (resolving mapped ID).
      -- If we overwrite an existing entry, then with the same value as it had before.
      Data.insertIdMapping idMapping
      pure idMapping

createConvIdMapping :: Qualified (Id (Id.Remote Id.C)) -> Galley (IdMapping Id.C)
createConvIdMapping qualifiedConvId = do
  isFederationEnabled >>= \case
    False ->
      -- TODO: different error "federation-not-enabled"?
      throwM . federationNotImplemented' . pure $ (Nothing, qualifiedConvId)
    True -> do
      let mappedId = hashQualifiedId qualifiedConvId
      let idMapping = IdMapping mappedId qualifiedConvId
      -- The mapping is deterministic, so we don't bother reading existing values.
      -- We just need the entry for the reverse direction (resolving mapped ID).
      -- If we overwrite an existing entry, then with the same value as it had before.
      Data.insertIdMapping idMapping
      pure idMapping

-- | Deterministically hashes a qualified ID to a single UUID
--
-- FUTUREWORK: This uses V5 UUID namespaces (SHA-1 under the hood). To provide better
-- protection against collisions, we should use something else, e.g. based on SHA-256.
hashQualifiedId :: Qualified (Id (Id.Remote a)) -> Id (Id.Mapped a)
hashQualifiedId Qualified {_qLocalPart, _qDomain} = Id (UUID.V5.generateNamed namespace object)
  where
    -- using the ID as the namespace sounds backwards, but it works
    namespace = Id.toUUID _qLocalPart
    object = BS.unpack . Text.E.encodeUtf8 . domainText $ _qDomain
