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
import qualified Data.ByteString as BS
import Data.Domain (domainText)
import qualified Data.Id as Id
import Data.Id (Id (Id, toUUID), OpaqueConvId, OpaqueUserId)
import Data.IdMapping (IdMapping (IdMapping), MappedOrLocalId (Local, Mapped))
import Data.Qualified (Qualified (Qualified, _qDomain, _qLocalPart))
import qualified Data.Text.Encoding as Text.E
import qualified Data.UUID.V5 as UUID.V5
import Galley.API.Error (federationNotImplemented')
import Galley.API.Util (JSON, isFederationEnabled)
import Galley.App (Galley, fromJsonBody)
import qualified Galley.Data.IdMapping as Data (getIdMapping, insertIdMapping)
import Galley.Types.IdMapping (PostIdMappingRequest (reqQualifiedId))
import Imports
import Network.Wai (Response)
import Network.Wai.Predicate ((:::) ((:::)))
import Network.Wai.Utilities (JsonRequest, empty, json)

-- | For debugging and tests.
-- We just pick @()@ here, as conversation and user ID mappings share a table.
getIdMappingH :: Id (Id.Opaque ()) ::: JSON -> Galley Response
getIdMappingH (opaqueId ::: _) =
  json <$> resolveOpaqueId opaqueId

-- | Used by Brig and Galley to notify each other when they find a new 'IdMapping'.
postIdMappingH ::
  JsonRequest PostIdMappingRequest ::: JSON ->
  Galley Response
postIdMappingH (req ::: _) = do
  qualifiedId <- reqQualifiedId <$> fromJsonBody req
  const empty <$> createIdMapping qualifiedId

--------------------------------------------------------------------------------
-- helpers

-- FUTUREWORK(federation, #1178): implement function to resolve IDs in batch

resolveOpaqueUserId :: OpaqueUserId -> Galley (MappedOrLocalId Id.U)
resolveOpaqueUserId = resolveOpaqueId

resolveOpaqueConvId :: OpaqueConvId -> Galley (MappedOrLocalId Id.C)
resolveOpaqueConvId = resolveOpaqueId

-- | It doesn't really matter which type of 'IdMapping' we create, as they will all be
-- identical and can therefore be written to the same table.
--
-- We still don't want to expose this function directly and instead use specialized versions.
resolveOpaqueId :: forall a. Id (Id.Opaque a) -> Galley (MappedOrLocalId a)
resolveOpaqueId opaqueId = do
  isFederationEnabled >>= \case
    False ->
      -- don't check the ID mapping, just assume it's local
      pure $ Local assumedLocalId
    True ->
      -- TODO(mheinzel): should we first check if the user/conv exists locally?
      Data.getIdMapping assumedMappedId <&> \case
        Just idMapping -> Mapped idMapping
        Nothing -> Local assumedLocalId
  where
    assumedMappedId = Id (toUUID opaqueId) :: Id (Id.Mapped a)
    assumedLocalId = Id (toUUID opaqueId) :: Id a

createUserIdMapping :: Qualified (Id (Id.Remote Id.U)) -> Galley (IdMapping Id.U)
createUserIdMapping = createIdMapping

createConvIdMapping :: Qualified (Id (Id.Remote Id.C)) -> Galley (IdMapping Id.C)
createConvIdMapping = createIdMapping

-- | It doesn't really matter which type of 'IdMapping' we create, as they will all be
-- identical and can therefore be written to the same table.
--
-- We still don't want to expose this function directly and instead use specialized versions.
--
-- TODO(mheinzel): also intra-call Brig if the mapping is new.
createIdMapping :: Typeable a => Qualified (Id (Id.Remote a)) -> Galley (IdMapping a)
createIdMapping qualifiedId = do
  isFederationEnabled >>= \case
    False ->
      -- TODO(mheinzel): different error "federation-not-enabled"?
      throwM . federationNotImplemented' . pure $ (Nothing, qualifiedId)
    True -> do
      let mappedId = hashQualifiedId qualifiedId
      let idMapping = IdMapping mappedId qualifiedId
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
