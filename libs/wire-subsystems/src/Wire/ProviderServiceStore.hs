{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.ProviderServiceStore
  ( -- * Effect
    ProviderServiceStore (..),

    -- * Smart constructors (Polysemy)
    createProviderService,
    lookupProviderService,
    listProviderServices,
    updateProviderService,
    deleteProviderService,
    lookupProviderServiceProfile,
    listProviderServiceProfiles,
    lookupProviderServiceConn,
    updateProviderServiceConn,
    insertProviderServiceIndexes,
    deleteProviderServiceIndexes,
    paginateProviderServiceTags,
    paginateProviderServiceNames,
    insertProviderServiceWhitelist,
    deleteProviderServiceWhitelist,
    paginateProviderServiceWhitelist,
    getProviderServiceWhitelistStatus,

    -- * Type aliases shared by the effect signatures
    RangedServiceTags,
    IndexRow,

    -- * Pure pagination helpers shared by the interpreters

    -- (see Note [buggy pagination] in "Wire.ProviderServiceStore.Cassandra")
    filterResults,
    filterbyProvider,
    filterPrefix,
    nextRow,
    mkPrefixIndex,
    toLowerName,
    trim,
  )
where

import Cassandra (Page (..))
import Data.Id (ProviderId, ServiceId, TeamId)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.Misc (Fingerprint, HttpsUrl, Rsa)
import Data.Range (Range)
import Data.Text qualified as Text
import Imports
import Polysemy
import Wire.API.Provider.Service (Service, ServiceConn, ServiceKey, ServiceProfile, ServiceProfilePage, ServiceToken)
import Wire.API.Provider.Service.Tag (QueryAnyTags, ServiceTag)
import Wire.API.User.Profile (Asset, Name (..), fromName)

-- | Service tags are limited to 3 per service.
type RangedServiceTags = Range 0 3 (Set ServiceTag)

-- | A row of the @service_tag@ / @service_prefix@ index tables.
type IndexRow = (Name, ProviderId, ServiceId)

-- | The provider-platform @service@ tables of the brig keyspace
-- (@service@, @service_tag@, @service_prefix@, @service_whitelist@,
-- @service_whitelist_rev@), previously accessed directly via
-- @Brig.Provider.DB@. One operation per former DB function, signatures
-- unchanged.
data ProviderServiceStore m a where
  CreateProviderService ::
    ProviderId ->
    Name ->
    Text ->
    Text ->
    HttpsUrl ->
    ServiceToken ->
    ServiceKey ->
    Fingerprint Rsa ->
    [Asset] ->
    Set ServiceTag ->
    ProviderServiceStore m ServiceId
  LookupProviderService :: ProviderId -> ServiceId -> ProviderServiceStore m (Maybe Service)
  ListProviderServices :: ProviderId -> ProviderServiceStore m [Service]
  UpdateProviderService ::
    ProviderId ->
    ServiceId ->
    Name ->
    RangedServiceTags ->
    Maybe (Name, Name) ->
    Maybe Text ->
    Maybe Text ->
    Maybe [Asset] ->
    Maybe (RangedServiceTags, RangedServiceTags) ->
    Bool ->
    ProviderServiceStore m ()
  DeleteProviderService :: ProviderId -> ServiceId -> Name -> RangedServiceTags -> ProviderServiceStore m ()
  LookupProviderServiceProfile :: ProviderId -> ServiceId -> ProviderServiceStore m (Maybe ServiceProfile)
  ListProviderServiceProfiles :: ProviderId -> ProviderServiceStore m [ServiceProfile]
  LookupProviderServiceConn :: ProviderId -> ServiceId -> ProviderServiceStore m (Maybe ServiceConn)
  UpdateProviderServiceConn ::
    ProviderId ->
    ServiceId ->
    Maybe HttpsUrl ->
    Maybe (NonEmpty ServiceToken) ->
    Maybe (NonEmpty (ServiceKey, Fingerprint Rsa)) ->
    Maybe Bool ->
    ProviderServiceStore m ()
  InsertProviderServiceIndexes :: ProviderId -> ServiceId -> Name -> RangedServiceTags -> ProviderServiceStore m ()
  DeleteProviderServiceIndexes :: ProviderId -> ServiceId -> Name -> RangedServiceTags -> ProviderServiceStore m ()
  PaginateProviderServiceTags :: QueryAnyTags 1 3 -> Maybe Text -> Int32 -> Maybe ProviderId -> ProviderServiceStore m ServiceProfilePage
  PaginateProviderServiceNames :: Maybe (Range 1 128 Text) -> Int32 -> Maybe ProviderId -> ProviderServiceStore m ServiceProfilePage
  InsertProviderServiceWhitelist :: TeamId -> ProviderId -> ServiceId -> ProviderServiceStore m ()
  DeleteProviderServiceWhitelist :: Maybe TeamId -> ProviderId -> ServiceId -> ProviderServiceStore m ()
  PaginateProviderServiceWhitelist :: TeamId -> Maybe (Range 1 128 Text) -> Bool -> Int32 -> ProviderServiceStore m ServiceProfilePage
  GetProviderServiceWhitelistStatus :: TeamId -> ProviderId -> ServiceId -> ProviderServiceStore m Bool

makeSem ''ProviderServiceStore

--------------------------------------------------------------------------------
-- Pure pagination helpers (moved verbatim from Brig.Provider.DB; see
-- Note [buggy pagination] in Wire.ProviderServiceStore.Cassandra)

filterResults :: Maybe ProviderId -> Text -> Page IndexRow -> Page IndexRow
filterResults providerFilter start = maybe id filterbyProvider providerFilter . filterPrefix start

filterbyProvider :: ProviderId -> Page IndexRow -> Page IndexRow
filterbyProvider pid p =
  let filtered = filter (\(_, provider, _) -> pid == provider) (result p)
      -- check if we have filtered out any result
      allValid = length filtered == length (result p)
      more = allValid && hasMore p
   in p {hasMore = more, result = filtered}

filterPrefix :: Text -> Page IndexRow -> Page IndexRow
filterPrefix prefix p =
  let prefixed = filter (\(Name n, _, _) -> prefix `Text.isPrefixOf` Text.toLower n) (result p)
      -- if they were all valid prefixes, there may be more in the database
      allValid = length prefixed == length (result p)
      more = allValid && hasMore p
   in p {hasMore = more, result = prefixed}

nextRow :: [[IndexRow]] -> Maybe (IndexRow, [[IndexRow]])
nextRow rs = case mapMaybe uncons rs of
  [] -> Nothing
  hs ->
    let next = fst . List.minimumBy (compare `on` fst) $ hs
        cons (r, rs') = if r == next then rs' else r : rs'
        rest = map cons hs
     in Just (next, rest)

mkPrefixIndex :: Name -> Text
mkPrefixIndex = Text.toLower . Text.take 1 . fromName

toLowerName :: Name -> Name
toLowerName = Name . Text.toLower . fromName

trim :: Int32 -> [a] -> [a]
trim = take . fromIntegral
