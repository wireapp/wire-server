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

module Wire.ClientStore where

import Data.Id
import Data.Json.Util (UTCTimeMillis)
import Data.Time
import Imports
import Polysemy
import Wire.API.MLS.CipherSuite
import Wire.API.User.Client
import Wire.API.User.Client.Prekey
import Wire.API.UserMap

data DuplicateMLSPublicKey = DuplicateMLSPublicKey

data ClientStore m a where
  -- Lifecycle
  Upsert :: UserId -> ClientId -> UTCTimeMillis -> NewClient -> ClientStore m (Maybe DuplicateMLSPublicKey)
  Delete :: UserId -> ClientId -> ClientStore m ()
  UpdateLabel :: UserId -> ClientId -> Maybe Text -> ClientStore m ()
  UpdateCapabilities :: UserId -> ClientId -> Maybe ClientCapabilityList -> ClientStore m ()
  UpdateLastActive :: UserId -> ClientId -> UTCTime -> ClientStore m ()
  -- Lookups
  LookupClient :: UserId -> ClientId -> ClientStore m (Maybe Client)
  LookupClients :: UserId -> ClientStore m [Client]
  LookupClientIds :: UserId -> ClientStore m [ClientId]
  LookupClientIdsBulk :: [UserId] -> ClientStore m UserClients
  LookupClientsBulk :: [UserId] -> ClientStore m (UserMap (Set Client))
  LookupPubClientsBulk :: [UserId] -> ClientStore m (UserMap (Set PubClient))
  LookupPrekeyIds :: UserId -> ClientId -> ClientStore m [PrekeyId]
  LookupPrekeyPresenceBulk :: [(UserId, ClientId)] -> ClientStore m (Set (UserId, ClientId))
  GetActivityTimestamps :: UserId -> ClientStore m [Maybe UTCTime]
  -- Proteus
  UpdatePrekeys :: UserId -> ClientId -> [UncheckedPrekeyBundle] -> ClientStore m ()
  ClaimPrekey :: UserId -> ClientId -> ClientStore m (Maybe ClientPrekey)
  -- MLS
  AddMLSPublicKeys :: UserId -> ClientId -> [(SignatureSchemeTag, ByteString)] -> ClientStore m (Maybe DuplicateMLSPublicKey)
  LookupMLSPublicKey :: UserId -> ClientId -> SignatureSchemeTag -> ClientStore m (Maybe LByteString)

makeSem ''ClientStore
