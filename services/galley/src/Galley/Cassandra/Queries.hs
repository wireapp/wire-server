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

-- | Tables that are used in this module:
-- - clients
-- - conversation_codes
-- - custom_backend
-- - legalhold_pending_prekeys
-- - legalhold_service
-- - legalhold_whitelisted
-- - team
-- - team_member
-- update using: `rg -i -P '(?:update|from|into)\s+([A-Za-z0-9_]+)' -or '$1' --no-line-number services/galley/src/Galley/Cassandra/Queries.hs | sort | uniq`
module Galley.Cassandra.Queries
  ( selectCustomBackend,
    upsertCustomBackend,
    deleteCustomBackend,
    insertCode,
    lookupCode,
    deleteCode,
    upsertMemberAddClient,
    upsertMemberRmClient,
    selectClients,
    rmClients,
    selectSearchVisibility,
    updateSearchVisibility,
    insertLegalHoldSettings,
    selectLegalHoldSettings,
    removeLegalHoldSettings,
    insertPendingPrekeys,
    dropPendingPrekeys,
    selectPendingPrekeys,
    updateUserLegalHoldStatus,
    insertLegalHoldWhitelistedTeam,
    removeLegalHoldWhitelistedTeam,
  )
where

import Cassandra as C hiding (Value)
import Data.Domain (Domain)
import Data.Id
import Data.LegalHold
import Data.Misc
import Data.Text.Lazy qualified as LT
import Galley.Data.Scope
import Imports
import Text.RawString.QQ
import Wire.API.Conversation.Code
import Wire.API.Password (Password)
import Wire.API.Provider
import Wire.API.Provider.Service
import Wire.API.Team.SearchVisibility
import Wire.API.User.Client.Prekey

-- Conversations accessible by code -----------------------------------------

insertCode :: PrepQuery W (Key, Value, ConvId, Scope, Maybe Password, Int32) ()
insertCode = "INSERT INTO conversation_codes (key, value, conversation, scope, password) VALUES (?, ?, ?, ?, ?) USING TTL ?"

lookupCode :: PrepQuery R (Key, Scope) (Value, Int32, ConvId, Maybe Password)
lookupCode = "SELECT value, ttl(value), conversation, password FROM conversation_codes WHERE key = ? AND scope = ?"

deleteCode :: PrepQuery W (Key, Scope) ()
deleteCode = "DELETE FROM conversation_codes WHERE key = ? AND scope = ?"

-- Clients ------------------------------------------------------------------

selectClients :: PrepQuery R (Identity [UserId]) (UserId, C.Set ClientId)
selectClients = "select user, clients from clients where user in ?"

rmClients :: PrepQuery W (Identity UserId) ()
rmClients = "delete from clients where user = ?"

upsertMemberAddClient :: ClientId -> QueryString W (Identity UserId) ()
upsertMemberAddClient c =
  let t = LT.fromStrict (clientToText c)
   in QueryString $ "update clients set clients = clients + {'" <> t <> "'} where user = ?"

upsertMemberRmClient :: ClientId -> QueryString W (Identity UserId) ()
upsertMemberRmClient c =
  let t = LT.fromStrict (clientToText c)
   in QueryString $ "update clients set clients = clients - {'" <> t <> "'} where user = ?"

-- LegalHold ----------------------------------------------------------------

insertLegalHoldSettings :: PrepQuery W (HttpsUrl, Fingerprint Rsa, ServiceToken, ServiceKey, TeamId) ()
insertLegalHoldSettings =
  [r|
    update legalhold_service
    set base_url    = ?,
        fingerprint = ?,
        auth_token  = ?,
        pubkey      = ?
    where team_id = ?
  |]

selectLegalHoldSettings :: PrepQuery R (Identity TeamId) (HttpsUrl, Fingerprint Rsa, ServiceToken, ServiceKey)
selectLegalHoldSettings =
  [r|
   select base_url, fingerprint, auth_token, pubkey
     from legalhold_service
     where team_id = ?
   |]

removeLegalHoldSettings :: PrepQuery W (Identity TeamId) ()
removeLegalHoldSettings = "delete from legalhold_service where team_id = ?"

insertPendingPrekeys :: PrepQuery W (UserId, PrekeyId, Text) ()
insertPendingPrekeys =
  [r|
        insert into legalhold_pending_prekeys (user, key, data) values (?, ?, ?)
    |]

dropPendingPrekeys :: PrepQuery W (Identity UserId) ()
dropPendingPrekeys =
  [r|
        delete from legalhold_pending_prekeys
          where user = ?
    |]

selectPendingPrekeys :: PrepQuery R (Identity UserId) (PrekeyId, Text)
selectPendingPrekeys =
  [r|
        select key, data
          from legalhold_pending_prekeys
          where user = ?
          order by key asc
    |]

updateUserLegalHoldStatus :: PrepQuery W (UserLegalHoldStatus, TeamId, UserId) ()
updateUserLegalHoldStatus =
  [r|
        update team_member
          set legalhold_status = ?
          where team = ? and user = ?
    |]

insertLegalHoldWhitelistedTeam :: PrepQuery W (Identity TeamId) ()
insertLegalHoldWhitelistedTeam =
  [r|
        insert into legalhold_whitelisted (team) values (?)
    |]

removeLegalHoldWhitelistedTeam :: PrepQuery W (Identity TeamId) ()
removeLegalHoldWhitelistedTeam =
  [r|
        delete from legalhold_whitelisted where team = ?
    |]

-- Search Visibility --------------------------------------------------------

selectSearchVisibility :: PrepQuery R (Identity TeamId) (Identity (Maybe TeamSearchVisibility))
selectSearchVisibility =
  "select search_visibility from team where team = ?"

updateSearchVisibility :: PrepQuery W (TeamSearchVisibility, TeamId) ()
updateSearchVisibility =
  {- `IF EXISTS`, but that requires benchmarking -} "update team set search_visibility = ? where team = ?"

-- Custom Backend -----------------------------------------------------------

selectCustomBackend :: PrepQuery R (Identity Domain) (HttpsUrl, HttpsUrl)
selectCustomBackend =
  "select config_json_url, webapp_welcome_url from custom_backend where domain = ?"

upsertCustomBackend :: PrepQuery W (HttpsUrl, HttpsUrl, Domain) ()
upsertCustomBackend =
  "update custom_backend set config_json_url = ?, webapp_welcome_url = ? where domain = ?"

deleteCustomBackend :: PrepQuery W (Identity Domain) ()
deleteCustomBackend =
  "delete from custom_backend where domain = ?"
