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

module Galley.Data
  ( ResultSet,
    ResultSetType (..),
    PageWithState (..),
    mkResultSet,
    resultSetType,
    resultSetResult,
    schemaVersion,

    -- * Teams
    withTeamMembersWithChunks,
    teamIdsForPagination,
    teamIdsFrom,

    -- * Conversation Codes
    lookupCode,
    deleteCode,
    insertCode,

    -- * Clients
    eraseClients,
    lookupClients,
    lookupClients',
    updateClient,

    -- * Utilities
    localOne2OneConvId,

    -- * Defaults
    defRole,
    defRegularConvAccess,
  )
where

import Brig.Types.Code
import Cassandra
import Control.Arrow (second)
import Control.Exception (ErrorCall (ErrorCall))
import Control.Lens hiding ((<|))
import Control.Monad.Catch (throwM)
import Control.Monad.Extra (ifM)
import Data.Id as Id
import Data.Json.Util (UTCTimeMillis (..))
import Data.LegalHold (UserLegalHoldStatus (..), defUserLegalHoldStatus)
import Data.List.Split (chunksOf)
import Data.Range
import Galley.App
import Galley.Data.Access
import Galley.Data.Conversation
import Galley.Data.Instances ()
import Galley.Data.LegalHold (isTeamLegalholdWhitelisted)
import qualified Galley.Data.Queries as Cql
import Galley.Data.ResultSet
import Galley.Data.Types as Data
import Galley.Types.Clients (Clients)
import qualified Galley.Types.Clients as Clients
import Galley.Types.Teams hiding
  ( Event,
    EventType (..),
    self,
    teamConversations,
    teamMembers,
  )
import Imports hiding (Set, max)
import qualified UnliftIO
import Wire.API.Team.Member

schemaVersion :: Int32
schemaVersion = 54

-- | Insert a conversation code
insertCode :: Code -> Galley r ()
insertCode c = do
  let k = codeKey c
  let v = codeValue c
  let cnv = codeConversation c
  let t = round (codeTTL c)
  let s = codeScope c
  retry x5 (write Cql.insertCode (params Quorum (k, v, cnv, s, t)))

-- | Lookup a conversation by code.
lookupCode :: Key -> Scope -> Galley r (Maybe Code)
lookupCode k s = fmap (toCode k s) <$> retry x1 (query1 Cql.lookupCode (params Quorum (k, s)))

-- | Delete a code associated with the given conversation key
deleteCode :: Key -> Scope -> Galley r ()
deleteCode k s = retry x5 $ write Cql.deleteCode (params Quorum (k, s))

-- Teams --------------------------------------------------------------------

teamIdsFrom :: UserId -> Maybe TeamId -> Range 1 100 Int32 -> Galley r (ResultSet TeamId)
teamIdsFrom usr range (fromRange -> max) =
  mkResultSet . fmap runIdentity . strip <$> case range of
    Just c -> paginate Cql.selectUserTeamsFrom (paramsP Quorum (usr, c) (max + 1))
    Nothing -> paginate Cql.selectUserTeams (paramsP Quorum (Identity usr) (max + 1))
  where
    strip p = p {result = take (fromIntegral max) (result p)}

teamIdsForPagination :: UserId -> Maybe TeamId -> Range 1 100 Int32 -> Galley r (Page TeamId)
teamIdsForPagination usr range (fromRange -> max) =
  fmap runIdentity <$> case range of
    Just c -> paginate Cql.selectUserTeamsFrom (paramsP Quorum (usr, c) max)
    Nothing -> paginate Cql.selectUserTeams (paramsP Quorum (Identity usr) max)

-- This function has a bit of a difficult type to work with because we don't have a pure function of type
-- (UserId, Permissions, Maybe UserId, Maybe UTCTimeMillis, Maybe UserLegalHoldStatus) -> TeamMember so we
-- cannot fmap over the ResultSet. We don't want to mess around with the Result size nextPage either otherwise
teamMembersForPagination :: TeamId -> Maybe UserId -> Range 1 HardTruncationLimit Int32 -> Galley r (Page (UserId, Permissions, Maybe UserId, Maybe UTCTimeMillis, Maybe UserLegalHoldStatus))
teamMembersForPagination tid start (fromRange -> max) =
  case start of
    Just u -> paginate Cql.selectTeamMembersFrom (paramsP Quorum (tid, u) max)
    Nothing -> paginate Cql.selectTeamMembers (paramsP Quorum (Identity tid) max)

-- Clients ------------------------------------------------------------------

updateClient :: Bool -> UserId -> ClientId -> Galley r ()
updateClient add usr cls = do
  let q = if add then Cql.addMemberClient else Cql.rmMemberClient
  retry x5 $ write (q cls) (params Quorum (Identity usr))

-- Do, at most, 16 parallel lookups of up to 128 users each
lookupClients :: [UserId] -> Galley r Clients
lookupClients = liftClient . lookupClients'

-- This is only used by tests
lookupClients' :: [UserId] -> Client Clients
lookupClients' users =
  Clients.fromList . concat . concat
    <$> forM (chunksOf 2048 users) (UnliftIO.mapConcurrently getClients . chunksOf 128)
  where
    getClients us =
      map (second fromSet)
        <$> retry x1 (query Cql.selectClients (params Quorum (Identity us)))

eraseClients :: UserId -> Galley r ()
eraseClients user = retry x5 (write Cql.rmClients (params Quorum (Identity user)))

-- Internal utilities

-- | Construct 'TeamMember' from database tuple.
-- If FeatureLegalHoldWhitelistTeamsAndImplicitConsent is enabled set UserLegalHoldDisabled
-- if team is whitelisted.
--
-- Throw an exception if one of invitation timestamp and inviter is 'Nothing' and the
-- other is 'Just', which can only be caused by inconsistent database content.
newTeamMember' :: TeamId -> (UserId, Permissions, Maybe UserId, Maybe UTCTimeMillis, Maybe UserLegalHoldStatus) -> Galley r TeamMember
newTeamMember' tid (uid, perms, minvu, minvt, fromMaybe defUserLegalHoldStatus -> lhStatus) = do
  mk minvu minvt >>= maybeGrant
  where
    maybeGrant :: TeamMember -> Galley r TeamMember
    maybeGrant m =
      ifM
        (isTeamLegalholdWhitelisted tid)
        (pure (grantImplicitConsent m))
        (pure m)

    grantImplicitConsent :: TeamMember -> TeamMember
    grantImplicitConsent =
      legalHoldStatus %~ \case
        UserLegalHoldNoConsent -> UserLegalHoldDisabled
        -- the other cases don't change; we just enumerate them to catch future changes in
        -- 'UserLegalHoldStatus' better.
        UserLegalHoldDisabled -> UserLegalHoldDisabled
        UserLegalHoldPending -> UserLegalHoldPending
        UserLegalHoldEnabled -> UserLegalHoldEnabled

    mk (Just invu) (Just invt) = pure $ TeamMember uid perms (Just (invu, invt)) lhStatus
    mk Nothing Nothing = pure $ TeamMember uid perms Nothing lhStatus
    mk _ _ = throwM $ ErrorCall "TeamMember with incomplete metadata."

-- | Invoke the given action with a list of TeamMemberRows IDs
-- which are looked up based on:
withTeamMembersWithChunks ::
  TeamId ->
  ([TeamMember] -> Galley r ()) ->
  Galley r ()
withTeamMembersWithChunks tid action = do
  mems <- teamMembersForPagination tid Nothing (unsafeRange hardTruncationLimit)
  handleMembers mems
  where
    handleMembers mems = do
      tMembers <- mapM (newTeamMember' tid) (result mems)
      action tMembers
      when (hasMore mems) $
        handleMembers =<< liftClient (nextPage mems)
{-# INLINE withTeamMembersWithChunks #-}
