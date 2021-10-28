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
    addTeamMember,
    updateTeamMember,
    createTeam,
    removeTeamMember,
    listBillingTeamMembers,
    team,
    Galley.Data.teamName,
    teamConversation,
    teamConversations,
    teamIdsFrom,
    teamIdsForPagination,
    teamIdsOf,
    teamMember,
    withTeamMembersWithChunks,
    teamMembersWithLimit,
    teamMembersForFanout,
    teamMembersCollectedWithPagination,
    teamMembersLimited,
    userTeams,
    usersTeams,
    oneUserTeam,
    Galley.Data.teamBinding,
    teamCreationTime,
    deleteTeam,
    removeTeamConv,
    updateTeam,
    updateTeamStatus,

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
import Cassandra.Util
import Control.Arrow (second)
import Control.Exception (ErrorCall (ErrorCall))
import Control.Lens hiding ((<|))
import Control.Monad.Catch (throwM)
import Control.Monad.Extra (ifM)
import Data.ByteString.Conversion hiding (parser)
import Data.Id as Id
import Data.Json.Util (UTCTimeMillis (..))
import Data.LegalHold (UserLegalHoldStatus (..), defUserLegalHoldStatus)
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as Map
import Data.Range
import qualified Data.Set as Set
import Data.UUID.V4 (nextRandom)
import Galley.App
import qualified Galley.Cassandra.Conversation as C
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
import qualified Galley.Types.Teams as Teams
import Galley.Types.Teams.Intra
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

team :: TeamId -> Galley r (Maybe TeamData)
team tid =
  fmap toTeam <$> retry x1 (query1 Cql.selectTeam (params Quorum (Identity tid)))
  where
    toTeam (u, n, i, k, d, s, st, b) =
      let t = newTeam tid u n i (fromMaybe NonBinding b) & teamIconKey .~ k
          status = if d then PendingDelete else fromMaybe Active s
       in TeamData t status (writeTimeToUTC <$> st)

teamName :: TeamId -> Galley r (Maybe Text)
teamName tid =
  fmap runIdentity
    <$> retry x1 (query1 Cql.selectTeamName (params Quorum (Identity tid)))

teamIdsOf :: UserId -> Range 1 32 (List TeamId) -> Galley r [TeamId]
teamIdsOf usr (fromList . fromRange -> tids) =
  map runIdentity <$> retry x1 (query Cql.selectUserTeamsIn (params Quorum (usr, tids)))

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

teamConversation :: TeamId -> ConvId -> Galley r (Maybe TeamConversation)
teamConversation t c =
  fmap (newTeamConversation c . runIdentity)
    <$> retry x1 (query1 Cql.selectTeamConv (params Quorum (t, c)))

teamConversations :: TeamId -> Galley r [TeamConversation]
teamConversations t =
  map (uncurry newTeamConversation)
    <$> retry x1 (query Cql.selectTeamConvs (params Quorum (Identity t)))

teamConversationsForPagination :: TeamId -> Maybe ConvId -> Range 1 HardTruncationLimit Int32 -> Galley r (Page TeamConversation)
teamConversationsForPagination tid start (fromRange -> max) =
  fmap (uncurry newTeamConversation) <$> case start of
    Just c -> paginate Cql.selectTeamConvsFrom (paramsP Quorum (tid, c) max)
    Nothing -> paginate Cql.selectTeamConvs (paramsP Quorum (Identity tid) max)

teamMembersForFanout :: TeamId -> Galley r TeamMemberList
teamMembersForFanout t = fanoutLimit >>= teamMembersWithLimit t

teamMembersWithLimit :: TeamId -> Range 1 HardTruncationLimit Int32 -> Galley r TeamMemberList
teamMembersWithLimit t (fromRange -> limit) = do
  -- NOTE: We use +1 as size and then trim it due to the semantics of C* when getting a page with the exact same size
  pageTuple <- retry x1 (paginate Cql.selectTeamMembers (paramsP Quorum (Identity t) (limit + 1)))
  ms <- mapM (newTeamMember' t) . take (fromIntegral limit) $ result pageTuple
  pure $
    if hasMore pageTuple
      then newTeamMemberList ms ListTruncated
      else newTeamMemberList ms ListComplete

-- This function has a bit of a difficult type to work with because we don't have a pure function of type
-- (UserId, Permissions, Maybe UserId, Maybe UTCTimeMillis, Maybe UserLegalHoldStatus) -> TeamMember so we
-- cannot fmap over the ResultSet. We don't want to mess around with the Result size nextPage either otherwise
teamMembersForPagination :: TeamId -> Maybe UserId -> Range 1 HardTruncationLimit Int32 -> Galley r (Page (UserId, Permissions, Maybe UserId, Maybe UTCTimeMillis, Maybe UserLegalHoldStatus))
teamMembersForPagination tid start (fromRange -> max) =
  case start of
    Just u -> paginate Cql.selectTeamMembersFrom (paramsP Quorum (tid, u) max)
    Nothing -> paginate Cql.selectTeamMembers (paramsP Quorum (Identity tid) max)

-- NOTE: Use this function with care... should only be required when deleting a team!
--       Maybe should be left explicitly for the caller?
teamMembersCollectedWithPagination :: TeamId -> Galley r [TeamMember]
teamMembersCollectedWithPagination tid = do
  mems <- teamMembersForPagination tid Nothing (unsafeRange 2000)
  collectTeamMembersPaginated [] mems
  where
    collectTeamMembersPaginated acc mems = do
      tMembers <- mapM (newTeamMember' tid) (result mems)
      if (null $ result mems)
        then collectTeamMembersPaginated (tMembers ++ acc) =<< liftClient (nextPage mems)
        else return (tMembers ++ acc)

-- Lookup only specific team members: this is particularly useful for large teams when
-- needed to look up only a small subset of members (typically 2, user to perform the action
-- and the target user)
teamMembersLimited :: TeamId -> [UserId] -> Galley r [TeamMember]
teamMembersLimited t u =
  mapM (newTeamMember' t)
    =<< retry x1 (query Cql.selectTeamMembers' (params Quorum (t, u)))

teamMember :: TeamId -> UserId -> Galley r (Maybe TeamMember)
teamMember t u = newTeamMember'' u =<< retry x1 (query1 Cql.selectTeamMember (params Quorum (t, u)))
  where
    newTeamMember'' ::
      UserId ->
      Maybe (Permissions, Maybe UserId, Maybe UTCTimeMillis, Maybe UserLegalHoldStatus) ->
      Galley r (Maybe TeamMember)
    newTeamMember'' _ Nothing = pure Nothing
    newTeamMember'' uid (Just (perms, minvu, minvt, mulhStatus)) =
      Just <$> newTeamMember' t (uid, perms, minvu, minvt, mulhStatus)

userTeams :: UserId -> Galley r [TeamId]
userTeams u =
  map runIdentity
    <$> retry x1 (query Cql.selectUserTeams (params Quorum (Identity u)))

usersTeams :: [UserId] -> Galley r (Map UserId TeamId)
usersTeams uids = liftClient $ do
  pairs :: [(UserId, TeamId)] <-
    catMaybes
      <$> UnliftIO.pooledMapConcurrentlyN 8 (\uid -> (uid,) <$$> oneUserTeamC uid) uids
  pure $ foldl' (\m (k, v) -> Map.insert k v m) Map.empty pairs

oneUserTeam :: UserId -> Galley r (Maybe TeamId)
oneUserTeam = liftClient . oneUserTeamC

oneUserTeamC :: UserId -> Client (Maybe TeamId)
oneUserTeamC u =
  fmap runIdentity
    <$> retry x1 (query1 Cql.selectOneUserTeam (params Quorum (Identity u)))

teamCreationTime :: TeamId -> Galley r (Maybe TeamCreationTime)
teamCreationTime t =
  checkCreation . fmap runIdentity
    <$> retry x1 (query1 Cql.selectTeamBindingWritetime (params Quorum (Identity t)))
  where
    checkCreation (Just (Just ts)) = Just $ TeamCreationTime ts
    checkCreation _ = Nothing

teamBinding :: TeamId -> Galley r (Maybe TeamBinding)
teamBinding t =
  fmap (fromMaybe NonBinding . runIdentity)
    <$> retry x1 (query1 Cql.selectTeamBinding (params Quorum (Identity t)))

createTeam ::
  Maybe TeamId ->
  UserId ->
  Range 1 256 Text ->
  Range 1 256 Text ->
  Maybe (Range 1 256 Text) ->
  TeamBinding ->
  Galley r Team
createTeam t uid (fromRange -> n) (fromRange -> i) k b = do
  tid <- maybe (Id <$> liftIO nextRandom) return t
  retry x5 $ write Cql.insertTeam (params Quorum (tid, uid, n, i, fromRange <$> k, initialStatus b, b))
  pure (newTeam tid uid n i b & teamIconKey .~ (fromRange <$> k))
  where
    initialStatus Binding = PendingActive -- Team becomes Active after User account activation
    initialStatus NonBinding = Active

deleteTeam :: TeamId -> Galley r ()
deleteTeam tid = do
  -- TODO: delete service_whitelist records that mention this team
  retry x5 $ write Cql.markTeamDeleted (params Quorum (PendingDelete, tid))
  mems <- teamMembersForPagination tid Nothing (unsafeRange 2000)
  removeTeamMembers mems
  cnvs <- teamConversationsForPagination tid Nothing (unsafeRange 2000)
  removeConvs cnvs
  retry x5 $ write Cql.deleteTeam (params Quorum (Deleted, tid))
  where
    removeConvs :: Page TeamConversation -> Galley r ()
    removeConvs cnvs = do
      for_ (result cnvs) $ removeTeamConv tid . view conversationId
      unless (null $ result cnvs) $
        removeConvs =<< liftClient (nextPage cnvs)

    removeTeamMembers ::
      Page
        ( UserId,
          Permissions,
          Maybe UserId,
          Maybe UTCTimeMillis,
          Maybe UserLegalHoldStatus
        ) ->
      Galley r ()
    removeTeamMembers mems = do
      mapM_ (removeTeamMember tid . view _1) (result mems)
      unless (null $ result mems) $
        removeTeamMembers =<< liftClient (nextPage mems)

addTeamMember :: TeamId -> TeamMember -> Galley r ()
addTeamMember t m =
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency Quorum
    addPrepQuery
      Cql.insertTeamMember
      ( t,
        m ^. userId,
        m ^. permissions,
        m ^? invitation . _Just . _1,
        m ^? invitation . _Just . _2
      )
    addPrepQuery Cql.insertUserTeam (m ^. userId, t)
    when (m `hasPermission` SetBilling) $
      addPrepQuery Cql.insertBillingTeamMember (t, m ^. userId)

updateTeamMember ::
  -- | Old permissions, used for maintaining 'billing_team_member' table
  Permissions ->
  TeamId ->
  UserId ->
  -- | New permissions
  Permissions ->
  Galley r ()
updateTeamMember oldPerms tid uid newPerms = do
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency Quorum
    addPrepQuery Cql.updatePermissions (newPerms, tid, uid)

    when (SetBilling `Set.member` acquiredPerms) $
      addPrepQuery Cql.insertBillingTeamMember (tid, uid)

    when (SetBilling `Set.member` lostPerms) $
      addPrepQuery Cql.deleteBillingTeamMember (tid, uid)
  where
    permDiff = Set.difference `on` view Teams.self
    acquiredPerms = newPerms `permDiff` oldPerms
    lostPerms = oldPerms `permDiff` newPerms

removeTeamMember :: TeamId -> UserId -> Galley r ()
removeTeamMember t m =
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency Quorum
    addPrepQuery Cql.deleteTeamMember (t, m)
    addPrepQuery Cql.deleteUserTeam (m, t)
    addPrepQuery Cql.deleteBillingTeamMember (t, m)

listBillingTeamMembers :: TeamId -> Galley r [UserId]
listBillingTeamMembers tid =
  fmap runIdentity
    <$> retry x1 (query Cql.listBillingTeamMembers (params Quorum (Identity tid)))

removeTeamConv :: TeamId -> ConvId -> Galley r ()
removeTeamConv tid cid = liftClient $ do
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency Quorum
    addPrepQuery Cql.markConvDeleted (Identity cid)
    addPrepQuery Cql.deleteTeamConv (tid, cid)
  C.deleteConversation cid

updateTeamStatus :: TeamId -> TeamStatus -> Galley r ()
updateTeamStatus t s = retry x5 $ write Cql.updateTeamStatus (params Quorum (s, t))

updateTeam :: TeamId -> TeamUpdateData -> Galley r ()
updateTeam tid u = retry x5 . batch $ do
  setType BatchLogged
  setConsistency Quorum
  for_ (u ^. nameUpdate) $ \n ->
    addPrepQuery Cql.updateTeamName (fromRange n, tid)
  for_ (u ^. iconUpdate) $ \i ->
    addPrepQuery Cql.updateTeamIcon (fromRange i, tid)
  for_ (u ^. iconKeyUpdate) $ \k ->
    addPrepQuery Cql.updateTeamIconKey (fromRange k, tid)

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
