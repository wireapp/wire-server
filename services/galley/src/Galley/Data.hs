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

    -- * Conversations
    Conversation (..),
    convMetadata,
    convAccessData,
    acceptConnect,
    conversation,
    conversationIdsFrom,
    localConversationIdsOf,
    remoteConversationStatus,
    localConversationIdsPageFrom,
    localConversationIdRowsForPagination,
    localConversations,
    conversationMeta,
    conversationsRemote,
    createConnectConversation,
    createConnectConversationWithRemote,
    createConversation,
    createLegacyOne2OneConversation,
    createOne2OneConversation,
    createSelfConversation,
    isConvAlive,
    updateConversation,
    updateConversationAccess,
    updateConversationReceiptMode,
    updateConversationMessageTimer,
    deleteConversation,
    lookupReceiptMode,
    remoteConversationIdsPageFrom,

    -- * Conversation Members
    addMember,
    addMembers,
    addLocalMembersToRemoteConv,
    member,
    members,
    lookupRemoteMembers,
    removeMember,
    removeLocalMembersFromLocalConv,
    removeRemoteMembersFromLocalConv,
    removeLocalMembersFromRemoteConv,
    updateSelfMember,
    updateSelfMemberLocalConv,
    updateSelfMemberRemoteConv,
    updateOtherMember,
    updateOtherMemberLocalConv,
    updateOtherMemberRemoteConv,
    ToUserRole (..),
    toQualifiedUserRole,
    filterRemoteConvMembers,

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
    newMember,

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
import Data.Domain (Domain)
import Data.Id as Id
import Data.Json.Util (UTCTimeMillis (..))
import Data.LegalHold (UserLegalHoldStatus (..), defUserLegalHoldStatus)
import qualified Data.List.Extra as List
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as Map
import Data.Misc (Milliseconds)
import qualified Data.Monoid
import Data.Qualified
import Data.Range
import qualified Data.Set as Set
import qualified Data.UUID.Tagged as U
import Data.UUID.V4 (nextRandom)
import Galley.App
import Galley.Data.Instances ()
import Galley.Data.LegalHold (isTeamLegalholdWhitelisted)
import qualified Galley.Data.Queries as Cql
import Galley.Data.Types as Data
import Galley.Types hiding (Conversation)
import Galley.Types.Bot (newServiceRef)
import Galley.Types.Clients (Clients)
import qualified Galley.Types.Clients as Clients
import Galley.Types.Conversations.Members
import Galley.Types.Conversations.Roles
import Galley.Types.Teams hiding
  ( Event,
    EventType (..),
    self,
    teamConversations,
    teamMembers,
  )
import qualified Galley.Types.Teams as Teams
import Galley.Types.Teams.Intra
import Galley.Types.UserList
import Galley.Validation
import Imports hiding (Set, max)
import qualified System.Logger.Class as Log
import qualified UnliftIO
import Wire.API.Team.Member

-- We use this newtype to highlight the fact that the 'Page' wrapped in here
-- can not reliably used for paging.
--
-- The reason for this is that Cassandra returns 'hasMore' as true if the
-- page size requested is equal to result size. To work around this we
-- actually request for one additional element and drop the last value if
-- necessary. This means however that 'nextPage' does not work properly as
-- we would miss a value on every page size.
-- Thus, and since we don't want to expose the ResultSet constructor
-- because it gives access to `nextPage`, we give accessors to the results
-- and a more typed `hasMore` (ResultSetComplete | ResultSetTruncated)
data ResultSet a = ResultSet
  { resultSetResult :: [a],
    resultSetType :: ResultSetType
  }
  deriving stock (Show, Functor, Foldable, Traversable)

-- | A more descriptive type than using a simple bool to represent `hasMore`
data ResultSetType
  = ResultSetComplete
  | ResultSetTruncated
  deriving stock (Eq, Show)

mkResultSet :: Page a -> ResultSet a
mkResultSet page = ResultSet (result page) typ
  where
    typ
      | hasMore page = ResultSetTruncated
      | otherwise = ResultSetComplete

schemaVersion :: Int32
schemaVersion = 53

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
removeTeamConv tid cid = do
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency Quorum
    addPrepQuery Cql.markConvDeleted (Identity cid)
    addPrepQuery Cql.deleteTeamConv (tid, cid)
  deleteConversation cid

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

-- Conversations ------------------------------------------------------------

isConvAlive :: ConvId -> Galley r Bool
isConvAlive cid = do
  result <- retry x1 (query1 Cql.isConvDeleted (params Quorum (Identity cid)))
  case runIdentity <$> result of
    Nothing -> pure False
    Just Nothing -> pure True
    Just (Just True) -> pure False
    Just (Just False) -> pure True

conversation :: ConvId -> Galley r (Maybe Conversation)
conversation conv = liftClient $ do
  cdata <- UnliftIO.async $ retry x1 (query1 Cql.selectConv (params Quorum (Identity conv)))
  remoteMems <- UnliftIO.async $ lookupRemoteMembersC conv
  mbConv <-
    toConv conv
      <$> membersC conv
      <*> UnliftIO.wait remoteMems
      <*> UnliftIO.wait cdata
  return mbConv >>= conversationGC

{- "Garbage collect" the conversation, i.e. the conversation may be
   marked as deleted, in which case we delete it and return Nothing -}
conversationGC ::
  Maybe Conversation ->
  Client (Maybe Conversation)
conversationGC conv = case join (convDeleted <$> conv) of
  (Just True) -> do
    sequence_ $ deleteConversationC . convId <$> conv
    return Nothing
  _ -> return conv

localConversations :: [ConvId] -> Galley r [Conversation]
localConversations [] = return []
localConversations ids = do
  cs <- liftClient $ do
    convs <- UnliftIO.async fetchConvs
    mems <- UnliftIO.async $ memberLists ids
    remoteMems <- UnliftIO.async $ remoteMemberLists ids
    zipWith4 toConv ids
      <$> UnliftIO.wait mems
      <*> UnliftIO.wait remoteMems
      <*> UnliftIO.wait convs
  foldrM flatten [] (zip ids cs)
  where
    fetchConvs = do
      cs <- retry x1 $ query Cql.selectConvs (params Quorum (Identity ids))
      let m = Map.fromList $ map (\(c, t, u, n, a, r, i, d, mt, rm) -> (c, (t, u, n, a, r, i, d, mt, rm))) cs
      return $ map (`Map.lookup` m) ids
    flatten (i, c) cc = case c of
      Nothing -> do
        Log.warn $ Log.msg ("No conversation for: " <> toByteString i)
        return cc
      Just c' -> return (c' : cc)

toConv ::
  ConvId ->
  [LocalMember] ->
  [RemoteMember] ->
  Maybe (ConvType, UserId, Maybe (Set Access), Maybe AccessRole, Maybe Text, Maybe TeamId, Maybe Bool, Maybe Milliseconds, Maybe ReceiptMode) ->
  Maybe Conversation
toConv cid mms remoteMems conv =
  f mms <$> conv
  where
    f ms (cty, uid, acc, role, nme, ti, del, timer, rm) = Conversation cid cty uid nme (defAccess cty acc) (maybeRole cty role) ms remoteMems ti del timer rm

conversationMeta :: Domain -> ConvId -> Galley r (Maybe ConversationMetadata)
conversationMeta _localDomain conv =
  fmap toConvMeta
    <$> retry x1 (query1 Cql.selectConv (params Quorum (Identity conv)))
  where
    toConvMeta (t, c, a, r, n, i, _, mt, rm) =
      ConversationMetadata
        t
        c
        (defAccess t a)
        (maybeRole t r)
        n
        i
        mt
        rm

-- | Deprecated, use 'localConversationIdsPageFrom'
conversationIdsFrom ::
  UserId ->
  Maybe ConvId ->
  Range 1 1000 Int32 ->
  Galley r (ResultSet ConvId)
conversationIdsFrom usr start (fromRange -> max) =
  mkResultSet . strip . fmap runIdentity <$> case start of
    Just c -> paginate Cql.selectUserConvsFrom (paramsP Quorum (usr, c) (max + 1))
    Nothing -> paginate Cql.selectUserConvs (paramsP Quorum (Identity usr) (max + 1))
  where
    strip p = p {result = take (fromIntegral max) (result p)}

localConversationIdsPageFrom ::
  UserId ->
  Maybe PagingState ->
  Range 1 1000 Int32 ->
  Galley r (PageWithState ConvId)
localConversationIdsPageFrom usr pagingState (fromRange -> max) =
  fmap runIdentity <$> paginateWithState Cql.selectUserConvs (paramsPagingState Quorum (Identity usr) max pagingState)

remoteConversationIdsPageFrom :: UserId -> Maybe PagingState -> Int32 -> Galley r (PageWithState (Qualified ConvId))
remoteConversationIdsPageFrom usr pagingState max =
  uncurry (flip Qualified) <$$> paginateWithState Cql.selectUserRemoteConvs (paramsPagingState Quorum (Identity usr) max pagingState)

localConversationIdRowsForPagination :: UserId -> Maybe ConvId -> Range 1 1000 Int32 -> Galley r (Page ConvId)
localConversationIdRowsForPagination usr start (fromRange -> max) =
  runIdentity
    <$$> case start of
      Just c -> paginate Cql.selectUserConvsFrom (paramsP Quorum (usr, c) max)
      Nothing -> paginate Cql.selectUserConvs (paramsP Quorum (Identity usr) max)

-- | Takes a list of conversation ids and returns those found for the given
-- user.
localConversationIdsOf :: UserId -> [ConvId] -> Galley r [ConvId]
localConversationIdsOf usr cids = do
  runIdentity <$$> retry x1 (query Cql.selectUserConvsIn (params Quorum (usr, cids)))

-- | Takes a list of remote conversation ids and fetches member status flags
-- for the given user
remoteConversationStatus ::
  UserId ->
  [Remote ConvId] ->
  Galley r (Map (Remote ConvId) MemberStatus)
remoteConversationStatus uid =
  liftClient
    . fmap mconcat
    . UnliftIO.pooledMapConcurrentlyN 8 (remoteConversationStatusOnDomainC uid)
    . bucketRemote

remoteConversationStatusOnDomainC :: UserId -> Remote [ConvId] -> Client (Map (Remote ConvId) MemberStatus)
remoteConversationStatusOnDomainC uid rconvs =
  Map.fromList . map toPair
    <$> query Cql.selectRemoteConvMemberStatuses (params Quorum (uid, tDomain rconvs, tUnqualified rconvs))
  where
    toPair (conv, omus, omur, oar, oarr, hid, hidr) =
      ( qualifyAs rconvs conv,
        toMemberStatus (omus, omur, oar, oarr, hid, hidr)
      )

conversationsRemote :: UserId -> Galley r [Remote ConvId]
conversationsRemote usr = do
  uncurry toRemoteUnsafe <$$> retry x1 (query Cql.selectUserRemoteConvs (params Quorum (Identity usr)))

createConversation ::
  Local UserId ->
  Maybe (Range 1 256 Text) ->
  [Access] ->
  AccessRole ->
  ConvSizeChecked UserList UserId ->
  Maybe ConvTeamInfo ->
  -- | Message timer
  Maybe Milliseconds ->
  Maybe ReceiptMode ->
  RoleName ->
  Galley r Conversation
createConversation lusr name acc role others tinfo mtimer recpt othersConversationRole = do
  conv <- Id <$> liftIO nextRandom
  let lconv = qualifyAs lusr conv
      usr = tUnqualified lusr
  retry x5 $ case tinfo of
    Nothing ->
      write Cql.insertConv (params Quorum (conv, RegularConv, usr, Set (toList acc), role, fromRange <$> name, Nothing, mtimer, recpt))
    Just ti -> batch $ do
      setType BatchLogged
      setConsistency Quorum
      addPrepQuery Cql.insertConv (conv, RegularConv, usr, Set (toList acc), role, fromRange <$> name, Just (cnvTeamId ti), mtimer, recpt)
      addPrepQuery Cql.insertTeamConv (cnvTeamId ti, conv, cnvManaged ti)
  let newUsers = fmap (,othersConversationRole) (fromConvSize others)
  (lmems, rmems) <- addMembers lconv (ulAddLocal (tUnqualified lusr, roleNameWireAdmin) newUsers)
  pure $ newConv conv RegularConv usr lmems rmems acc role name (cnvTeamId <$> tinfo) mtimer recpt

createSelfConversation :: Local UserId -> Maybe (Range 1 256 Text) -> Galley r Conversation
createSelfConversation lusr name = do
  let usr = tUnqualified lusr
      conv = selfConv usr
      lconv = qualifyAs lusr conv
  retry x5 $
    write Cql.insertConv (params Quorum (conv, SelfConv, usr, privateOnly, privateRole, fromRange <$> name, Nothing, Nothing, Nothing))
  (lmems, rmems) <- addMembers lconv (UserList [tUnqualified lusr] [])
  pure $ newConv conv SelfConv usr lmems rmems [PrivateAccess] privateRole name Nothing Nothing Nothing

createConnectConversation ::
  Local x ->
  U.UUID U.V4 ->
  U.UUID U.V4 ->
  Maybe (Range 1 256 Text) ->
  Galley r Conversation
createConnectConversation loc a b name = do
  let conv = localOne2OneConvId a b
      lconv = qualifyAs loc conv
      a' = Id . U.unpack $ a
  retry x5 $
    write Cql.insertConv (params Quorum (conv, ConnectConv, a', privateOnly, privateRole, fromRange <$> name, Nothing, Nothing, Nothing))
  -- We add only one member, second one gets added later,
  -- when the other user accepts the connection request.
  (lmems, rmems) <- addMembers lconv (UserList [a'] [])
  pure $ newConv conv ConnectConv a' lmems rmems [PrivateAccess] privateRole name Nothing Nothing Nothing

createConnectConversationWithRemote ::
  Local ConvId ->
  Local UserId ->
  UserList UserId ->
  Galley r ()
createConnectConversationWithRemote lconvId creator m = do
  retry x5 $
    write Cql.insertConv (params Quorum (tUnqualified lconvId, ConnectConv, tUnqualified creator, privateOnly, privateRole, Nothing, Nothing, Nothing, Nothing))
  -- We add only one member, second one gets added later,
  -- when the other user accepts the connection request.
  void $ addMembers lconvId m

createLegacyOne2OneConversation ::
  Local x ->
  U.UUID U.V4 ->
  U.UUID U.V4 ->
  Maybe (Range 1 256 Text) ->
  Maybe TeamId ->
  Galley r Conversation
createLegacyOne2OneConversation loc a b name ti = do
  let conv = localOne2OneConvId a b
      lconv = qualifyAs loc conv
      a' = Id (U.unpack a)
      b' = Id (U.unpack b)
  createOne2OneConversation
    lconv
    (qualifyAs loc a')
    (qUntagged (qualifyAs loc b'))
    name
    ti

createOne2OneConversation ::
  Local ConvId ->
  Local UserId ->
  Qualified UserId ->
  Maybe (Range 1 256 Text) ->
  Maybe TeamId ->
  Galley r Conversation
createOne2OneConversation lconv self other name mtid = do
  retry x5 $ case mtid of
    Nothing -> write Cql.insertConv (params Quorum (tUnqualified lconv, One2OneConv, tUnqualified self, privateOnly, privateRole, fromRange <$> name, Nothing, Nothing, Nothing))
    Just tid -> batch $ do
      setType BatchLogged
      setConsistency Quorum
      addPrepQuery Cql.insertConv (tUnqualified lconv, One2OneConv, tUnqualified self, privateOnly, privateRole, fromRange <$> name, Just tid, Nothing, Nothing)
      addPrepQuery Cql.insertTeamConv (tid, tUnqualified lconv, False)
  (lmems, rmems) <- addMembers lconv (toUserList self [qUntagged self, other])
  pure $ newConv (tUnqualified lconv) One2OneConv (tUnqualified self) lmems rmems [PrivateAccess] privateRole name mtid Nothing Nothing

updateConversation :: ConvId -> Range 1 256 Text -> Galley r ()
updateConversation cid name = retry x5 $ write Cql.updateConvName (params Quorum (fromRange name, cid))

updateConversationAccess :: ConvId -> ConversationAccessData -> Galley r ()
updateConversationAccess cid (ConversationAccessData acc role) =
  retry x5 $
    write Cql.updateConvAccess (params Quorum (Set (toList acc), role, cid))

updateConversationReceiptMode :: ConvId -> ReceiptMode -> Galley r ()
updateConversationReceiptMode cid receiptMode = retry x5 $ write Cql.updateConvReceiptMode (params Quorum (receiptMode, cid))

lookupReceiptMode :: ConvId -> Galley r (Maybe ReceiptMode)
lookupReceiptMode cid = join . fmap runIdentity <$> retry x1 (query1 Cql.selectReceiptMode (params Quorum (Identity cid)))

updateConversationMessageTimer :: ConvId -> Maybe Milliseconds -> Galley r ()
updateConversationMessageTimer cid mtimer = retry x5 $ write Cql.updateConvMessageTimer (params Quorum (mtimer, cid))

deleteConversation :: ConvId -> Galley r ()
deleteConversation = liftClient . deleteConversationC

deleteConversationC :: ConvId -> Client ()
deleteConversationC cid = do
  retry x5 $ write Cql.markConvDeleted (params Quorum (Identity cid))

  localMembers <- membersC cid
  for_ (nonEmpty localMembers) $ \ms ->
    removeLocalMembersFromLocalConvC cid (lmId <$> ms)

  remoteMembers <- lookupRemoteMembersC cid
  for_ (nonEmpty remoteMembers) $ \ms ->
    removeRemoteMembersFromLocalConvC cid (rmId <$> ms)

  retry x5 $ write Cql.deleteConv (params Quorum (Identity cid))

acceptConnect :: ConvId -> Galley r ()
acceptConnect cid = retry x5 $ write Cql.updateConvType (params Quorum (One2OneConv, cid))

-- | We deduce the conversation ID by adding the 4 components of the V4 UUID
-- together pairwise, and then setting the version bits (v4) and variant bits
-- (variant 2). This means that we always know what the UUID is for a
-- one-to-one conversation which hopefully makes them unique.
localOne2OneConvId :: U.UUID U.V4 -> U.UUID U.V4 -> ConvId
localOne2OneConvId a b = Id . U.unpack $ U.addv4 a b

newConv ::
  ConvId ->
  ConvType ->
  UserId ->
  [LocalMember] ->
  [RemoteMember] ->
  [Access] ->
  AccessRole ->
  Maybe (Range 1 256 Text) ->
  Maybe TeamId ->
  Maybe Milliseconds ->
  Maybe ReceiptMode ->
  Conversation
newConv cid ct usr mems rMems acc role name tid mtimer rMode =
  Conversation
    { convId = cid,
      convType = ct,
      convCreator = usr,
      convName = fromRange <$> name,
      convAccess = acc,
      convAccessRole = role,
      convLocalMembers = mems,
      convRemoteMembers = rMems,
      convTeam = tid,
      convDeleted = Nothing,
      convMessageTimer = mtimer,
      convReceiptMode = rMode
    }

convMetadata :: Conversation -> ConversationMetadata
convMetadata c =
  ConversationMetadata
    (convType c)
    (convCreator c)
    (convAccess c)
    (convAccessRole c)
    (convName c)
    (convTeam c)
    (convMessageTimer c)
    (convReceiptMode c)

convAccessData :: Conversation -> ConversationAccessData
convAccessData conv =
  ConversationAccessData
    (Set.fromList (convAccess conv))
    (convAccessRole conv)

defAccess :: ConvType -> Maybe (Set Access) -> [Access]
defAccess SelfConv Nothing = [PrivateAccess]
defAccess ConnectConv Nothing = [PrivateAccess]
defAccess One2OneConv Nothing = [PrivateAccess]
defAccess RegularConv Nothing = defRegularConvAccess
defAccess SelfConv (Just (Set [])) = [PrivateAccess]
defAccess ConnectConv (Just (Set [])) = [PrivateAccess]
defAccess One2OneConv (Just (Set [])) = [PrivateAccess]
defAccess RegularConv (Just (Set [])) = defRegularConvAccess
defAccess _ (Just (Set (x : xs))) = x : xs

maybeRole :: ConvType -> Maybe AccessRole -> AccessRole
maybeRole SelfConv _ = privateRole
maybeRole ConnectConv _ = privateRole
maybeRole One2OneConv _ = privateRole
maybeRole RegularConv Nothing = defRole
maybeRole RegularConv (Just r) = r

defRole :: AccessRole
defRole = ActivatedAccessRole

defRegularConvAccess :: [Access]
defRegularConvAccess = [InviteAccess]

privateRole :: AccessRole
privateRole = PrivateAccessRole

privateOnly :: Set Access
privateOnly = Set [PrivateAccess]

-- Conversation Members -----------------------------------------------------

member ::
  ConvId ->
  UserId ->
  Galley r (Maybe LocalMember)
member cnv usr =
  (toMember =<<)
    <$> retry x1 (query1 Cql.selectMember (params Quorum (cnv, usr)))

remoteMemberLists :: [ConvId] -> Client [[RemoteMember]]
remoteMemberLists convs = do
  mems <- retry x1 $ query Cql.selectRemoteMembers (params Quorum (Identity convs))
  let convMembers = foldr (insert . mkMem) Map.empty mems
  return $ map (\c -> fromMaybe [] (Map.lookup c convMembers)) convs
  where
    insert (conv, mem) acc =
      let f = (Just . maybe [mem] (mem :))
       in Map.alter f conv acc
    mkMem (cnv, domain, usr, role) = (cnv, toRemoteMember usr domain role)

toRemoteMember :: UserId -> Domain -> RoleName -> RemoteMember
toRemoteMember u d = RemoteMember (toRemoteUnsafe d u)

memberLists :: [ConvId] -> Client [[LocalMember]]
memberLists convs = do
  mems <- retry x1 $ query Cql.selectMembers (params Quorum (Identity convs))
  let convMembers = foldr (\m acc -> insert (mkMem m) acc) mempty mems
  return $ map (\c -> fromMaybe [] (Map.lookup c convMembers)) convs
  where
    insert (_, Nothing) acc = acc
    insert (conv, Just mem) acc =
      let f = (Just . maybe [mem] (mem :))
       in Map.alter f conv acc
    mkMem (cnv, usr, srv, prv, st, omus, omur, oar, oarr, hid, hidr, crn) =
      (cnv, toMember (usr, srv, prv, st, omus, omur, oar, oarr, hid, hidr, crn))

members :: ConvId -> Galley r [LocalMember]
members = liftClient . membersC

membersC :: ConvId -> Client [LocalMember]
membersC = fmap concat . liftClient . memberLists . pure

lookupRemoteMembers :: ConvId -> Galley r [RemoteMember]
lookupRemoteMembers = liftClient . lookupRemoteMembersC

lookupRemoteMembersC :: ConvId -> Client [RemoteMember]
lookupRemoteMembersC conv = join <$> remoteMemberLists [conv]

-- | Add a member to a local conversation, as an admin.
addMember :: Local ConvId -> Local UserId -> Galley r [LocalMember]
addMember c u = fst <$> addMembers c (UserList [tUnqualified u] [])

class ToUserRole a where
  toUserRole :: a -> (UserId, RoleName)

instance ToUserRole (UserId, RoleName) where
  toUserRole = id

instance ToUserRole UserId where
  toUserRole uid = (uid, roleNameWireAdmin)

toQualifiedUserRole :: ToUserRole a => Qualified a -> (Qualified UserId, RoleName)
toQualifiedUserRole = requalify . fmap toUserRole
  where
    requalify (Qualified (a, role) dom) = (Qualified a dom, role)

-- | Add members to a local conversation.
-- Conversation is local, so we can add any member to it (including remote ones).
-- When the role is not specified, it defaults to admin.
-- Please make sure the conversation doesn't exceed the maximum size!
addMembers :: ToUserRole a => Local ConvId -> UserList a -> Galley r ([LocalMember], [RemoteMember])
addMembers (tUnqualified -> conv) (fmap toUserRole -> UserList lusers rusers) = do
  -- batch statement with 500 users are known to be above the batch size limit
  -- and throw "Batch too large" errors. Therefor we chunk requests and insert
  -- sequentially. (parallelizing would not aid performance as the partition
  -- key, i.e. the convId, is on the same cassandra node)
  -- chunk size 32 was chosen to lead to batch statements
  -- below the batch threshold
  -- With chunk size of 64:
  -- [galley] Server warning: Batch for [galley_test.member, galley_test.user] is of size 7040, exceeding specified threshold of 5120 by 1920.
  --
  for_ (List.chunksOf 32 lusers) $ \chunk -> do
    retry x5 . batch $ do
      setType BatchLogged
      setConsistency Quorum
      for_ chunk $ \(u, r) -> do
        -- User is local, too, so we add it to both the member and the user table
        addPrepQuery Cql.insertMember (conv, u, Nothing, Nothing, r)
        addPrepQuery Cql.insertUserConv (u, conv)

  for_ (List.chunksOf 32 rusers) $ \chunk -> do
    retry x5 . batch $ do
      setType BatchLogged
      setConsistency Quorum
      for_ chunk $ \(qUntagged -> Qualified (uid, role) domain) -> do
        -- User is remote, so we only add it to the member_remote_user
        -- table, but the reverse mapping has to be done on the remote
        -- backend; so we assume an additional call to their backend has
        -- been (or will be) made separately. See Galley.API.Update.addMembers
        addPrepQuery Cql.insertRemoteMember (conv, domain, uid, role)

  pure (map newMemberWithRole lusers, map newRemoteMemberWithRole rusers)

-- | Set local users as belonging to a remote conversation. This is invoked by a
-- remote galley when users from the current backend are added to conversations
-- on the remote end.
addLocalMembersToRemoteConv :: Remote ConvId -> [UserId] -> Galley r ()
addLocalMembersToRemoteConv _ [] = pure ()
addLocalMembersToRemoteConv rconv users = do
  -- FUTUREWORK: consider using pooledMapConcurrentlyN
  for_ (List.chunksOf 32 users) $ \chunk ->
    retry x5 . batch $ do
      setType BatchLogged
      setConsistency Quorum
      for_ chunk $ \u ->
        addPrepQuery
          Cql.insertUserRemoteConv
          (u, tDomain rconv, tUnqualified rconv)

updateSelfMember ::
  Local x ->
  Qualified ConvId ->
  Local UserId ->
  MemberUpdate ->
  Galley r ()
updateSelfMember loc = foldQualified loc updateSelfMemberLocalConv updateSelfMemberRemoteConv

updateSelfMemberLocalConv ::
  Local ConvId ->
  Local UserId ->
  MemberUpdate ->
  Galley r ()
updateSelfMemberLocalConv lcid luid mup = do
  retry x5 . batch $ do
    setType BatchUnLogged
    setConsistency Quorum
    for_ (mupOtrMuteStatus mup) $ \ms ->
      addPrepQuery
        Cql.updateOtrMemberMutedStatus
        (ms, mupOtrMuteRef mup, tUnqualified lcid, tUnqualified luid)
    for_ (mupOtrArchive mup) $ \a ->
      addPrepQuery
        Cql.updateOtrMemberArchived
        (a, mupOtrArchiveRef mup, tUnqualified lcid, tUnqualified luid)
    for_ (mupHidden mup) $ \h ->
      addPrepQuery
        Cql.updateMemberHidden
        (h, mupHiddenRef mup, tUnqualified lcid, tUnqualified luid)

updateSelfMemberRemoteConv ::
  Remote ConvId ->
  Local UserId ->
  MemberUpdate ->
  Galley r ()
updateSelfMemberRemoteConv (qUntagged -> Qualified cid domain) luid mup = do
  retry x5 . batch $ do
    setType BatchUnLogged
    setConsistency Quorum
    for_ (mupOtrMuteStatus mup) $ \ms ->
      addPrepQuery
        Cql.updateRemoteOtrMemberMutedStatus
        (ms, mupOtrMuteRef mup, domain, cid, tUnqualified luid)
    for_ (mupOtrArchive mup) $ \a ->
      addPrepQuery
        Cql.updateRemoteOtrMemberArchived
        (a, mupOtrArchiveRef mup, domain, cid, tUnqualified luid)
    for_ (mupHidden mup) $ \h ->
      addPrepQuery
        Cql.updateRemoteMemberHidden
        (h, mupHiddenRef mup, domain, cid, tUnqualified luid)

updateOtherMember ::
  Local x ->
  Qualified ConvId ->
  Qualified UserId ->
  OtherMemberUpdate ->
  Galley r ()
updateOtherMember loc = foldQualified loc updateOtherMemberLocalConv updateOtherMemberRemoteConv

updateOtherMemberLocalConv ::
  Local ConvId ->
  Qualified UserId ->
  OtherMemberUpdate ->
  Galley r ()
updateOtherMemberLocalConv lcid quid omu =
  do
    let addQuery r
          | tDomain lcid == qDomain quid =
            addPrepQuery
              Cql.updateMemberConvRoleName
              (r, tUnqualified lcid, qUnqualified quid)
          | otherwise =
            addPrepQuery
              Cql.updateRemoteMemberConvRoleName
              (r, tUnqualified lcid, qDomain quid, qUnqualified quid)
    retry x5 . batch $ do
      setType BatchUnLogged
      setConsistency Quorum
      traverse_ addQuery (omuConvRoleName omu)

-- FUTUREWORK: https://wearezeta.atlassian.net/browse/SQCORE-887
updateOtherMemberRemoteConv ::
  Remote ConvId ->
  Qualified UserId ->
  OtherMemberUpdate ->
  Galley r ()
updateOtherMemberRemoteConv _ _ _ = pure ()

-- | Select only the members of a remote conversation from a list of users.
-- Return the filtered list and a boolean indicating whether the all the input
-- users are members.
filterRemoteConvMembers ::
  [UserId] ->
  Qualified ConvId ->
  Galley r ([UserId], Bool)
filterRemoteConvMembers users (Qualified conv dom) =
  liftClient $
    fmap Data.Monoid.getAll
      . foldMap (\muser -> (muser, Data.Monoid.All (not (null muser))))
      <$> UnliftIO.pooledMapConcurrentlyN 8 filterMember users
  where
    filterMember :: UserId -> Client [UserId]
    filterMember user =
      fmap (map runIdentity)
        . retry x1
        $ query Cql.selectRemoteConvMembers (params Quorum (user, dom, conv))

removeLocalMembersFromLocalConv :: ConvId -> NonEmpty UserId -> Galley r ()
removeLocalMembersFromLocalConv cnv = liftClient . removeLocalMembersFromLocalConvC cnv

removeLocalMembersFromLocalConvC :: ConvId -> NonEmpty UserId -> Client ()
removeLocalMembersFromLocalConvC cnv victims = do
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency Quorum
    for_ victims $ \victim -> do
      addPrepQuery Cql.removeMember (cnv, victim)
      addPrepQuery Cql.deleteUserConv (victim, cnv)

removeRemoteMembersFromLocalConv :: ConvId -> NonEmpty (Remote UserId) -> Galley r ()
removeRemoteMembersFromLocalConv cnv = liftClient . removeRemoteMembersFromLocalConvC cnv

removeRemoteMembersFromLocalConvC :: ConvId -> NonEmpty (Remote UserId) -> Client ()
removeRemoteMembersFromLocalConvC cnv victims = do
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency Quorum
    for_ victims $ \(qUntagged -> Qualified uid domain) ->
      addPrepQuery Cql.removeRemoteMember (cnv, domain, uid)

removeLocalMembersFromRemoteConv ::
  -- | The conversation to remove members from
  Remote ConvId ->
  -- | Members to remove local to this backend
  [UserId] ->
  Galley r ()
removeLocalMembersFromRemoteConv _ [] = pure ()
removeLocalMembersFromRemoteConv (qUntagged -> Qualified conv convDomain) victims =
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency Quorum
    for_ victims $ \u -> addPrepQuery Cql.deleteUserRemoteConv (u, convDomain, conv)

removeMember :: UserId -> ConvId -> Galley r ()
removeMember usr cnv = retry x5 . batch $ do
  setType BatchLogged
  setConsistency Quorum
  addPrepQuery Cql.removeMember (cnv, usr)
  addPrepQuery Cql.deleteUserConv (usr, cnv)

newMember :: UserId -> LocalMember
newMember u = newMemberWithRole (u, roleNameWireAdmin)

newMemberWithRole :: (UserId, RoleName) -> LocalMember
newMemberWithRole (u, r) =
  LocalMember
    { lmId = u,
      lmService = Nothing,
      lmStatus = defMemberStatus,
      lmConvRoleName = r
    }

newRemoteMemberWithRole :: Remote (UserId, RoleName) -> RemoteMember
newRemoteMemberWithRole ur@(qUntagged -> (Qualified (u, r) _)) =
  RemoteMember
    { rmId = qualifyAs ur u,
      rmConvRoleName = r
    }

toMemberStatus ::
  ( -- otr muted
    Maybe MutedStatus,
    Maybe Text,
    -- otr archived
    Maybe Bool,
    Maybe Text,
    -- hidden
    Maybe Bool,
    Maybe Text
  ) ->
  MemberStatus
toMemberStatus (omus, omur, oar, oarr, hid, hidr) =
  MemberStatus
    { msOtrMutedStatus = omus,
      msOtrMutedRef = omur,
      msOtrArchived = fromMaybe False oar,
      msOtrArchivedRef = oarr,
      msHidden = fromMaybe False hid,
      msHiddenRef = hidr
    }

toMember ::
  ( UserId,
    Maybe ServiceId,
    Maybe ProviderId,
    Maybe Cql.MemberStatus,
    -- otr muted
    Maybe MutedStatus,
    Maybe Text,
    -- otr archived
    Maybe Bool,
    Maybe Text,
    -- hidden
    Maybe Bool,
    Maybe Text,
    -- conversation role name
    Maybe RoleName
  ) ->
  Maybe LocalMember
toMember (usr, srv, prv, Just 0, omus, omur, oar, oarr, hid, hidr, crn) =
  Just $
    LocalMember
      { lmId = usr,
        lmService = newServiceRef <$> srv <*> prv,
        lmStatus = toMemberStatus (omus, omur, oar, oarr, hid, hidr),
        lmConvRoleName = fromMaybe roleNameWireAdmin crn
      }
toMember _ = Nothing

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
