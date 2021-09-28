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
    createConversation,
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
    updateClient,

    -- * Utilities
    one2OneConvId,
    newMember,

    -- * Defaults
    defRole,
    defRegularConvAccess,
  )
where

import Brig.Types.Code
import Cassandra hiding (Tagged)
import Cassandra.Util
import Control.Arrow (second)
import Control.Exception (ErrorCall (ErrorCall))
import Control.Lens hiding ((<|))
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Extra (ifM)
import Data.ByteString.Conversion hiding (parser)
import Data.Domain (Domain)
import Data.Id as Id
import Data.Json.Util (UTCTimeMillis (..))
import Data.LegalHold (UserLegalHoldStatus (..), defUserLegalHoldStatus)
import qualified Data.List.Extra as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as Map
import Data.Misc (Milliseconds)
import qualified Data.Monoid
import Data.Qualified
import Data.Range
import qualified Data.Set as Set
import Data.Tagged
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
import Galley.Types.Teams hiding (Event, EventType (..), teamConversations, teamMembers)
import Galley.Types.Teams.Intra
import Galley.Types.UserList
import Galley.Validation
import Imports hiding (Set, max)
import System.Logger.Class (MonadLogger)
import qualified System.Logger.Class as Log
import UnliftIO (async, mapConcurrently, wait)
import UnliftIO.Async (pooledMapConcurrentlyN)
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
insertCode :: MonadClient m => Code -> m ()
insertCode c = do
  let k = codeKey c
  let v = codeValue c
  let cnv = codeConversation c
  let t = round (codeTTL c)
  let s = codeScope c
  retry x5 (write Cql.insertCode (params Quorum (k, v, cnv, s, t)))

-- | Lookup a conversation by code.
lookupCode :: MonadClient m => Key -> Scope -> m (Maybe Code)
lookupCode k s = fmap (toCode k s) <$> retry x1 (query1 Cql.lookupCode (params Quorum (k, s)))

-- | Delete a code associated with the given conversation key
deleteCode :: MonadClient m => Key -> Scope -> m ()
deleteCode k s = retry x5 $ write Cql.deleteCode (params Quorum (k, s))

-- Teams --------------------------------------------------------------------

team :: MonadClient m => TeamId -> m (Maybe TeamData)
team tid =
  fmap toTeam <$> retry x1 (query1 Cql.selectTeam (params Quorum (Identity tid)))
  where
    toTeam (u, n, i, k, d, s, st, b) =
      let t = newTeam tid u n i (fromMaybe NonBinding b) & teamIconKey .~ k
          status = if d then PendingDelete else fromMaybe Active s
       in TeamData t status (writeTimeToUTC <$> st)

teamName :: MonadClient m => TeamId -> m (Maybe Text)
teamName tid =
  fmap runIdentity
    <$> retry x1 (query1 Cql.selectTeamName (params Quorum (Identity tid)))

teamIdsOf :: MonadClient m => UserId -> Range 1 32 (List TeamId) -> m [TeamId]
teamIdsOf usr (fromList . fromRange -> tids) =
  map runIdentity <$> retry x1 (query Cql.selectUserTeamsIn (params Quorum (usr, tids)))

teamIdsFrom :: MonadClient m => UserId -> Maybe TeamId -> Range 1 100 Int32 -> m (ResultSet TeamId)
teamIdsFrom usr range (fromRange -> max) =
  mkResultSet . fmap runIdentity . strip <$> case range of
    Just c -> paginate Cql.selectUserTeamsFrom (paramsP Quorum (usr, c) (max + 1))
    Nothing -> paginate Cql.selectUserTeams (paramsP Quorum (Identity usr) (max + 1))
  where
    strip p = p {result = take (fromIntegral max) (result p)}

teamIdsForPagination :: MonadClient m => UserId -> Maybe TeamId -> Range 1 100 Int32 -> m (Page TeamId)
teamIdsForPagination usr range (fromRange -> max) =
  fmap runIdentity <$> case range of
    Just c -> paginate Cql.selectUserTeamsFrom (paramsP Quorum (usr, c) max)
    Nothing -> paginate Cql.selectUserTeams (paramsP Quorum (Identity usr) max)

teamConversation :: MonadClient m => TeamId -> ConvId -> m (Maybe TeamConversation)
teamConversation t c =
  fmap (newTeamConversation c . runIdentity)
    <$> retry x1 (query1 Cql.selectTeamConv (params Quorum (t, c)))

teamConversations :: MonadClient m => TeamId -> m [TeamConversation]
teamConversations t =
  map (uncurry newTeamConversation)
    <$> retry x1 (query Cql.selectTeamConvs (params Quorum (Identity t)))

teamConversationsForPagination :: MonadClient m => TeamId -> Maybe ConvId -> Range 1 HardTruncationLimit Int32 -> m (Page TeamConversation)
teamConversationsForPagination tid start (fromRange -> max) =
  fmap (uncurry newTeamConversation) <$> case start of
    Just c -> paginate Cql.selectTeamConvsFrom (paramsP Quorum (tid, c) max)
    Nothing -> paginate Cql.selectTeamConvs (paramsP Quorum (Identity tid) max)

teamMembersForFanout :: TeamId -> Galley TeamMemberList
teamMembersForFanout t = fanoutLimit >>= teamMembersWithLimit t

teamMembersWithLimit :: forall m. (MonadThrow m, MonadClient m, MonadReader Env m) => TeamId -> Range 1 HardTruncationLimit Int32 -> m TeamMemberList
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
teamMembersForPagination :: MonadClient m => TeamId -> Maybe UserId -> Range 1 HardTruncationLimit Int32 -> m (Page (UserId, Permissions, Maybe UserId, Maybe UTCTimeMillis, Maybe UserLegalHoldStatus))
teamMembersForPagination tid start (fromRange -> max) =
  case start of
    Just u -> paginate Cql.selectTeamMembersFrom (paramsP Quorum (tid, u) max)
    Nothing -> paginate Cql.selectTeamMembers (paramsP Quorum (Identity tid) max)

-- NOTE: Use this function with care... should only be required when deleting a team!
--       Maybe should be left explicitly for the caller?
teamMembersCollectedWithPagination :: TeamId -> Galley [TeamMember]
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
teamMembersLimited :: forall m. (MonadThrow m, MonadClient m, MonadReader Env m) => TeamId -> [UserId] -> m [TeamMember]
teamMembersLimited t u =
  mapM (newTeamMember' t)
    =<< retry x1 (query Cql.selectTeamMembers' (params Quorum (t, u)))

teamMember :: forall m. (MonadThrow m, MonadClient m, MonadReader Env m) => TeamId -> UserId -> m (Maybe TeamMember)
teamMember t u = newTeamMember'' u =<< retry x1 (query1 Cql.selectTeamMember (params Quorum (t, u)))
  where
    newTeamMember'' ::
      UserId ->
      Maybe (Permissions, Maybe UserId, Maybe UTCTimeMillis, Maybe UserLegalHoldStatus) ->
      m (Maybe TeamMember)
    newTeamMember'' _ Nothing = pure Nothing
    newTeamMember'' uid (Just (perms, minvu, minvt, mulhStatus)) =
      Just <$> newTeamMember' t (uid, perms, minvu, minvt, mulhStatus)

userTeams :: MonadClient m => UserId -> m [TeamId]
userTeams u =
  map runIdentity
    <$> retry x1 (query Cql.selectUserTeams (params Quorum (Identity u)))

usersTeams :: (MonadUnliftIO m, MonadClient m) => [UserId] -> m (Map UserId TeamId)
usersTeams uids = do
  pairs :: [(UserId, TeamId)] <- catMaybes <$> pooledMapConcurrentlyN 8 (\uid -> (uid,) <$$> oneUserTeam uid) uids
  pure $ foldl' (\m (k, v) -> Map.insert k v m) Map.empty pairs

oneUserTeam :: MonadClient m => UserId -> m (Maybe TeamId)
oneUserTeam u =
  fmap runIdentity
    <$> retry x1 (query1 Cql.selectOneUserTeam (params Quorum (Identity u)))

teamCreationTime :: MonadClient m => TeamId -> m (Maybe TeamCreationTime)
teamCreationTime t =
  checkCreation . fmap runIdentity
    <$> retry x1 (query1 Cql.selectTeamBindingWritetime (params Quorum (Identity t)))
  where
    checkCreation (Just (Just ts)) = Just $ TeamCreationTime ts
    checkCreation _ = Nothing

teamBinding :: MonadClient m => TeamId -> m (Maybe TeamBinding)
teamBinding t =
  fmap (fromMaybe NonBinding . runIdentity)
    <$> retry x1 (query1 Cql.selectTeamBinding (params Quorum (Identity t)))

createTeam ::
  MonadClient m =>
  Maybe TeamId ->
  UserId ->
  Range 1 256 Text ->
  Range 1 256 Text ->
  Maybe (Range 1 256 Text) ->
  TeamBinding ->
  m Team
createTeam t uid (fromRange -> n) (fromRange -> i) k b = do
  tid <- maybe (Id <$> liftIO nextRandom) return t
  retry x5 $ write Cql.insertTeam (params Quorum (tid, uid, n, i, fromRange <$> k, initialStatus b, b))
  pure (newTeam tid uid n i b & teamIconKey .~ (fromRange <$> k))
  where
    initialStatus Binding = PendingActive -- Team becomes Active after User account activation
    initialStatus NonBinding = Active

deleteTeam :: forall m. (MonadClient m, Log.MonadLogger m, MonadThrow m) => TeamId -> m ()
deleteTeam tid = do
  -- TODO: delete service_whitelist records that mention this team
  retry x5 $ write Cql.markTeamDeleted (params Quorum (PendingDelete, tid))
  mems <- teamMembersForPagination tid Nothing (unsafeRange 2000)
  removeTeamMembers mems
  cnvs <- teamConversationsForPagination tid Nothing (unsafeRange 2000)
  removeConvs cnvs
  retry x5 $ write Cql.deleteTeam (params Quorum (Deleted, tid))
  where
    removeConvs :: Page TeamConversation -> m ()
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
      m ()
    removeTeamMembers mems = do
      mapM_ (removeTeamMember tid . view _1) (result mems)
      unless (null $ result mems) $
        removeTeamMembers =<< liftClient (nextPage mems)

addTeamMember :: MonadClient m => TeamId -> TeamMember -> m ()
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
  MonadClient m =>
  -- | Old permissions, used for maintaining 'billing_team_member' table
  Permissions ->
  TeamId ->
  UserId ->
  -- | New permissions
  Permissions ->
  m ()
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
    permDiff = Set.difference `on` view self
    acquiredPerms = newPerms `permDiff` oldPerms
    lostPerms = oldPerms `permDiff` newPerms

removeTeamMember :: MonadClient m => TeamId -> UserId -> m ()
removeTeamMember t m =
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency Quorum
    addPrepQuery Cql.deleteTeamMember (t, m)
    addPrepQuery Cql.deleteUserTeam (m, t)
    addPrepQuery Cql.deleteBillingTeamMember (t, m)

listBillingTeamMembers :: MonadClient m => TeamId -> m [UserId]
listBillingTeamMembers tid =
  fmap runIdentity
    <$> retry x1 (query Cql.listBillingTeamMembers (params Quorum (Identity tid)))

removeTeamConv :: (MonadClient m, Log.MonadLogger m, MonadThrow m) => TeamId -> ConvId -> m ()
removeTeamConv tid cid = do
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency Quorum
    addPrepQuery Cql.markConvDeleted (Identity cid)
    addPrepQuery Cql.deleteTeamConv (tid, cid)
  deleteConversation cid

updateTeamStatus :: MonadClient m => TeamId -> TeamStatus -> m ()
updateTeamStatus t s = retry x5 $ write Cql.updateTeamStatus (params Quorum (s, t))

updateTeam :: MonadClient m => TeamId -> TeamUpdateData -> m ()
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

isConvAlive :: MonadClient m => ConvId -> m Bool
isConvAlive cid = do
  result <- retry x1 (query1 Cql.isConvDeleted (params Quorum (Identity cid)))
  case runIdentity <$> result of
    Nothing -> pure False
    Just Nothing -> pure True
    Just (Just True) -> pure False
    Just (Just False) -> pure True

conversation ::
  (MonadUnliftIO m, MonadClient m, Log.MonadLogger m, MonadThrow m) =>
  ConvId ->
  m (Maybe Conversation)
conversation conv = do
  cdata <- async $ retry x1 (query1 Cql.selectConv (params Quorum (Identity conv)))
  remoteMems <- async $ lookupRemoteMembers conv
  mbConv <- toConv conv <$> members conv <*> wait remoteMems <*> wait cdata
  return mbConv >>= conversationGC

{- "Garbage collect" the conversation, i.e. the conversation may be
   marked as deleted, in which case we delete it and return Nothing -}
conversationGC ::
  (MonadClient m, Log.MonadLogger m, MonadThrow m) =>
  Maybe Conversation ->
  m (Maybe Conversation)
conversationGC conv = case join (convDeleted <$> conv) of
  (Just True) -> do
    sequence_ $ deleteConversation . convId <$> conv
    return Nothing
  _ -> return conv

localConversations ::
  (MonadLogger m, MonadUnliftIO m, MonadClient m) =>
  [ConvId] ->
  m [Conversation]
localConversations [] = return []
localConversations ids = do
  convs <- async fetchConvs
  mems <- async $ memberLists ids
  remoteMems <- async $ remoteMemberLists ids
  cs <- zipWith4 toConv ids <$> wait mems <*> wait remoteMems <*> wait convs
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

conversationMeta :: MonadClient m => Domain -> ConvId -> m (Maybe ConversationMetadata)
conversationMeta localDomain conv =
  fmap toConvMeta
    <$> retry x1 (query1 Cql.selectConv (params Quorum (Identity conv)))
  where
    toConvMeta (t, c, a, r, n, i, _, mt, rm) =
      ConversationMetadata
        (Qualified conv localDomain)
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
  (MonadClient m) =>
  UserId ->
  Maybe ConvId ->
  Range 1 1000 Int32 ->
  m (ResultSet ConvId)
conversationIdsFrom usr start (fromRange -> max) =
  mkResultSet . strip . fmap runIdentity <$> case start of
    Just c -> paginate Cql.selectUserConvsFrom (paramsP Quorum (usr, c) (max + 1))
    Nothing -> paginate Cql.selectUserConvs (paramsP Quorum (Identity usr) (max + 1))
  where
    strip p = p {result = take (fromIntegral max) (result p)}

localConversationIdsPageFrom ::
  (MonadClient m) =>
  UserId ->
  Maybe PagingState ->
  Range 1 1000 Int32 ->
  m (PageWithState ConvId)
localConversationIdsPageFrom usr pagingState (fromRange -> max) =
  fmap runIdentity <$> paginateWithState Cql.selectUserConvs (paramsPagingState Quorum (Identity usr) max pagingState)

remoteConversationIdsPageFrom :: (MonadClient m) => UserId -> Maybe PagingState -> Int32 -> m (PageWithState (Qualified ConvId))
remoteConversationIdsPageFrom usr pagingState max =
  uncurry (flip Qualified) <$$> paginateWithState Cql.selectUserRemoteConvs (paramsPagingState Quorum (Identity usr) max pagingState)

localConversationIdRowsForPagination :: MonadClient m => UserId -> Maybe ConvId -> Range 1 1000 Int32 -> m (Page ConvId)
localConversationIdRowsForPagination usr start (fromRange -> max) =
  runIdentity
    <$$> case start of
      Just c -> paginate Cql.selectUserConvsFrom (paramsP Quorum (usr, c) max)
      Nothing -> paginate Cql.selectUserConvs (paramsP Quorum (Identity usr) max)

-- | Takes a list of conversation ids and returns those found for the given
-- user.
localConversationIdsOf :: forall m. (MonadClient m, MonadUnliftIO m) => UserId -> [ConvId] -> m [ConvId]
localConversationIdsOf usr cids = do
  runIdentity <$$> retry x1 (query Cql.selectUserConvsIn (params Quorum (usr, cids)))

-- | Takes a list of remote conversation ids and fetches member status flags
-- for the given user
remoteConversationStatus ::
  (MonadClient m, MonadUnliftIO m) =>
  UserId ->
  [Remote ConvId] ->
  m (Map (Remote ConvId) MemberStatus)
remoteConversationStatus uid =
  fmap mconcat
    . pooledMapConcurrentlyN 8 (uncurry (remoteConversationStatusOnDomain uid))
    . partitionRemote

remoteConversationStatusOnDomain :: MonadClient m => UserId -> Domain -> [ConvId] -> m (Map (Remote ConvId) MemberStatus)
remoteConversationStatusOnDomain uid domain convs =
  Map.fromList . map toPair
    <$> query Cql.selectRemoteConvMemberStatuses (params Quorum (uid, domain, convs))
  where
    toPair (conv, omus, omur, oar, oarr, hid, hidr) =
      ( toRemote (Qualified conv domain),
        toMemberStatus (omus, omur, oar, oarr, hid, hidr)
      )

conversationsRemote :: (MonadClient m) => UserId -> m [Remote ConvId]
conversationsRemote usr = do
  (\(d, c) -> toRemote $ Qualified c d) <$$> retry x1 (query Cql.selectUserRemoteConvs (params Quorum (Identity usr)))

createConversation ::
  MonadClient m =>
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
  m Conversation
createConversation lusr name acc role others tinfo mtimer recpt othersConversationRole = do
  conv <- Id <$> liftIO nextRandom
  let lconv = qualifyAs lusr conv
      usr = lUnqualified lusr
  retry x5 $ case tinfo of
    Nothing ->
      write Cql.insertConv (params Quorum (conv, RegularConv, usr, Set (toList acc), role, fromRange <$> name, Nothing, mtimer, recpt))
    Just ti -> batch $ do
      setType BatchLogged
      setConsistency Quorum
      addPrepQuery Cql.insertConv (conv, RegularConv, usr, Set (toList acc), role, fromRange <$> name, Just (cnvTeamId ti), mtimer, recpt)
      addPrepQuery Cql.insertTeamConv (cnvTeamId ti, conv, cnvManaged ti)
  let newUsers = fmap (,othersConversationRole) (fromConvSize others)
  (lmems, rmems) <- addMembers lconv (ulAddLocal (lUnqualified lusr, roleNameWireAdmin) newUsers)
  pure $ newConv conv RegularConv usr lmems rmems acc role name (cnvTeamId <$> tinfo) mtimer recpt

createSelfConversation :: MonadClient m => Local UserId -> Maybe (Range 1 256 Text) -> m Conversation
createSelfConversation lusr name = do
  let usr = lUnqualified lusr
      conv = selfConv usr
      lconv = qualifyAs lusr conv
  retry x5 $
    write Cql.insertConv (params Quorum (conv, SelfConv, usr, privateOnly, privateRole, fromRange <$> name, Nothing, Nothing, Nothing))
  (lmems, rmems) <- addMembers lconv (UserList [lUnqualified lusr] [])
  pure $ newConv conv SelfConv usr lmems rmems [PrivateAccess] privateRole name Nothing Nothing Nothing

createConnectConversation ::
  MonadClient m =>
  Local x ->
  U.UUID U.V4 ->
  U.UUID U.V4 ->
  Maybe (Range 1 256 Text) ->
  m Conversation
createConnectConversation loc a b name = do
  let conv = one2OneConvId a b
      lconv = qualifyAs loc conv
      a' = Id . U.unpack $ a
  retry x5 $
    write Cql.insertConv (params Quorum (conv, ConnectConv, a', privateOnly, privateRole, fromRange <$> name, Nothing, Nothing, Nothing))
  -- We add only one member, second one gets added later,
  -- when the other user accepts the connection request.
  (lmems, rmems) <- addMembers lconv (UserList [a'] [])
  pure $ newConv conv ConnectConv a' lmems rmems [PrivateAccess] privateRole name Nothing Nothing Nothing

createOne2OneConversation ::
  MonadClient m =>
  Local x ->
  U.UUID U.V4 ->
  U.UUID U.V4 ->
  Maybe (Range 1 256 Text) ->
  Maybe TeamId ->
  m Conversation
createOne2OneConversation loc a b name ti = do
  let conv = one2OneConvId a b
      lconv = qualifyAs loc conv
      a' = Id (U.unpack a)
      b' = Id (U.unpack b)
  retry x5 $ case ti of
    Nothing -> write Cql.insertConv (params Quorum (conv, One2OneConv, a', privateOnly, privateRole, fromRange <$> name, Nothing, Nothing, Nothing))
    Just tid -> batch $ do
      setType BatchLogged
      setConsistency Quorum
      addPrepQuery Cql.insertConv (conv, One2OneConv, a', privateOnly, privateRole, fromRange <$> name, Just tid, Nothing, Nothing)
      addPrepQuery Cql.insertTeamConv (tid, conv, False)
  -- FUTUREWORK: federated one2one
  (lmems, rmems) <- addMembers lconv (UserList [a', b'] [])
  pure $ newConv conv One2OneConv a' lmems rmems [PrivateAccess] privateRole name ti Nothing Nothing

updateConversation :: MonadClient m => ConvId -> Range 1 256 Text -> m ()
updateConversation cid name = retry x5 $ write Cql.updateConvName (params Quorum (fromRange name, cid))

updateConversationAccess :: MonadClient m => ConvId -> Set.Set Access -> AccessRole -> m ()
updateConversationAccess cid acc role = retry x5 $ write Cql.updateConvAccess (params Quorum (Set (toList acc), role, cid))

updateConversationReceiptMode :: MonadClient m => ConvId -> ReceiptMode -> m ()
updateConversationReceiptMode cid receiptMode = retry x5 $ write Cql.updateConvReceiptMode (params Quorum (receiptMode, cid))

lookupReceiptMode :: MonadClient m => ConvId -> m (Maybe ReceiptMode)
lookupReceiptMode cid = join . fmap runIdentity <$> retry x1 (query1 Cql.selectReceiptMode (params Quorum (Identity cid)))

updateConversationMessageTimer :: MonadClient m => ConvId -> Maybe Milliseconds -> m ()
updateConversationMessageTimer cid mtimer = retry x5 $ write Cql.updateConvMessageTimer (params Quorum (mtimer, cid))

deleteConversation :: (MonadClient m, Log.MonadLogger m, MonadThrow m) => ConvId -> m ()
deleteConversation cid = do
  retry x5 $ write Cql.markConvDeleted (params Quorum (Identity cid))
  mm <- members cid
  for_ mm $ \m -> removeMember (lmId m) cid
  retry x5 $ write Cql.deleteConv (params Quorum (Identity cid))

acceptConnect :: MonadClient m => ConvId -> m ()
acceptConnect cid = retry x5 $ write Cql.updateConvType (params Quorum (One2OneConv, cid))

-- | We deduce the conversation ID by adding the 4 components of the V4 UUID
-- together pairwise, and then setting the version bits (v4) and variant bits
-- (variant 2). This means that we always know what the UUID is for a
-- one-to-one conversation which hopefully makes them unique.
one2OneConvId :: U.UUID U.V4 -> U.UUID U.V4 -> ConvId
one2OneConvId a b = Id . U.unpack $ U.addv4 a b

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

convMetadata :: Domain -> Conversation -> ConversationMetadata
convMetadata localDomain c =
  ConversationMetadata
    (Qualified (convId c) localDomain)
    (convType c)
    (convCreator c)
    (convAccess c)
    (convAccessRole c)
    (convName c)
    (convTeam c)
    (convMessageTimer c)
    (convReceiptMode c)

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
  (MonadClient m, Log.MonadLogger m, MonadThrow m) =>
  ConvId ->
  UserId ->
  m (Maybe LocalMember)
member cnv usr =
  (toMember =<<)
    <$> retry x1 (query1 Cql.selectMember (params Quorum (cnv, usr)))

remoteMemberLists ::
  (MonadClient m) =>
  [ConvId] ->
  m [[RemoteMember]]
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
toRemoteMember u d = RemoteMember (toRemote (Qualified u d))

memberLists ::
  (MonadClient m, Log.MonadLogger m, MonadThrow m) =>
  [ConvId] ->
  m [[LocalMember]]
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

members :: (MonadClient m, Log.MonadLogger m, MonadThrow m) => ConvId -> m [LocalMember]
members conv = join <$> memberLists [conv]

lookupRemoteMembers :: (MonadClient m) => ConvId -> m [RemoteMember]
lookupRemoteMembers conv = join <$> remoteMemberLists [conv]

-- | Add a member to a local conversation, as an admin.
addMember :: MonadClient m => Local ConvId -> Local UserId -> m [LocalMember]
addMember c u = fst <$> addMembers c (UserList [lUnqualified u] [])

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
addMembers ::
  forall m a.
  (MonadClient m, ToUserRole a) =>
  Local ConvId ->
  UserList a ->
  m ([LocalMember], [RemoteMember])
addMembers (lUnqualified -> conv) (fmap toUserRole -> UserList lusers rusers) = do
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
      for_ chunk $ \(unTagged -> Qualified (uid, role) domain) -> do
        -- User is remote, so we only add it to the member_remote_user
        -- table, but the reverse mapping has to be done on the remote
        -- backend; so we assume an additional call to their backend has
        -- been (or will be) made separately. See Galley.API.Update.addMembers
        addPrepQuery Cql.insertRemoteMember (conv, domain, uid, role)

  pure (map newMemberWithRole lusers, map newRemoteMemberWithRole rusers)

-- | Set local users as belonging to a remote conversation. This is invoked by a
-- remote galley when users from the current backend are added to conversations
-- on the remote end.
addLocalMembersToRemoteConv :: MonadClient m => Qualified ConvId -> [UserId] -> m ()
addLocalMembersToRemoteConv _ [] = pure ()
addLocalMembersToRemoteConv qconv users = do
  -- FUTUREWORK: consider using pooledMapConcurrentlyN
  for_ (List.chunksOf 32 users) $ \chunk ->
    retry x5 . batch $ do
      setType BatchLogged
      setConsistency Quorum
      for_ chunk $ \u ->
        addPrepQuery
          Cql.insertUserRemoteConv
          (u, qDomain qconv, qUnqualified qconv)

updateSelfMember ::
  MonadClient m =>
  Local x ->
  Qualified ConvId ->
  Local UserId ->
  MemberUpdate ->
  m ()
updateSelfMember loc = foldQualified loc updateSelfMemberLocalConv updateSelfMemberRemoteConv

updateSelfMemberLocalConv ::
  MonadClient m =>
  Local ConvId ->
  Local UserId ->
  MemberUpdate ->
  m ()
updateSelfMemberLocalConv lcid luid mup = do
  retry x5 . batch $ do
    setType BatchUnLogged
    setConsistency Quorum
    for_ (mupOtrMuteStatus mup) $ \ms ->
      addPrepQuery
        Cql.updateOtrMemberMutedStatus
        (ms, mupOtrMuteRef mup, lUnqualified lcid, lUnqualified luid)
    for_ (mupOtrArchive mup) $ \a ->
      addPrepQuery
        Cql.updateOtrMemberArchived
        (a, mupOtrArchiveRef mup, lUnqualified lcid, lUnqualified luid)
    for_ (mupHidden mup) $ \h ->
      addPrepQuery
        Cql.updateMemberHidden
        (h, mupHiddenRef mup, lUnqualified lcid, lUnqualified luid)

updateSelfMemberRemoteConv ::
  MonadClient m =>
  Remote ConvId ->
  Local UserId ->
  MemberUpdate ->
  m ()
updateSelfMemberRemoteConv (Tagged (Qualified cid domain)) luid mup = do
  retry x5 . batch $ do
    setType BatchUnLogged
    setConsistency Quorum
    for_ (mupOtrMuteStatus mup) $ \ms ->
      addPrepQuery
        Cql.updateRemoteOtrMemberMutedStatus
        (ms, mupOtrMuteRef mup, domain, cid, lUnqualified luid)
    for_ (mupOtrArchive mup) $ \a ->
      addPrepQuery
        Cql.updateRemoteOtrMemberArchived
        (a, mupOtrArchiveRef mup, domain, cid, lUnqualified luid)
    for_ (mupHidden mup) $ \h ->
      addPrepQuery
        Cql.updateRemoteMemberHidden
        (h, mupHiddenRef mup, domain, cid, lUnqualified luid)

updateOtherMember ::
  MonadClient m =>
  Local x ->
  Qualified ConvId ->
  Qualified UserId ->
  OtherMemberUpdate ->
  m ()
updateOtherMember loc = foldQualified loc updateOtherMemberLocalConv updateOtherMemberRemoteConv

updateOtherMemberLocalConv ::
  MonadClient m =>
  Local ConvId ->
  Qualified UserId ->
  OtherMemberUpdate ->
  m ()
updateOtherMemberLocalConv lcid quid omu =
  do
    let addQuery r
          | lDomain lcid == qDomain quid =
            addPrepQuery
              Cql.updateMemberConvRoleName
              (r, lUnqualified lcid, qUnqualified quid)
          | otherwise =
            addPrepQuery
              Cql.updateRemoteMemberConvRoleName
              (r, lUnqualified lcid, qDomain quid, qUnqualified quid)
    retry x5 . batch $ do
      setType BatchUnLogged
      setConsistency Quorum
      traverse_ addQuery (omuConvRoleName omu)

-- FUTUREWORK: https://wearezeta.atlassian.net/browse/SQCORE-887
updateOtherMemberRemoteConv ::
  MonadClient m =>
  Remote ConvId ->
  Qualified UserId ->
  OtherMemberUpdate ->
  m ()
updateOtherMemberRemoteConv _ _ _ = pure ()

-- | Select only the members of a remote conversation from a list of users.
-- Return the filtered list and a boolean indicating whether the all the input
-- users are members.
filterRemoteConvMembers :: (MonadUnliftIO m, MonadClient m) => [UserId] -> Qualified ConvId -> m ([UserId], Bool)
filterRemoteConvMembers users (Qualified conv dom) =
  fmap Data.Monoid.getAll
    . foldMap (\muser -> (muser, Data.Monoid.All (not (null muser))))
    <$> pooledMapConcurrentlyN 8 filterMember users
  where
    filterMember :: MonadClient m => UserId -> m [UserId]
    filterMember user =
      fmap (map runIdentity)
        . retry x1
        $ query Cql.selectRemoteConvMembers (params Quorum (user, dom, conv))

removeLocalMembersFromLocalConv ::
  MonadClient m =>
  ConvId ->
  NonEmpty UserId ->
  m ()
removeLocalMembersFromLocalConv cnv victims = do
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency Quorum
    for_ victims $ \victim -> do
      addPrepQuery Cql.removeMember (cnv, victim)
      addPrepQuery Cql.deleteUserConv (victim, cnv)

removeRemoteMembersFromLocalConv ::
  MonadClient m =>
  ConvId ->
  NonEmpty (Remote UserId) ->
  m ()
removeRemoteMembersFromLocalConv cnv victims = do
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency Quorum
    for_ victims $ \(unTagged -> Qualified uid domain) ->
      addPrepQuery Cql.removeRemoteMember (cnv, domain, uid)

removeLocalMembersFromRemoteConv ::
  MonadClient m =>
  -- | The conversation to remove members from
  Qualified ConvId ->
  -- | Members to remove local to this backend
  [UserId] ->
  m ()
removeLocalMembersFromRemoteConv _ [] = pure ()
removeLocalMembersFromRemoteConv (Qualified conv convDomain) victims =
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency Quorum
    for_ victims $ \u -> addPrepQuery Cql.deleteUserRemoteConv (u, convDomain, conv)

removeMember :: MonadClient m => UserId -> ConvId -> m ()
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
newRemoteMemberWithRole ur@(unTagged -> (Qualified (u, r) _)) =
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

updateClient :: MonadClient m => Bool -> UserId -> ClientId -> m ()
updateClient add usr cls = do
  let q = if add then Cql.addMemberClient else Cql.rmMemberClient
  retry x5 $ write (q cls) (params Quorum (Identity usr))

-- Do, at most, 16 parallel lookups of up to 128 users each
lookupClients ::
  (MonadClient m, MonadUnliftIO m) =>
  [UserId] ->
  m Clients
lookupClients users =
  Clients.fromList . concat . concat
    <$> forM (chunksOf 2048 users) (mapConcurrently getClients . chunksOf 128)
  where
    getClients us =
      map (second fromSet)
        <$> retry x1 (query Cql.selectClients (params Quorum (Identity us)))

eraseClients :: MonadClient m => UserId -> m ()
eraseClients user = retry x5 (write Cql.rmClients (params Quorum (Identity user)))

-- Internal utilities

-- | Construct 'TeamMember' from database tuple.
-- If FeatureLegalHoldWhitelistTeamsAndImplicitConsent is enabled set UserLegalHoldDisabled
-- if team is whitelisted.
--
-- Throw an exception if one of invitation timestamp and inviter is 'Nothing' and the
-- other is 'Just', which can only be caused by inconsistent database content.
newTeamMember' :: (MonadIO m, MonadThrow m, MonadClient m, MonadReader Env m) => TeamId -> (UserId, Permissions, Maybe UserId, Maybe UTCTimeMillis, Maybe UserLegalHoldStatus) -> m TeamMember
newTeamMember' tid (uid, perms, minvu, minvt, fromMaybe defUserLegalHoldStatus -> lhStatus) = do
  mk minvu minvt >>= maybeGrant
  where
    maybeGrant :: (MonadClient m, MonadReader Env m) => TeamMember -> m TeamMember
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
  ([TeamMember] -> Galley ()) ->
  Galley ()
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
