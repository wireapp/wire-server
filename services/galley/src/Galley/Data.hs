{-# LANGUAGE RecordWildCards #-}

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
    oneUserTeam,
    Galley.Data.teamBinding,
    teamCreationTime,
    deleteTeam,
    removeTeamConv,
    updateTeam,
    updateTeamStatus,

    -- * Conversations
    Conversation (..),
    acceptConnect,
    conversation,
    conversationIdsFrom,
    conversationIdRowsFrom,
    conversationIdRowsForPagination,
    conversationIdsOf,
    conversationMeta,
    conversations,
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

    -- * Conversation Members
    addMember,
    addMembersWithRole,
    member,
    members,
    removeMember,
    removeMembers,
    updateMember,

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
    MappedOrLocalIdRow,
    toMappedOrLocalId,

    -- * Defaults
    defRole,
    defRegularConvAccess,
  )
where

import Brig.Types.Code
import Cassandra
import Cassandra.Util
import Control.Arrow (second)
import Control.Lens hiding ((<|))
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Bifunctor (first)
import Data.ByteString.Conversion hiding (parser)
import Data.Coerce (coerce)
import Data.Domain (Domain)
import Data.Function (on)
import Data.Id as Id
import Data.IdMapping (IdMapping (IdMapping), MappedOrLocalId (Local, Mapped), opaqueIdFromMappedOrLocal)
import Data.Json.Util (UTCTimeMillis (..))
import Data.LegalHold (UserLegalHoldStatus (..))
import qualified Data.List.Extra as List
import Data.List.Split (chunksOf)
import Data.List1 (List1, list1, singleton)
import qualified Data.Map.Strict as Map
import Data.Misc (Milliseconds)
import Data.Qualified (Qualified (Qualified))
import Data.Range
import qualified Data.Set as Set
import qualified Data.String.Conversions as Str.C (cs)
import Data.Time.Clock
import qualified Data.UUID.Tagged as U
import Data.UUID.V4 (nextRandom)
import Galley.API.Error (internalErrorWithDescription)
import Galley.App
import Galley.Data.Instances ()
import qualified Galley.Data.Queries as Cql
import Galley.Data.Types as Data
import Galley.Types hiding (Conversation)
import Galley.Types.Bot (newServiceRef)
import Galley.Types.Clients (Clients)
import qualified Galley.Types.Clients as Clients
import Galley.Types.Conversations.Roles
import Galley.Types.Teams hiding (Event, EventType (..), teamConversations, teamMembers)
import Galley.Types.Teams.Intra
import Galley.Validation
import Imports hiding (Set, max)
import System.Logger.Class (MonadLogger)
import qualified System.Logger.Class as Log
import UnliftIO (async, mapConcurrently, wait)

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
schemaVersion = 45

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

teamConversationsForPagination :: MonadClient m => TeamId -> Maybe OpaqueConvId -> Range 1 HardTruncationLimit Int32 -> m (Page TeamConversation)
teamConversationsForPagination tid start (fromRange -> max) =
  fmap (uncurry newTeamConversation) <$> case start of
    Just c -> paginate Cql.selectTeamConvsFrom (paramsP Quorum (tid, c) max)
    Nothing -> paginate Cql.selectTeamConvs (paramsP Quorum (Identity tid) max)

teamMembersForFanout :: TeamId -> Galley TeamMemberList
teamMembersForFanout t = fanoutLimit >>= teamMembersWithLimit t

teamMembersWithLimit :: forall m. (MonadThrow m, MonadClient m) => TeamId -> Range 1 HardTruncationLimit Int32 -> m TeamMemberList
teamMembersWithLimit t (fromRange -> limit) = do
  -- NOTE: We use +1 as size and then trim it due to the semantics of C* when getting a page with the exact same size
  pageTuple <- retry x1 (paginate Cql.selectTeamMembers (paramsP Quorum (Identity t) (limit + 1)))
  ms <- mapM newTeamMember' . take (fromIntegral limit) $ result pageTuple
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
      tMembers <- mapM newTeamMember' (result mems)
      if (null $ result mems)
        then collectTeamMembersPaginated (tMembers ++ acc) =<< liftClient (nextPage mems)
        else return (tMembers ++ acc)

-- Lookup only specific team members: this is particularly useful for large teams when
-- needed to look up only a small subset of members (typically 2, user to perform the action
-- and the target user)
teamMembersLimited :: forall m. (MonadThrow m, MonadClient m) => TeamId -> [UserId] -> m [TeamMember]
teamMembersLimited t u =
  mapM newTeamMember'
    =<< retry x1 (query Cql.selectTeamMembers' (params Quorum (t, u)))

teamMember :: forall m. (MonadThrow m, MonadClient m) => TeamId -> UserId -> m (Maybe TeamMember)
teamMember t u = newTeamMember'' u =<< retry x1 (query1 Cql.selectTeamMember (params Quorum (t, u)))
  where
    newTeamMember'' ::
      UserId ->
      Maybe (Permissions, Maybe UserId, Maybe UTCTimeMillis, Maybe UserLegalHoldStatus) ->
      m (Maybe TeamMember)
    newTeamMember'' _ Nothing = pure Nothing
    newTeamMember'' uid (Just (perms, minvu, minvt, mulhStatus)) =
      Just <$> newTeamMember' (uid, perms, minvu, minvt, mulhStatus)

userTeams :: MonadClient m => UserId -> m [TeamId]
userTeams u =
  map runIdentity
    <$> retry x1 (query Cql.selectUserTeams (params Quorum (Identity u)))

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

deleteTeam :: (MonadClient m, Log.MonadLogger m, MonadThrow m) => TeamId -> m ()
deleteTeam tid = do
  retry x5 $ write Cql.markTeamDeleted (params Quorum (PendingDelete, tid))
  mems <- teamMembersForPagination tid Nothing (unsafeRange 2000)
  removeTeamMembers mems
  cnvs <- teamConversationsForPagination tid Nothing (unsafeRange 2000)
  removeConvs cnvs
  retry x5 $ write Cql.deleteTeam (params Quorum (Deleted, tid))
  where
    removeConvs cnvs = do
      for_ (result cnvs) $ removeTeamConv tid . view conversationId
      unless (null $ result cnvs) $
        removeConvs =<< liftClient (nextPage cnvs)
    removeTeamMembers mems = do
      tMembers <- mapM newTeamMember' (result mems)
      for_ tMembers $ removeTeamMember tid . view userId
      unless (null $ result mems) $
        removeTeamMembers =<< liftClient (nextPage mems)

-- TODO: delete service_whitelist records that mention this team

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
  mbConv <- toConv conv <$> members conv <*> wait cdata
  return mbConv >>= conversationGC

{- "Garbage collect" the conversation, i.e. the conversation may be
   marked as deleted, in which case we delete it and return Nothing -}
conversationGC ::
  (MonadClient m, Log.MonadLogger m, MonadThrow m) =>
  (Maybe Conversation) ->
  m (Maybe Conversation)
conversationGC conv = case join (convDeleted <$> conv) of
  (Just True) -> do
    sequence_ $ deleteConversation . convId <$> conv
    return Nothing
  _ -> return conv

conversations ::
  (MonadLogger m, MonadUnliftIO m, MonadClient m) =>
  [ConvId] ->
  m [Conversation]
conversations [] = return []
conversations ids = do
  convs <- async fetchConvs
  mems <- memberLists ids
  cs <- zipWith3 toConv ids mems <$> wait convs
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
  [Member] ->
  Maybe (ConvType, UserId, Maybe (Set Access), Maybe AccessRole, Maybe Text, Maybe TeamId, Maybe Bool, Maybe Milliseconds, Maybe ReceiptMode) ->
  Maybe Conversation
toConv cid mms conv =
  f mms <$> conv
  where
    f ms (cty, uid, acc, role, nme, ti, del, timer, rm) = Conversation cid cty uid nme (defAccess cty acc) (maybeRole cty role) ms ti del timer rm

conversationMeta :: MonadClient m => ConvId -> m (Maybe ConversationMeta)
conversationMeta conv =
  fmap toConvMeta
    <$> retry x1 (query1 Cql.selectConv (params Quorum (Identity conv)))
  where
    toConvMeta (t, c, a, r, n, i, _, mt, rm) = ConversationMeta conv t c (defAccess t a) (maybeRole t r) n i mt rm

conversationIdsFrom ::
  (MonadClient m, Log.MonadLogger m, MonadThrow m) =>
  UserId ->
  Maybe OpaqueConvId ->
  Range 1 1000 Int32 ->
  m (ResultSet (MappedOrLocalId Id.C))
conversationIdsFrom usr start max =
  traverse toMappedOrLocalId =<< conversationIdRowsFrom usr start max

conversationIdRowsFrom ::
  (MonadClient m) =>
  UserId ->
  Maybe OpaqueConvId ->
  Range 1 1000 Int32 ->
  m (ResultSet (MappedOrLocalIdRow Id.C))
conversationIdRowsFrom usr start (fromRange -> max) =
  mkResultSet . strip <$> case start of
    Just c -> paginate Cql.selectUserConvsFrom (paramsP Quorum (usr, c) (max + 1))
    Nothing -> paginate Cql.selectUserConvs (paramsP Quorum (Identity usr) (max + 1))
  where
    strip p = p {result = take (fromIntegral max) (result p)}

-- | We can't easily apply toMappedOrLocalId here, so we leave it to the consumers of this function.
conversationIdRowsForPagination :: MonadClient m => UserId -> Maybe OpaqueConvId -> Range 1 1000 Int32 -> m (Page (MappedOrLocalIdRow Id.C))
conversationIdRowsForPagination usr start (fromRange -> max) =
  case start of
    Just c -> paginate Cql.selectUserConvsFrom (paramsP Quorum (usr, c) max)
    Nothing -> paginate Cql.selectUserConvs (paramsP Quorum (Identity usr) max)

conversationIdsOf ::
  (MonadClient m, Log.MonadLogger m, MonadThrow m) =>
  UserId ->
  Range 1 32 (List OpaqueConvId) ->
  m [MappedOrLocalId Id.C]
conversationIdsOf usr (fromList . fromRange -> cids) =
  traverse toMappedOrLocalId
    =<< retry x1 (query Cql.selectUserConvsIn (params Quorum (usr, cids)))

createConversation ::
  MonadClient m =>
  UserId ->
  Maybe (Range 1 256 Text) ->
  [Access] ->
  AccessRole ->
  ConvSizeChecked [UserId] ->
  Maybe ConvTeamInfo ->
  -- | Message timer
  Maybe Milliseconds ->
  Maybe ReceiptMode ->
  RoleName ->
  m Conversation
createConversation usr name acc role others tinfo mtimer recpt othersConversationRole = do
  conv <- Id <$> liftIO nextRandom
  now <- liftIO getCurrentTime
  retry x5 $ case tinfo of
    Nothing ->
      write Cql.insertConv (params Quorum (conv, RegularConv, usr, Set (toList acc), role, fromRange <$> name, Nothing, mtimer, recpt))
    Just ti -> batch $ do
      setType BatchLogged
      setConsistency Quorum
      addPrepQuery Cql.insertConv (conv, RegularConv, usr, Set (toList acc), role, fromRange <$> name, Just (cnvTeamId ti), mtimer, recpt)
      addPrepQuery Cql.insertTeamConv (cnvTeamId ti, conv, cnvManaged ti)
  mems <- snd <$> addMembersUncheckedWithRole now conv (usr, roleNameWireAdmin) (list1 (usr, roleNameWireAdmin) ((,othersConversationRole) <$> fromConvSize others))
  return $ newConv conv RegularConv usr (toList mems) acc role name (cnvTeamId <$> tinfo) mtimer recpt

createSelfConversation :: MonadClient m => UserId -> Maybe (Range 1 256 Text) -> m Conversation
createSelfConversation usr name = do
  let conv = selfConv usr
  now <- liftIO getCurrentTime
  retry x5 $
    write Cql.insertConv (params Quorum (conv, SelfConv, usr, privateOnly, privateRole, fromRange <$> name, Nothing, Nothing, Nothing))
  mems <- snd <$> addMembersUnchecked now conv usr (singleton usr)
  return $ newConv conv SelfConv usr (toList mems) [PrivateAccess] privateRole name Nothing Nothing Nothing

createConnectConversation ::
  MonadClient m =>
  U.UUID U.V4 ->
  U.UUID U.V4 ->
  Maybe (Range 1 256 Text) ->
  Connect ->
  m (Conversation, Event)
createConnectConversation a b name conn = do
  let conv = one2OneConvId a b
      a' = Id . U.unpack $ a
  now <- liftIO getCurrentTime
  retry x5 $
    write Cql.insertConv (params Quorum (conv, ConnectConv, a', privateOnly, privateRole, fromRange <$> name, Nothing, Nothing, Nothing))
  -- We add only one member, second one gets added later,
  -- when the other user accepts the connection request.
  mems <- snd <$> addMembersUnchecked now conv a' (singleton a')
  let e = Event ConvConnect conv a' now (Just $ EdConnect conn)
  return (newConv conv ConnectConv a' (toList mems) [PrivateAccess] privateRole name Nothing Nothing Nothing, e)

createOne2OneConversation ::
  MonadClient m =>
  U.UUID U.V4 ->
  U.UUID U.V4 ->
  Maybe (Range 1 256 Text) ->
  Maybe TeamId ->
  m Conversation
createOne2OneConversation a b name ti = do
  let conv = one2OneConvId a b
      a' = Id (U.unpack a)
      b' = Id (U.unpack b)
  now <- liftIO getCurrentTime
  retry x5 $ case ti of
    Nothing -> write Cql.insertConv (params Quorum (conv, One2OneConv, a', privateOnly, privateRole, fromRange <$> name, Nothing, Nothing, Nothing))
    Just tid -> batch $ do
      setType BatchLogged
      setConsistency Quorum
      addPrepQuery Cql.insertConv (conv, One2OneConv, a', privateOnly, privateRole, fromRange <$> name, Just tid, Nothing, Nothing)
      addPrepQuery Cql.insertTeamConv (tid, conv, False)
  mems <- snd <$> addMembersUnchecked now conv a' (list1 a' [b'])
  return $ newConv conv One2OneConv a' (toList mems) [PrivateAccess] privateRole name ti Nothing Nothing

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
  for_ mm $ \m -> removeMember (memId m) cid
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
  [Member] ->
  [Access] ->
  AccessRole ->
  Maybe (Range 1 256 Text) ->
  Maybe TeamId ->
  Maybe Milliseconds ->
  Maybe ReceiptMode ->
  Conversation
newConv cid ct usr mems acc role name tid mtimer rMode =
  Conversation
    { convId = cid,
      convType = ct,
      convCreator = usr,
      convName = fromRange <$> name,
      convAccess = acc,
      convAccessRole = role,
      convMembers = mems,
      convTeam = tid,
      convDeleted = Nothing,
      convMessageTimer = mtimer,
      convReceiptMode = rMode
    }

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

type MappedOrLocalIdRow a = (Id (Opaque a), Maybe (Id (Remote a)), Maybe Domain)

toMappedOrLocalId :: (Log.MonadLogger m, MonadThrow m) => MappedOrLocalIdRow a -> m (MappedOrLocalId a)
toMappedOrLocalId = \case
  (mappedId, Just remoteId, Just domain) ->
    pure $ Mapped (IdMapping (coerce mappedId) (Qualified remoteId domain))
  (localId, Nothing, Nothing) ->
    pure $ Local (coerce localId)
  invalid -> do
    -- This should never happen as we always write rows with either both or none of these
    -- values.
    -- FUTUREWORK: we could try to recover from this situation by checking if an ID mapping
    -- for this mapped ID exists (and potentially even repair the row). At the moment, the
    -- problem seems unlikely enough not to warrant the complexity, though.
    -- In some cases it could also be better not to fail, but skip this entry, e.g. when
    -- deleting a user, we should remove him from all conversations we can, not stop halfway.
    let msg = "Invalid remote ID in database row: " <> show invalid
    Log.err $ Log.msg msg
    throwM $ internalErrorWithDescription (Str.C.cs msg)

fromMappedOrLocalId :: MappedOrLocalId a -> MappedOrLocalIdRow a
fromMappedOrLocalId = \case
  Local localId ->
    (makeIdOpaque localId, Nothing, Nothing)
  Mapped (IdMapping mappedId (Qualified remoteId domain)) ->
    (makeMappedIdOpaque mappedId, Just remoteId, Just domain)

-- Conversation Members -----------------------------------------------------

member ::
  (MonadClient m, Log.MonadLogger m, MonadThrow m) =>
  ConvId ->
  UserId ->
  m (Maybe Member)
member cnv usr =
  fmap (join @Maybe) . traverse toMember
    =<< retry x1 (query1 Cql.selectMember (params Quorum (cnv, makeIdOpaque usr)))

memberLists ::
  (MonadClient m, Log.MonadLogger m, MonadThrow m) =>
  [ConvId] ->
  m [[Member]]
memberLists convs = do
  mems <- retry x1 $ query Cql.selectMembers (params Quorum (Identity convs))
  convMembers <- foldrM (\m acc -> liftA2 insert (mkMem m) (pure acc)) Map.empty mems
  return $ map (\c -> fromMaybe [] (Map.lookup c convMembers)) convs
  where
    insert Nothing acc = acc
    insert (Just (conv, mem)) acc =
      let f = (Just . maybe [mem] (mem :))
       in Map.alter f conv acc
    mkMem (cnv, usr, usrRemoteId, usrRemoteDomain, srv, prv, st, omu, omus, omur, oar, oarr, hid, hidr, crn) =
      fmap (cnv,) <$> toMember (usr, usrRemoteId, usrRemoteDomain, srv, prv, st, omu, omus, omur, oar, oarr, hid, hidr, crn)

members :: (MonadClient m, Log.MonadLogger m, MonadThrow m) => ConvId -> m [Member]
members conv = join <$> memberLists [conv]

-- | Add a member to a local conversation, as an admin.
addMember :: MonadClient m => UTCTime -> ConvId -> UserId -> m (Event, List1 Member)
addMember t c u = addMembersUnchecked t c u (singleton u)

-- | Add members to a local conversation.
addMembersWithRole :: MonadClient m => UTCTime -> ConvId -> (UserId, RoleName) -> ConvMemberAddSizeChecked (List1 (UserId, RoleName)) -> m (Event, List1 Member)
addMembersWithRole t c orig mems = addMembersUncheckedWithRole t c orig (fromMemberSize mems)

-- | Add members to a local conversation, all as admins.
-- Please make sure the conversation doesn't exceed the maximum size!
addMembersUnchecked :: MonadClient m => UTCTime -> ConvId -> UserId -> List1 UserId -> m (Event, List1 Member)
addMembersUnchecked t conv orig usrs = addMembersUncheckedWithRole t conv (orig, roleNameWireAdmin) ((,roleNameWireAdmin) <$> usrs)

-- | Add members to a local conversation.
-- Please make sure the conversation doesn't exceed the maximum size!
--
-- For now, we only accept local 'UserId's, but that will change with federation.
addMembersUncheckedWithRole :: MonadClient m => UTCTime -> ConvId -> (UserId, RoleName) -> List1 (UserId, RoleName) -> m (Event, List1 Member)
addMembersUncheckedWithRole t conv (orig, _origRole) usrs = do
  -- batch statement with 500 users are known to be above the batch size limit
  -- and throw "Batch too large" errors. Therefor we chunk requests and insert
  -- sequentially. (parallelizing would not aid performance as the partition
  -- key, i.e. the convId, is on the same cassandra node)
  -- chunk size 32 was chosen to lead to batch statements
  -- below the batch threshold
  -- With chunk size of 64:
  -- [galley] Server warning: Batch for [galley_test.member, galley_test.user] is of size 7040, exceeding specified threshold of 5120 by 1920.
  for_ (List.chunksOf 32 (toList usrs)) $ \chunk -> do
    retry x5 . batch $ do
      setType BatchLogged
      setConsistency Quorum
      for_ chunk $ \(u, r) -> do
        -- Conversation is local, so we can add any member to it (including remote ones).
        let (usrOpaqueId, usrRemoteId, usrRemoteDomain) = fromMappedOrLocalId (Local u)
        addPrepQuery Cql.insertMember (conv, usrOpaqueId, usrRemoteId, usrRemoteDomain, Nothing, Nothing, r)
        -- Once we accept remote users in this function, we need to distinguish here between
        -- local and remote ones.
        -- - For local members, we add the conversation to the table as it's done already.
        -- - For remote members, we don't do anything here and assume an additional call to
        --   their backend has been (or will be) made separately.
        addPrepQuery Cql.insertUserConv (u, makeIdOpaque conv, Nothing, Nothing)
  let e = Event MemberJoin conv orig t (Just . EdMembersJoin . SimpleMembers . toSimpleMembers $ toList usrs)
  return (e, fmap (uncurry newMemberWithRole . first Local) usrs)
  where
    toSimpleMembers :: [(UserId, RoleName)] -> [SimpleMember]
    toSimpleMembers = fmap (uncurry SimpleMember)

updateMember :: MonadClient m => ConvId -> UserId -> MemberUpdate -> m MemberUpdateData
updateMember cid uid mup = do
  retry x5 . batch $ do
    setType BatchUnLogged
    setConsistency Quorum
    let opaqueUserId = makeIdOpaque uid
    for_ (mupOtrMute mup) $ \m ->
      addPrepQuery Cql.updateOtrMemberMuted (m, mupOtrMuteRef mup, cid, opaqueUserId)
    for_ (mupOtrMuteStatus mup) $ \ms ->
      addPrepQuery Cql.updateOtrMemberMutedStatus (ms, mupOtrMuteRef mup, cid, opaqueUserId)
    for_ (mupOtrArchive mup) $ \a ->
      addPrepQuery Cql.updateOtrMemberArchived (a, mupOtrArchiveRef mup, cid, opaqueUserId)
    for_ (mupHidden mup) $ \h ->
      addPrepQuery Cql.updateMemberHidden (h, mupHiddenRef mup, cid, opaqueUserId)
    for_ (mupConvRoleName mup) $ \r ->
      addPrepQuery Cql.updateMemberConvRoleName (r, cid, opaqueUserId)
  return
    MemberUpdateData
      { misTarget = Just uid,
        misOtrMuted = mupOtrMute mup,
        misOtrMutedStatus = mupOtrMuteStatus mup,
        misOtrMutedRef = mupOtrMuteRef mup,
        misOtrArchived = mupOtrArchive mup,
        misOtrArchivedRef = mupOtrArchiveRef mup,
        misHidden = mupHidden mup,
        misHiddenRef = mupHiddenRef mup,
        misConvRoleName = mupConvRoleName mup
      }

removeMembers :: MonadClient m => Conversation -> UserId -> List1 (MappedOrLocalId Id.U) -> m Event
removeMembers conv orig victims = do
  t <- liftIO getCurrentTime
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency Quorum
    for_ (toList victims) $ \u -> do
      addPrepQuery Cql.removeMember (convId conv, opaqueIdFromMappedOrLocal u)
      case u of
        Local userLocalId ->
          addPrepQuery Cql.deleteUserConv (userLocalId, makeIdOpaque (convId conv))
        Mapped _ ->
          -- the user's conversation has to be deleted on their own backend
          pure ()
  return $ Event MemberLeave (convId conv) orig t (Just (EdMembersLeave leavingMembers))
  where
    -- FUTUREWORK(federation, #1274): We need to tell clients about remote members leaving, too.
    leavingMembers = UserIdList . mapMaybe localIdOrNothing . toList $ victims
    localIdOrNothing = \case
      Local localId -> Just localId
      Mapped _ -> Nothing

removeMember :: MonadClient m => MappedOrLocalId Id.U -> ConvId -> m ()
removeMember usr cnv = retry x5 . batch $ do
  setType BatchLogged
  setConsistency Quorum
  addPrepQuery Cql.removeMember (cnv, opaqueIdFromMappedOrLocal usr)
  case usr of
    Local userLocalId ->
      addPrepQuery Cql.deleteUserConv (userLocalId, makeIdOpaque cnv)
    Mapped _ ->
      -- the user's conversation has to be deleted on their own backend
      pure ()

newMember :: a -> InternalMember a
newMember = flip newMemberWithRole roleNameWireAdmin

newMemberWithRole :: a -> RoleName -> InternalMember a
newMemberWithRole u r =
  Member
    { memId = u,
      memService = Nothing,
      memOtrMuted = False,
      memOtrMutedStatus = Nothing,
      memOtrMutedRef = Nothing,
      memOtrArchived = False,
      memOtrArchivedRef = Nothing,
      memHidden = False,
      memHiddenRef = Nothing,
      memConvRoleName = r
    }

toMember ::
  (Log.MonadLogger m, MonadThrow m) =>
  ( OpaqueUserId,
    Maybe RemoteUserId,
    Maybe Domain,
    Maybe ServiceId,
    Maybe ProviderId,
    Maybe Cql.MemberStatus,
    -- otr muted
    Maybe Bool,
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
  m (Maybe Member)
toMember (usr, usrRemoteId, usrRemoteDomain, srv, prv, sta, omu, omus, omur, oar, oarr, hid, hidr, crn) =
  toMappedOrLocalId (usr, usrRemoteId, usrRemoteDomain) <&> \memberId ->
    if sta /= Just 0
      then Nothing
      else
        Just $
          Member
            { memId = memberId,
              memService = newServiceRef <$> srv <*> prv,
              memOtrMuted = fromMaybe False omu,
              memOtrMutedStatus = omus,
              memOtrMutedRef = omur,
              memOtrArchived = fromMaybe False oar,
              memOtrArchivedRef = oarr,
              memHidden = fromMaybe False hid,
              memHiddenRef = hidr,
              memConvRoleName = fromMaybe roleNameWireAdmin crn
            }

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
  Clients.fromList . fmap (first makeIdOpaque) . concat . concat
    <$> forM (chunksOf 2048 users) (mapConcurrently getClients . chunksOf 128)
  where
    getClients us =
      map (second fromSet)
        <$> retry x1 (query Cql.selectClients (params Quorum (Identity us)))

eraseClients :: MonadClient m => UserId -> m ()
eraseClients user = retry x5 (write Cql.rmClients (params Quorum (Identity user)))

-- Internal utilities
newTeamMember' :: (MonadThrow m, MonadClient m) => (UserId, Permissions, Maybe UserId, Maybe UTCTimeMillis, Maybe UserLegalHoldStatus) -> m TeamMember
newTeamMember' (uid, perms, minvu, minvt, mlhStatus) = newTeamMemberRaw uid perms minvu minvt (fromMaybe UserLegalHoldDisabled mlhStatus)

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
      tMembers <- mapM newTeamMember' (result mems)
      action tMembers
      unless (null $ result mems) $
        handleMembers =<< liftClient (nextPage mems)
{-# INLINE withTeamMembersWithChunks #-}
