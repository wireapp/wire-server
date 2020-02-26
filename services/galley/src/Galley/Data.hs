{-# LANGUAGE RecordWildCards #-}

module Galley.Data
  ( ResultSet (..),
    schemaVersion,

    -- * Teams
    addTeamMember,
    updateTeamMember,
    createTeam,
    removeTeamMember,
    team,
    Galley.Data.teamName,
    teamConversation,
    teamConversations,
    teamIdsFrom,
    teamIdsOf,
    teamMember,
    teamMembers,
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
import Control.Monad.Catch (MonadThrow)
import Data.ByteString.Conversion hiding (parser)
import Data.Id
import Data.Json.Util (UTCTimeMillis (..))
import Data.LegalHold (UserLegalHoldStatus (..))
import qualified Data.List.Extra as List
import Data.List.Split (chunksOf)
import Data.List1 (List1, list1, singleton)
import qualified Data.Map.Strict as Map
import Data.Misc (Milliseconds)
import Data.Range
import qualified Data.Set
import Data.Time.Clock
import qualified Data.UUID.Tagged as U
import Data.UUID.V4 (nextRandom)
import Galley.App
import Galley.Data.Instances ()
import qualified Galley.Data.Queries as Cql
import Galley.Data.Types
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
import System.Logger.Message ((+++), msg, val)
import UnliftIO (async, mapConcurrently, wait)

-- We use this newtype to highlight the fact that the 'Page' wrapped in here
-- can not reliably used for paging.
--
-- The reason for this is that Cassandra returns 'hasMore' as true if the
-- page size requested is equal to result size. To work around this we
-- actually request for one additional element and drop the last value if
-- necessary. This means however that 'nextPage' does not work properly as
-- we would miss a value on every page size.
newtype ResultSet a = ResultSet {page :: Page a}

schemaVersion :: Int32
schemaVersion = 38

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
  ResultSet . fmap runIdentity . strip <$> case range of
    Just c -> paginate Cql.selectUserTeamsFrom (paramsP Quorum (usr, c) (max + 1))
    Nothing -> paginate Cql.selectUserTeams (paramsP Quorum (Identity usr) (max + 1))
  where
    strip p = p {result = take (fromIntegral max) (result p)}

teamConversation :: MonadClient m => TeamId -> ConvId -> m (Maybe TeamConversation)
teamConversation t c =
  fmap (newTeamConversation c . runIdentity)
    <$> retry x1 (query1 Cql.selectTeamConv (params Quorum (t, c)))

teamConversations :: MonadClient m => TeamId -> m [TeamConversation]
teamConversations t =
  map (uncurry newTeamConversation)
    <$> retry x1 (query Cql.selectTeamConvs (params Quorum (Identity t)))

teamMembers :: forall m. (MonadThrow m, MonadClient m) => TeamId -> m [TeamMember]
teamMembers t =
  mapM newTeamMember'
    =<< retry x1 (query Cql.selectTeamMembers (params Quorum (Identity t)))
  where
    newTeamMember' ::
      (UserId, Permissions, Maybe UserId, Maybe UTCTimeMillis, Maybe UserLegalHoldStatus) ->
      m TeamMember
    newTeamMember' (uid, perms, minvu, minvt, mlhStatus) =
      newTeamMemberRaw uid perms minvu minvt (fromMaybe UserLegalHoldDisabled mlhStatus)

-- Lookup only specific team members: this is particularly useful for large teams when
-- needed to look up only a small subset of members (typically 2, user to perform the action
-- and the target user)
teamMembersLimited :: forall m. (MonadThrow m, MonadClient m) => TeamId -> [UserId] -> m [TeamMember]
teamMembersLimited t u =
  mapM newTeamMember'
    =<< retry x1 (query Cql.selectTeamMembers' (params Quorum (t, u)))
  where
    newTeamMember' ::
      (UserId, Permissions, Maybe UserId, Maybe UTCTimeMillis, Maybe UserLegalHoldStatus) ->
      m TeamMember
    newTeamMember' (uid, perms, minvu, minvt, mlhStatus) =
      newTeamMemberRaw uid perms minvu minvt (fromMaybe UserLegalHoldDisabled mlhStatus)

teamMember :: forall m. (MonadThrow m, MonadClient m) => TeamId -> UserId -> m (Maybe TeamMember)
teamMember t u = newTeamMember' u =<< retry x1 (query1 Cql.selectTeamMember (params Quorum (t, u)))
  where
    newTeamMember' ::
      UserId ->
      Maybe (Permissions, Maybe UserId, Maybe UTCTimeMillis, Maybe UserLegalHoldStatus) ->
      m (Maybe TeamMember)
    newTeamMember' _ Nothing = pure Nothing
    newTeamMember' uid (Just (perms, minvu, minvt, mulhStatus)) =
      Just <$> newTeamMemberRaw uid perms minvu minvt (fromMaybe UserLegalHoldDisabled mulhStatus)

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
  checkBinding . fmap runIdentity
    <$> retry x1 (query1 Cql.selectTeamBinding (params Quorum (Identity t)))
  where
    checkBinding :: Maybe (Maybe TeamBinding) -> Maybe TeamBinding
    checkBinding (Just (Just Binding)) = Just Binding
    checkBinding (Just _) = Just NonBinding
    checkBinding Nothing = Nothing

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

deleteTeam :: MonadClient m => TeamId -> m ()
deleteTeam tid = do
  retry x5 $ write Cql.markTeamDeleted (params Quorum (PendingDelete, tid))
  mm <- teamMembers tid
  for_ mm $ removeTeamMember tid . view userId
  cc <- teamConversations tid
  for_ cc $ removeTeamConv tid . view conversationId
  retry x5 $ write Cql.deleteTeam (params Quorum (Deleted, tid))

-- TODO: delete service_whitelist records that mention this team

addTeamMember :: MonadClient m => TeamId -> TeamMember -> m ()
addTeamMember t m =
  retry x5 $ batch $ do
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

updateTeamMember :: MonadClient m => TeamId -> UserId -> Permissions -> m ()
updateTeamMember t u p = retry x5 $ write Cql.updatePermissions (params Quorum (p, t, u))

removeTeamMember :: MonadClient m => TeamId -> UserId -> m ()
removeTeamMember t m =
  retry x5 $ batch $ do
    setType BatchLogged
    setConsistency Quorum
    addPrepQuery Cql.deleteTeamMember (t, m)
    addPrepQuery Cql.deleteUserTeam (m, t)

removeTeamConv :: MonadClient m => TeamId -> ConvId -> m ()
removeTeamConv tid cid = do
  retry x5 $ batch $ do
    setType BatchLogged
    setConsistency Quorum
    addPrepQuery Cql.markConvDeleted (Identity cid)
    addPrepQuery Cql.deleteTeamConv (tid, cid)
  deleteConversation cid

updateTeamStatus :: MonadClient m => TeamId -> TeamStatus -> m ()
updateTeamStatus t s = retry x5 $ write Cql.updateTeamStatus (params Quorum (s, t))

updateTeam :: MonadClient m => TeamId -> TeamUpdateData -> m ()
updateTeam tid u = retry x5 $ batch $ do
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
  (MonadUnliftIO m, MonadClient m) =>
  ConvId ->
  m (Maybe Conversation)
conversation conv = do
  cdata <- async $ retry x1 (query1 Cql.selectConv (params Quorum (Identity conv)))
  mbConv <- toConv conv <$> members conv <*> wait cdata
  return mbConv >>= conversationGC

{- "Garbage collect" the conversation, i.e. the conversation may be
   marked as deleted, in which case we delete it and return Nothing -}
conversationGC :: MonadClient m => (Maybe Conversation) -> m (Maybe Conversation)
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
        Log.warn $ msg (val "No conversation for: " +++ toByteString i)
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

conversationIdsFrom :: MonadClient m => UserId -> Maybe ConvId -> Range 1 1000 Int32 -> m (ResultSet ConvId)
conversationIdsFrom usr range (fromRange -> max) =
  ResultSet . fmap runIdentity . strip <$> case range of
    Just c -> paginate Cql.selectUserConvsFrom (paramsP Quorum (usr, c) (max + 1))
    Nothing -> paginate Cql.selectUserConvs (paramsP Quorum (Identity usr) (max + 1))
  where
    strip p = p {result = take (fromIntegral max) (result p)}

conversationIdsOf :: MonadClient m => UserId -> Range 1 32 (List ConvId) -> m [ConvId]
conversationIdsOf usr (fromList . fromRange -> cids) =
  map runIdentity <$> retry x1 (query Cql.selectUserConvsIn (params Quorum (usr, cids)))

createConversation ::
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
  Galley Conversation
createConversation usr name acc role others tinfo mtimer recpt othersConversationRole = do
  conv <- Id <$> liftIO nextRandom
  now <- liftIO getCurrentTime
  retry x5 $ case tinfo of
    Nothing -> write Cql.insertConv (params Quorum (conv, RegularConv, usr, Set (toList acc), role, fromRange <$> name, Nothing, mtimer, recpt))
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
  U.UUID U.V4 ->
  U.UUID U.V4 ->
  Maybe (Range 1 256 Text) ->
  Maybe TeamId ->
  Galley Conversation
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

updateConversationAccess :: MonadClient m => ConvId -> Data.Set.Set Access -> AccessRole -> m ()
updateConversationAccess cid acc role = retry x5 $ write Cql.updateConvAccess (params Quorum (Set (toList acc), role, cid))

updateConversationReceiptMode :: MonadClient m => ConvId -> ReceiptMode -> m ()
updateConversationReceiptMode cid receiptMode = retry x5 $ write Cql.updateConvReceiptMode (params Quorum (receiptMode, cid))

lookupReceiptMode :: MonadClient m => ConvId -> m (Maybe ReceiptMode)
lookupReceiptMode cid = join . fmap runIdentity <$> retry x1 (query1 Cql.selectReceiptMode (params Quorum (Identity cid)))

updateConversationMessageTimer :: MonadClient m => ConvId -> Maybe Milliseconds -> m ()
updateConversationMessageTimer cid mtimer = retry x5 $ write Cql.updateConvMessageTimer (params Quorum (mtimer, cid))

deleteConversation :: MonadClient m => ConvId -> m ()
deleteConversation cid = do
  retry x5 $ write Cql.markConvDeleted (params Quorum (Identity cid))
  mm <- members cid
  for_ mm $ \m -> removeMember (memId m) cid
  retry x5 $ write Cql.deleteConv (params Quorum (Identity cid))

acceptConnect :: MonadClient m => ConvId -> m ()
acceptConnect cid = retry x5 $ write Cql.updateConvType (params Quorum (One2OneConv, cid))

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

-- Conversation Members -----------------------------------------------------

member :: MonadClient m => ConvId -> UserId -> m (Maybe Member)
member cnv usr = (toMember =<<) <$> retry x1 (query1 Cql.selectMember (params Quorum (cnv, usr)))

memberLists :: MonadClient m => [ConvId] -> m [[Member]]
memberLists convs = do
  mems <- retry x1 $ query Cql.selectMembers (params Quorum (Identity convs))
  let m = foldr (insert . mkMem) Map.empty mems
  return $ map (\i -> fromMaybe [] (Map.lookup i m)) convs
  where
    insert Nothing acc = acc
    insert (Just (conv, mem)) acc =
      let f = (Just . maybe [mem] (mem :))
       in Map.alter f conv acc
    mkMem (cnv, usr, srv, prv, st, omu, omus, omur, oar, oarr, hid, hidr, crn) =
      (cnv,) <$> toMember (usr, srv, prv, st, omu, omus, omur, oar, oarr, hid, hidr, crn)

members :: MonadClient m => ConvId -> m [Member]
members conv = join <$> memberLists [conv]

addMember :: MonadClient m => UTCTime -> ConvId -> UserId -> m (Event, List1 Member)
addMember t c u = addMembersUnchecked t c u (singleton u)

addMembersWithRole :: MonadClient m => UTCTime -> ConvId -> (UserId, RoleName) -> ConvMemberAddSizeChecked (List1 (UserId, RoleName)) -> m (Event, List1 Member)
addMembersWithRole t c orig mems = addMembersUncheckedWithRole t c orig (fromMemberSize mems)

addMembersUnchecked :: MonadClient m => UTCTime -> ConvId -> UserId -> List1 UserId -> m (Event, List1 Member)
addMembersUnchecked t conv orig usrs = addMembersUncheckedWithRole t conv (orig, roleNameWireAdmin) ((,roleNameWireAdmin) <$> usrs)

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
    retry x5 $ batch $ do
      setType BatchLogged
      setConsistency Quorum
      for_ chunk $ \(u, r) -> do
        addPrepQuery Cql.insertUserConv (u, conv)
        addPrepQuery Cql.insertMember (conv, u, Nothing, Nothing, r)
  let e = Event MemberJoin conv orig t (Just . EdMembersJoin . SimpleMembers . toSimpleMembers $ toList usrs)
  return (e, fmap (uncurry newMemberWithRole) usrs)
  where
    toSimpleMembers :: [(UserId, RoleName)] -> [SimpleMember]
    toSimpleMembers = fmap (uncurry SimpleMember)

updateMember :: MonadClient m => ConvId -> UserId -> MemberUpdate -> m MemberUpdateData
updateMember cid uid mup = do
  retry x5 $ batch $ do
    setType BatchUnLogged
    setConsistency Quorum
    for_ (mupOtrMute mup) $ \m ->
      addPrepQuery Cql.updateOtrMemberMuted (m, mupOtrMuteRef mup, cid, uid)
    for_ (mupOtrMuteStatus mup) $ \ms ->
      addPrepQuery Cql.updateOtrMemberMutedStatus (ms, mupOtrMuteRef mup, cid, uid)
    for_ (mupOtrArchive mup) $ \a ->
      addPrepQuery Cql.updateOtrMemberArchived (a, mupOtrArchiveRef mup, cid, uid)
    for_ (mupHidden mup) $ \h ->
      addPrepQuery Cql.updateMemberHidden (h, mupHiddenRef mup, cid, uid)
    for_ (mupConvRoleName mup) $ \r ->
      addPrepQuery Cql.updateMemberConvRoleName (r, cid, uid)
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

removeMembers :: MonadClient m => Conversation -> UserId -> List1 UserId -> m Event
removeMembers conv orig victims = do
  t <- liftIO getCurrentTime
  retry x5 $ batch $ do
    setType BatchLogged
    setConsistency Quorum
    for_ (toList victims) $ \u -> do
      addPrepQuery Cql.removeMember (convId conv, u)
      addPrepQuery Cql.deleteUserConv (u, convId conv)
  return $ Event MemberLeave (convId conv) orig t (Just . EdMembersLeave . UserIdList . toList $ victims)

removeMember :: MonadClient m => UserId -> ConvId -> m ()
removeMember usr cnv = retry x5 $ batch $ do
  setType BatchLogged
  setConsistency Quorum
  addPrepQuery Cql.removeMember (cnv, usr)
  addPrepQuery Cql.deleteUserConv (usr, cnv)

newMember :: UserId -> Member
newMember = flip newMemberWithRole roleNameWireAdmin

newMemberWithRole :: UserId -> RoleName -> Member
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
  ( UserId,
    Maybe ServiceId,
    Maybe ProviderId,
    Maybe Cql.MemberStatus,
    Maybe Bool,
    Maybe MutedStatus,
    Maybe Text, -- otr muted
    Maybe Bool,
    Maybe Text, -- otr archived
    Maybe Bool,
    Maybe Text, -- hidden
    Maybe RoleName -- conversation role name
  ) ->
  Maybe Member
toMember (usr, srv, prv, sta, omu, omus, omur, oar, oarr, hid, hidr, crn) =
  if sta /= Just 0
    then Nothing
    else
      Just $
        Member
          { memId = usr,
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
  Clients.fromList . concat . concat
    <$> forM (chunksOf 2048 users) (mapConcurrently getClients . chunksOf 128)
  where
    getClients us =
      map (second fromSet)
        <$> retry x1 (query Cql.selectClients (params Quorum (Identity us)))

eraseClients :: MonadClient m => UserId -> m ()
eraseClients user = retry x5 (write Cql.rmClients (params Quorum (Identity user)))
