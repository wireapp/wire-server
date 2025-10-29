-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.ConversationStore.Cassandra
  ( interpretMLSCommitLockStoreToCassandra,
    interpretConversationStoreToCassandra,
    interpretConversationStoreToCassandraAndPostgres,
    MigrationError (..),
  )
where

import Cassandra
import Cassandra qualified as Cql
import Cassandra.Settings
import Cassandra.Util
import Control.Arrow
import Control.Error.Util hiding (hoistMaybe)
import Control.Lens
import Control.Monad.Trans.Maybe
import Data.ByteString.Conversion
import Data.Domain
import Data.Id
import Data.List.Extra qualified as List
import Data.Map qualified as Map
import Data.Misc
import Data.Monoid
import Data.Qualified
import Data.Range
import Data.Set qualified as Set
import Data.Time
import Data.UUID.Util qualified as UUID
import Imports
import Network.HTTP.Types.Status (status500)
import Network.Wai.Utilities.Error qualified as WaiError
import Network.Wai.Utilities.JSONResponse
import Polysemy
import Polysemy.Async (Async)
import Polysemy.Conc
import Polysemy.Embed
import Polysemy.Error (Error, mapError, throw)
import Polysemy.Input
import Polysemy.Time
import Polysemy.TinyLog
import System.Logger qualified as Log
import UnliftIO qualified
import Wire.API.Conversation hiding (Conversation, Member, members, newGroupId)
import Wire.API.Conversation.CellsState
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role hiding (DeleteConversation)
import Wire.API.Error
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Credential
import Wire.API.MLS.Group.Serialisation
import Wire.API.MLS.GroupInfo
import Wire.API.MLS.LeafNode (LeafIndex)
import Wire.API.MLS.SubConversation
import Wire.API.Provider.Service
import Wire.ConversationStore (ConversationStore (..), LockAcquired (..), MLSCommitLockStore (..))
import Wire.ConversationStore qualified as ConvStore
import Wire.ConversationStore.Cassandra.Instances ()
import Wire.ConversationStore.Cassandra.Queries qualified as Cql
import Wire.ConversationStore.Cassandra.Queries qualified as Queries
import Wire.ConversationStore.MLS.Types
import Wire.ConversationStore.Migration.Cleanup
import Wire.ConversationStore.MigrationLock
import Wire.ConversationStore.Postgres (interpretConversationStoreToPostgres)
import Wire.Postgres
import Wire.Sem.Paging.Cassandra
import Wire.StoredConversation
import Wire.StoredConversation qualified as StoreConv
import Wire.UserList
import Wire.Util

-----------------------------------------------------------------------------------------
-- CONVERSATION STORE

createConversation :: Local ConvId -> NewConversation -> Client StoredConversation
createConversation lcnv nc = do
  let storedConv = newStoredConversation lcnv nc
      meta = storedConv.metadata
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency LocalQuorum
    addPrepQuery
      Cql.insertConv
      ( storedConv.id_,
        meta.cnvmType,
        meta.cnvmCreator,
        Cql.Set meta.cnvmAccess,
        Cql.Set (toList meta.cnvmAccessRoles),
        meta.cnvmName,
        meta.cnvmTeam,
        meta.cnvmMessageTimer,
        meta.cnvmReceiptMode,
        protocolTag storedConv.protocol,
        getGroupId storedConv.protocol,
        meta.cnvmGroupConvType,
        meta.cnvmChannelAddPermission,
        meta.cnvmCellsState,
        meta.cnvmParent
      )
    for_ (cnvmTeam meta) $ \tid -> addPrepQuery Cql.insertTeamConv (tid, storedConv.id_)
  let localUsers = map (\m -> (m.id_, m.convRoleName)) storedConv.localMembers
      remoteUsers = map (\m -> (,m.convRoleName) <$> m.id_) storedConv.remoteMembers
  void $ addMembers storedConv.id_ $ UserList localUsers remoteUsers
  pure storedConv

deleteConversation :: ConvId -> Client ()
deleteConversation cid = do
  retry x5 $ write Cql.markConvDeleted (params LocalQuorum (Identity cid))

  localMembers <- members cid
  remoteMembers <- lookupRemoteMembers cid

  removeMembersFromLocalConv cid $
    UserList ((.id_) <$> localMembers) ((.id_) <$> remoteMembers)

  retry x5 $ write Cql.deleteConv (params LocalQuorum (Identity cid))

conversationMeta :: ConvId -> Client (Maybe ConversationMetadata)
conversationMeta conv =
  fmap (toConvMeta . snd . toStoredConvRow)
    <$> retry x1 (query1 Cql.selectConv (params LocalQuorum (Identity conv)))

parseAccessRoles :: Maybe AccessRoleLegacy -> Maybe (Imports.Set AccessRole) -> Maybe (Imports.Set AccessRole)
parseAccessRoles mbLegacy mbAccess = mbAccess <|> fromAccessRoleLegacy <$> mbLegacy

toStoredConvRow :: Queries.ConvRow -> (Maybe Bool, StoreConv.ConvRow)
toStoredConvRow (cty, muid, acc, role, roleV2, nme, ti, del, timer, rm, ptag, mgid, mep, mts, mcs, mgct, mAp, mcells, mparent) =
  ( del,
    ( cty,
      muid,
      Cql.fromSet <$> acc,
      parseAccessRoles role (Set.fromList . Cql.fromSet <$> roleV2),
      nme,
      ti,
      timer,
      rm,
      ptag,
      mgid,
      mep,
      writetimeToUTC <$> mts,
      mcs,
      mgct,
      mAp,
      mcells,
      mparent
    )
  )

getGroupInfo :: ConvId -> Client (Maybe GroupInfoData)
getGroupInfo cid = do
  (runIdentity =<<)
    <$> retry
      x1
      ( query1
          Cql.selectGroupInfo
          (params LocalQuorum (Identity cid))
      )

isConvAlive :: ConvId -> Client Bool
isConvAlive cid = do
  result <- retry x1 (query1 Cql.isConvDeleted (params LocalQuorum (Identity cid)))
  case runIdentity <$> result of
    Nothing -> pure False
    Just Nothing -> pure True
    Just (Just True) -> pure False
    Just (Just False) -> pure True

updateConvType :: ConvId -> ConvType -> Client ()
updateConvType cid ty =
  retry x5 $
    write Cql.updateConvType (params LocalQuorum (ty, cid))

updateConvName :: ConvId -> Range 1 256 Text -> Client ()
updateConvName cid name = retry x5 $ write Cql.updateConvName (params LocalQuorum (fromRange name, cid))

updateConvAccess :: ConvId -> ConversationAccessData -> Client ()
updateConvAccess cid (ConversationAccessData acc role) =
  retry x5 $
    write Cql.updateConvAccess (params LocalQuorum (Cql.Set (toList acc), Cql.Set (toList role), cid))

updateConvReceiptMode :: ConvId -> ReceiptMode -> Client ()
updateConvReceiptMode cid receiptMode = retry x5 $ write Cql.updateConvReceiptMode (params LocalQuorum (receiptMode, cid))

updateConvMessageTimer :: ConvId -> Maybe Milliseconds -> Client ()
updateConvMessageTimer cid mtimer = retry x5 $ write Cql.updateConvMessageTimer (params LocalQuorum (mtimer, cid))

getConvEpoch :: ConvId -> Client (Maybe Epoch)
getConvEpoch cid =
  (runIdentity =<<)
    <$> retry
      x1
      (query1 Cql.getConvEpoch (params LocalQuorum (Identity cid)))

updateConvEpoch :: ConvId -> Epoch -> Client ()
updateConvEpoch cid epoch = retry x5 $ write Cql.updateConvEpoch (params LocalQuorum (epoch, cid))

updateConvCipherSuite :: ConvId -> CipherSuiteTag -> Client ()
updateConvCipherSuite cid cs =
  retry x5 $
    write
      Cql.updateConvCipherSuite
      (params LocalQuorum (cs, cid))

updateConvCellsState :: ConvId -> CellsState -> Client ()
updateConvCellsState cid ps =
  retry x5 $
    write
      Cql.updateConvCellsState
      (params LocalQuorum (ps, cid))

resetConversation :: ConvId -> GroupId -> Client ()
resetConversation cid groupId =
  retry x5 $
    write
      Cql.resetConversation
      (params LocalQuorum (groupId, cid))

setGroupInfo :: ConvId -> GroupInfoData -> Client ()
setGroupInfo conv gid =
  write Cql.updateGroupInfo (params LocalQuorum (gid, conv))

getConversation :: ConvId -> Client (Maybe StoredConversation)
getConversation conv = do
  cdata <- UnliftIO.async $ retry x1 (query1 Cql.selectConv (params LocalQuorum (Identity conv)))
  remoteMems <- UnliftIO.async $ lookupRemoteMembers conv
  mConvRow <- UnliftIO.wait (toStoredConvRow <$$> cdata)
  case mConvRow of
    Nothing -> pure Nothing
    Just (Just True, _) -> do
      deleteConversation conv
      pure Nothing
    Just (_, convRow) -> do
      toConv conv
        <$> members conv
        <*> UnliftIO.wait remoteMems
        <*> pure (Just convRow)

localConversations ::
  ( Member (Embed IO) r,
    Member TinyLog r
  ) =>
  ClientState ->
  [ConvId] ->
  Sem r [StoredConversation]
localConversations client =
  collectAndLog
    <=< ( runEmbedded (runClient client) . embed . UnliftIO.pooledMapConcurrentlyN 8 localConversation'
        )
  where
    collectAndLog cs = case partitionEithers cs of
      (errs, convs) -> traverse_ (warn . Log.msg) errs $> convs

    localConversation' :: ConvId -> Client (Either ByteString StoredConversation)
    localConversation' cid =
      note ("No conversation for: " <> toByteString' cid) <$> getConversation cid

-- | Takes a list of conversation ids and returns those found for the given
-- user.
localConversationIdsOf :: UserId -> [ConvId] -> Client [ConvId]
localConversationIdsOf usr cids = do
  runIdentity <$$> retry x1 (query Cql.selectUserConvsIn (params LocalQuorum (usr, cids)))

getLocalConvIds :: UserId -> Maybe ConvId -> Range 1 1000 Int32 -> Client (ResultSet ConvId)
getLocalConvIds usr start (fromRange -> maxIds) = do
  mkResultSetByLength (fromIntegral maxIds) . fmap runIdentity . result <$> case start of
    Just c -> paginate Cql.selectUserConvsFrom (paramsP LocalQuorum (usr, c) (maxIds + 1))
    Nothing -> paginate Cql.selectUserConvs (paramsP LocalQuorum (Identity usr) (maxIds + 1))

getRemoteConvIds :: UserId -> Maybe (Remote ConvId) -> Range 1 1000 Int32 -> Client (ResultSet (Remote ConvId))
getRemoteConvIds usr start (fromRange -> maxIds) = do
  mkResultSetByLength (fromIntegral maxIds) . fmap (uncurry toRemoteUnsafe) . result <$> case start of
    Just (tUntagged -> Qualified c dom) -> paginate Cql.selectUserRemoteConvsFrom (paramsP LocalQuorum (usr, dom, c) (maxIds + 1))
    Nothing -> paginate Cql.selectUserRemoteConvs (paramsP LocalQuorum (Identity usr) (maxIds + 1))

-- | Takes a list of remote conversation ids and fetches member status flags
-- for the given user
remoteConversationStatus ::
  UserId ->
  [Remote ConvId] ->
  Client (Map (Remote ConvId) MemberStatus)
remoteConversationStatus uid =
  fmap mconcat
    . UnliftIO.pooledMapConcurrentlyN 8 (remoteConversationStatusOnDomain uid)
    . bucketRemote

remoteConversationStatusOnDomain :: UserId -> Remote [ConvId] -> Client (Map (Remote ConvId) MemberStatus)
remoteConversationStatusOnDomain uid rconvs =
  Map.fromList . map toPair
    <$> query Cql.selectRemoteConvMemberStatuses (params LocalQuorum (uid, tDomain rconvs, tUnqualified rconvs))
  where
    toPair (conv, omus, omur, oar, oarr, hid, hidr) =
      ( qualifyAs rconvs conv,
        toMemberStatus (omus, omur, oar, oarr, hid, hidr)
      )

updateToMixedProtocol ::
  (Member (Embed IO) r) =>
  ClientState ->
  ConvId ->
  GroupId ->
  Epoch ->
  Sem r ()
updateToMixedProtocol client convId gid epoch = do
  runEmbedded (runClient client) . embed . retry x5 . batch $ do
    setType BatchLogged
    setConsistency LocalQuorum
    addPrepQuery Cql.updateToMixedConv (convId, ProtocolMixedTag, gid, epoch)
  pure ()

updateToMLSProtocol ::
  (Member (Embed IO) r) =>
  ClientState ->
  ConvId ->
  Sem r ()
updateToMLSProtocol client cnv =
  runEmbedded (runClient client) . embed . retry x5 $
    write Cql.updateToMLSConv (params LocalQuorum (cnv, ProtocolMLSTag))

updateChannelAddPermissions :: ConvId -> AddPermission -> Client ()
updateChannelAddPermissions cid cap = retry x5 $ write Cql.updateChannelAddPermission (params LocalQuorum (cap, cid))

acquireCommitLock :: GroupId -> Epoch -> NominalDiffTime -> Client LockAcquired
acquireCommitLock groupId epoch ttl = do
  rows <-
    retry x5 $
      trans
        Cql.acquireCommitLock
        ( params
            LocalQuorum
            (groupId, epoch, round ttl)
        )
          { serialConsistency = Just LocalSerialConsistency
          }
  pure $
    if checkTransSuccess rows
      then Acquired
      else NotAcquired

releaseCommitLock :: GroupId -> Epoch -> Client ()
releaseCommitLock groupId epoch =
  retry x5 $
    write
      Cql.releaseCommitLock
      ( params
          LocalQuorum
          (groupId, epoch)
      )

checkTransSuccess :: [Row] -> Bool
checkTransSuccess [] = False
checkTransSuccess (row : _) = either (const False) (fromMaybe False) $ fromRow 0 row

removeTeamConv :: TeamId -> ConvId -> Client ()
removeTeamConv tid cid = liftClient $ do
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency LocalQuorum
    addPrepQuery Cql.markConvDeleted (Identity cid)
    addPrepQuery Cql.deleteTeamConv (tid, cid)
  deleteConversation cid

teamConversation :: TeamId -> ConvId -> Client (Maybe ConvId)
teamConversation t c =
  runIdentity <$$> retry x1 (query1 Cql.selectTeamConv (params LocalQuorum (t, c)))

getTeamConversations :: TeamId -> Client [ConvId]
getTeamConversations t =
  runIdentity <$$> retry x1 (query Cql.selectTeamConvs (params LocalQuorum (Identity t)))

deleteTeamConversations :: TeamId -> Client ()
deleteTeamConversations tid = do
  convs <- teamConversationsForPagination Nothing 2000
  remove convs
  where
    remove :: Page ConvId -> Client ()
    remove convs = do
      for_ (result convs) $ removeTeamConv tid
      unless (null $ result convs) $
        remove =<< nextPage convs

    teamConversationsForPagination ::
      Maybe ConvId ->
      Int32 ->
      Client (Page ConvId)
    teamConversationsForPagination start n =
      runIdentity <$$> case start of
        Just c -> paginate Cql.selectTeamConvsFrom (paramsP LocalQuorum (tid, c) n)
        Nothing -> paginate Cql.selectTeamConvs (paramsP LocalQuorum (Identity tid) n)

-----------------------------------------------------------------------------------------
-- MEMBERS STORE

-- | Add members to a local conversation.
-- Conversation is local, so we can add any member to it (including remote ones).
-- When the role is not specified, it defaults to admin.
-- Please make sure the conversation doesn't exceed the maximum size!
addMembers ::
  ConvId ->
  UserList (UserId, RoleName) ->
  Client ([LocalMember], [RemoteMember])
addMembers conv (UserList lusers rusers) = do
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
      setConsistency LocalQuorum
      for_ chunk $ \(u, r) -> do
        -- User is local, too, so we add it to both the member and the user table
        addPrepQuery Cql.insertMember (conv, u, Nothing, Nothing, r)
        addPrepQuery Cql.insertUserConv (u, conv)

  for_ (List.chunksOf 32 rusers) $ \chunk -> do
    retry x5 . batch $ do
      setType BatchLogged
      setConsistency LocalQuorum
      for_ chunk $ \(tUntagged -> Qualified (uid, role) domain) -> do
        -- User is remote, so we only add it to the member_remote_user
        -- table, but the reverse mapping has to be done on the remote
        -- backend; so we assume an additional call to their backend has
        -- been (or will be) made separately. See Galley.API.Update.addMembers
        addPrepQuery Cql.insertRemoteMember (conv, domain, uid, role)

  pure (map newMemberWithRole lusers, map newRemoteMemberWithRole rusers)

removeMembersFromLocalConv :: ConvId -> UserList UserId -> Client ()
removeMembersFromLocalConv cnv victims = void $ do
  UnliftIO.concurrently
    (removeLocalMembersFromLocalConv cnv (ulLocals victims))
    (removeRemoteMembersFromLocalConv cnv (ulRemotes victims))

removeLocalMembersFromLocalConv :: ConvId -> [UserId] -> Client ()
removeLocalMembersFromLocalConv _ [] = pure ()
removeLocalMembersFromLocalConv cnv victims = do
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency LocalQuorum
    for_ victims $ \victim -> do
      addPrepQuery Cql.removeMember (cnv, victim)
      addPrepQuery Cql.deleteUserConv (victim, cnv)

removeRemoteMembersFromLocalConv :: ConvId -> [Remote UserId] -> Client ()
removeRemoteMembersFromLocalConv _ [] = pure ()
removeRemoteMembersFromLocalConv cnv victims = do
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency LocalQuorum
    for_ victims $ \(tUntagged -> Qualified uid domain) ->
      addPrepQuery Cql.removeRemoteMember (cnv, domain, uid)

members :: ConvId -> Client [LocalMember]
members conv = do
  parents <- retry x1 $ query Cql.selectConvParent (params LocalQuorum (Identity conv))
  nubBy ((==) `on` (.id_)) . concatMap (mapMaybe toMember)
    <$> UnliftIO.pooledMapConcurrentlyN 16 fetchMembers (conv : mapMaybe runIdentity parents)
  where
    fetchMembers convId =
      retry x1 $
        query Cql.selectMembers (params LocalQuorum (Identity convId))

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
      { id_ = usr,
        service = newServiceRef <$> srv <*> prv,
        status = toMemberStatus (omus, omur, oar, oarr, hid, hidr),
        convRoleName = fromMaybe roleNameWireAdmin crn
      }
toMember _ = Nothing

lookupRemoteMember :: ConvId -> Domain -> UserId -> Client (Maybe RemoteMember)
lookupRemoteMember conv domain usr = do
  mkMem <$$> retry x1 (query1 Cql.selectRemoteMember (params LocalQuorum (conv, domain, usr)))
  where
    mkMem (Identity role) =
      RemoteMember
        { id_ = toRemoteUnsafe domain usr,
          convRoleName = role
        }

lookupRemoteMembers :: ConvId -> Client [RemoteMember]
lookupRemoteMembers conv = do
  fmap (map mkMem) . retry x1 $ query Cql.selectRemoteMembers (params LocalQuorum (Identity conv))
  where
    mkMem (domain, usr, role) =
      RemoteMember
        { id_ = toRemoteUnsafe domain usr,
          convRoleName = role
        }

member ::
  ConvId ->
  UserId ->
  Client (Maybe LocalMember)
member cnv usr = do
  parents <- retry x1 $ query Cql.selectConvParent (params LocalQuorum (Identity cnv))
  asum . map (toMember =<<)
    <$> UnliftIO.pooledMapConcurrentlyN 16 fetchMembers (cnv : mapMaybe runIdentity parents)
  where
    fetchMembers convId =
      retry x1 (query1 Cql.selectMember (params LocalQuorum (convId, usr)))

-- | Set local users as belonging to a remote conversation. This is invoked by a
-- remote galley when users from the current backend are added to conversations
-- on the remote end.
addLocalMembersToRemoteConv :: Remote ConvId -> [UserId] -> Client ()
addLocalMembersToRemoteConv _ [] = pure ()
addLocalMembersToRemoteConv rconv users = do
  -- FUTUREWORK: consider using pooledMapConcurrentlyN
  for_ (List.chunksOf 32 users) $ \chunk ->
    retry x5 . batch $ do
      setType BatchLogged
      setConsistency LocalQuorum
      for_ chunk $ \u ->
        addPrepQuery
          Cql.insertUserRemoteConv
          (u, tDomain rconv, tUnqualified rconv)

updateSelfMember ::
  Qualified ConvId ->
  Local UserId ->
  MemberUpdate ->
  Client ()
updateSelfMember qcnv lusr =
  foldQualified
    lusr
    updateSelfMemberLocalConv
    updateSelfMemberRemoteConv
    qcnv
    lusr

updateSelfMemberLocalConv ::
  Local ConvId ->
  Local UserId ->
  MemberUpdate ->
  Client ()
updateSelfMemberLocalConv lcid luid mup = do
  retry x5 . batch $ do
    setType BatchUnLogged
    setConsistency LocalQuorum
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
  Client ()
updateSelfMemberRemoteConv (tUntagged -> Qualified cid domain) luid mup = do
  retry x5 . batch $ do
    setType BatchUnLogged
    setConsistency LocalQuorum
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

updateOtherMemberLocalConv ::
  Local ConvId ->
  Qualified UserId ->
  OtherMemberUpdate ->
  Client ()
updateOtherMemberLocalConv lcid quid omu =
  do
    let add r
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
      setConsistency LocalQuorum
      traverse_ add (omuConvRoleName omu)

-- | Select only the members of a remote conversation from a list of users.
-- Return the filtered list and a boolean indicating whether the all the input
-- users are members.
filterRemoteConvMembers ::
  [UserId] ->
  Remote ConvId ->
  Client ([UserId], Bool)
filterRemoteConvMembers users (tUntagged -> Qualified conv dom) =
  fmap Data.Monoid.getAll
    . foldMap (\muser -> (muser, Data.Monoid.All (not (null muser))))
    <$> UnliftIO.pooledMapConcurrentlyN 8 filterMember users
  where
    filterMember :: UserId -> Client [UserId]
    filterMember user =
      fmap (map runIdentity)
        . retry x1
        $ query Cql.selectRemoteConvMembers (params LocalQuorum (user, dom, conv))

lookupLocalMemberRemoteConv ::
  UserId ->
  Remote ConvId ->
  Client (Maybe UserId)
lookupLocalMemberRemoteConv uid (tUntagged -> Qualified conv dom) =
  runIdentity
    <$$> retry
      x5
      (query1 Cql.selectRemoteConvMembers (params LocalQuorum (uid, dom, conv)))

haveRemoteConvs :: [UserId] -> Client [UserId]
haveRemoteConvs uids =
  catMaybes <$> UnliftIO.pooledMapConcurrentlyN 16 runSelect uids
  where
    selectUserFromRemoteConv :: PrepQuery R (Identity UserId) (Identity UserId)
    selectUserFromRemoteConv = "select user from user_remote_conv where user = ? limit 1"

    runSelect :: UserId -> Client (Maybe UserId)
    runSelect uid =
      runIdentity <$$> retry x5 (query1 selectUserFromRemoteConv (params LocalQuorum (Identity uid)))

removeLocalMembersFromRemoteConv ::
  -- | The conversation to remove members from
  Remote ConvId ->
  -- | Members to remove local to this backend
  [UserId] ->
  Client ()
removeLocalMembersFromRemoteConv _ [] = pure ()
removeLocalMembersFromRemoteConv (tUntagged -> Qualified conv convDomain) victims =
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency LocalQuorum
    for_ victims $ \u -> addPrepQuery Cql.deleteUserRemoteConv (u, convDomain, conv)

addMLSClients :: GroupId -> Qualified UserId -> Set.Set (ClientId, LeafIndex) -> Client ()
addMLSClients groupId (Qualified usr domain) cs = retry x5 . batch $ do
  setType BatchLogged
  setConsistency LocalQuorum
  for_ cs $ \(c, idx) ->
    addPrepQuery Cql.addMLSClient (groupId, domain, usr, c, fromIntegral idx)

planMLSClientRemoval :: (Foldable f) => GroupId -> f ClientIdentity -> Client ()
planMLSClientRemoval groupId cids =
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency LocalQuorum
    for_ cids $ \cid -> do
      addPrepQuery
        Cql.planMLSClientRemoval
        (groupId, ciDomain cid, ciUser cid, ciClient cid)

removeMLSClients :: GroupId -> Qualified UserId -> Set.Set ClientId -> Client ()
removeMLSClients groupId (Qualified usr domain) cs = retry x5 . batch $ do
  setType BatchLogged
  setConsistency LocalQuorum
  for_ cs $ \c ->
    addPrepQuery Cql.removeMLSClient (groupId, domain, usr, c)

removeAllMLSClients :: GroupId -> Client ()
removeAllMLSClients groupId = do
  retry x5 $ write Cql.removeAllMLSClients (params LocalQuorum (Identity groupId))

-- FUTUREWORK: support adding bots to a remote conversation
addBotMember :: ServiceRef -> BotId -> ConvId -> Client BotMember
addBotMember s bot cnv = do
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency LocalQuorum
    addPrepQuery Cql.insertUserConv (botUserId bot, cnv)
    addPrepQuery Cql.insertBot (cnv, bot, sid, pid)
  pure (BotMember mem)
  where
    pid = s ^. serviceRefProvider
    sid = s ^. serviceRefId
    mem = (newMember (botUserId bot)) {service = Just s}

lookupMLSClientLeafIndices :: GroupId -> Client (ClientMap LeafIndex, IndexMap)
lookupMLSClientLeafIndices groupId = do
  entries <- retry x5 (query Cql.lookupMLSClients (params LocalQuorum (Identity groupId)))
  pure $ (mkClientMap &&& mkIndexMap) entries

lookupMLSClients :: GroupId -> Client (ClientMap LeafIndex)
lookupMLSClients = fmap fst . lookupMLSClientLeafIndices

-----------------------------------------------------------------------------------------
-- SUB CONVERSATION STORE

selectSubConversation :: ConvId -> SubConvId -> Client (Maybe SubConversation)
selectSubConversation convId subConvId = runMaybeT $ do
  (mSuite, mEpoch, mEpochWritetime, mGroupId) <-
    MaybeT $
      retry x5 (query1 Cql.selectSubConversation (params LocalQuorum (convId, subConvId)))
  let activeData =
        ActiveMLSConversationData
          <$> mEpoch
          <*> fmap writetimeToUTC mEpochWritetime
          <*> mSuite
  groupId <- hoistMaybe mGroupId
  (cm, im) <- lift $ lookupMLSClientLeafIndices groupId
  pure $
    SubConversation
      { scParentConvId = convId,
        scSubConvId = subConvId,
        scMLSData =
          ConversationMLSData
            { cnvmlsGroupId = groupId,
              cnvmlsActiveData = activeData
            },
        scMembers = cm,
        scIndexMap = im
      }

insertSubConversation ::
  ConvId ->
  SubConvId ->
  GroupId ->
  Client SubConversation
insertSubConversation convId subConvId groupId = do
  retry
    x5
    ( write
        Cql.insertSubConversation
        ( params
            LocalQuorum
            (convId, subConvId, Epoch 0, groupId, Nothing)
        )
    )
  pure (newSubConversation convId subConvId groupId)

updateSubConvGroupInfo :: ConvId -> SubConvId -> Maybe GroupInfoData -> Client ()
updateSubConvGroupInfo convId subConvId mGroupInfo =
  retry x5 (write Cql.updateSubConvGroupInfo (params LocalQuorum (convId, subConvId, mGroupInfo)))

selectSubConvGroupInfo :: ConvId -> SubConvId -> Client (Maybe GroupInfoData)
selectSubConvGroupInfo convId subConvId =
  (runIdentity =<<) <$> retry x5 (query1 Cql.selectSubConvGroupInfo (params LocalQuorum (convId, subConvId)))

selectSubConvEpoch :: ConvId -> SubConvId -> Client (Maybe Epoch)
selectSubConvEpoch convId subConvId =
  (runIdentity =<<) <$> retry x5 (query1 Cql.selectSubConvEpoch (params LocalQuorum (convId, subConvId)))

setEpochForSubConversation :: ConvId -> SubConvId -> Epoch -> Client ()
setEpochForSubConversation cid sconv epoch =
  retry x5 (write Cql.insertEpochForSubConversation (params LocalQuorum (epoch, cid, sconv)))

setCipherSuiteForSubConversation :: ConvId -> SubConvId -> CipherSuiteTag -> Client ()
setCipherSuiteForSubConversation cid sconv cs =
  retry x5 (write Cql.insertCipherSuiteForSubConversation (params LocalQuorum (cs, cid, sconv)))

deleteSubConversation :: ConvId -> SubConvId -> Client ()
deleteSubConversation cid sconv =
  retry x5 $ write Cql.deleteSubConversation (params LocalQuorum (cid, sconv))

listSubConversations :: ConvId -> Client (Map SubConvId ConversationMLSData)
listSubConversations cid = do
  subs <- retry x1 (query Cql.listSubConversations (params LocalQuorum (Identity cid)))
  pure . Map.fromList $ do
    (subId, cs, epoch, ts, gid) <- subs
    let activeData = case (epoch, ts) of
          (Epoch 0, _) -> Nothing
          (_, Writetime t) ->
            Just
              ActiveMLSConversationData
                { epoch = epoch,
                  epochTimestamp = t,
                  ciphersuite = cs
                }

    pure
      ( subId,
        ConversationMLSData
          { cnvmlsGroupId = gid,
            cnvmlsActiveData = activeData
          }
      )

interpretMLSCommitLockStoreToCassandra :: (Member (Embed IO) r, Member TinyLog r) => ClientState -> InterpreterFor MLSCommitLockStore r
interpretMLSCommitLockStoreToCassandra client = interpret $ \case
  AcquireCommitLock gId epoch ttl -> do
    logEffect "MLSCommitLockStore.AcquireCommitLock"
    embedClient client $ acquireCommitLock gId epoch ttl
  ReleaseCommitLock gId epoch -> do
    logEffect "MLSCommitLockStore.ReleaseCommitLock"
    embedClient client $ releaseCommitLock gId epoch

interpretConversationStoreToCassandra ::
  forall r a.
  ( Member (Embed IO) r,
    Member TinyLog r
  ) =>
  ClientState ->
  Sem (ConversationStore ': r) a ->
  Sem r a
interpretConversationStoreToCassandra client = interpret $ \case
  UpsertConversation lcnv nc -> do
    logEffect "ConversationStore.CreateConversation"
    embedClient client $ createConversation lcnv nc
  GetConversation cid -> do
    logEffect "ConversationStore.GetConversation"
    embedClient client $ getConversation cid
  GetConversationEpoch cid -> do
    logEffect "ConversationStore.GetConversationEpoch"
    embedClient client $ getConvEpoch cid
  GetConversations cids -> do
    logEffect "ConversationStore.GetConversations"
    localConversations client cids
  GetLocalConversationIds uid start maxIds -> do
    logEffect "ConversationStore.GetLocalConversationIds"
    embedClient client $ getLocalConvIds uid start maxIds
  GetRemoteConversationIds uid start maxIds -> do
    logEffect "ConversationStore.GetRemoteConversationIds"
    embedClient client $ getRemoteConvIds uid start maxIds
  GetConversationMetadata cid -> do
    logEffect "ConversationStore.GetConversationMetadata"
    embedClient client $ conversationMeta cid
  GetGroupInfo cid -> do
    logEffect "ConversationStore.GetGroupInfo"
    embedClient client $ getGroupInfo cid
  IsConversationAlive cid -> do
    logEffect "ConversationStore.IsConversationAlive"
    embedClient client $ isConvAlive cid
  SelectConversations uid cids -> do
    logEffect "ConversationStore.SelectConversations"
    embedClient client $ localConversationIdsOf uid cids
  GetRemoteConversationStatus uid cids -> do
    logEffect "ConversationStore.GetRemoteConversationStatus"
    embedClient client $ remoteConversationStatus uid cids
  SetConversationType cid ty -> do
    logEffect "ConversationStore.SetConversationType"
    embedClient client $ updateConvType cid ty
  SetConversationName cid value -> do
    logEffect "ConversationStore.SetConversationName"
    embedClient client $ updateConvName cid value
  SetConversationAccess cid value -> do
    logEffect "ConversationStore.SetConversationAccess"
    embedClient client $ updateConvAccess cid value
  SetConversationReceiptMode cid value -> do
    logEffect "ConversationStore.SetConversationReceiptMode"
    embedClient client $ updateConvReceiptMode cid value
  SetConversationMessageTimer cid value -> do
    logEffect "ConversationStore.SetConversationMessageTimer"
    embedClient client $ updateConvMessageTimer cid value
  SetConversationEpoch cid epoch -> do
    logEffect "ConversationStore.SetConversationEpoch"
    embedClient client $ updateConvEpoch cid epoch
  SetConversationCipherSuite cid cs -> do
    logEffect "ConversationStore.SetConversationCipherSuite"
    embedClient client $ updateConvCipherSuite cid cs
  SetConversationCellsState cid ps -> do
    logEffect "ConversationStore.SetConversationCellsState"
    embedClient client $ updateConvCellsState cid ps
  ResetConversation cid groupId -> do
    logEffect "ConversationStore.ResetConversation"
    embedClient client $ resetConversation cid groupId
  DeleteConversation cid -> do
    logEffect "ConversationStore.DeleteConversation"
    embedClient client $ deleteConversation cid
  SetGroupInfo cid gib -> do
    logEffect "ConversationStore.SetGroupInfo"
    embedClient client $ setGroupInfo cid gib
  UpdateToMixedProtocol cid groupId epoch -> do
    logEffect "ConversationStore.UpdateToMixedProtocol"
    updateToMixedProtocol client cid groupId epoch
  UpdateToMLSProtocol cid -> do
    logEffect "ConversationStore.UpdateToMLSProtocol"
    updateToMLSProtocol client cid
  UpdateChannelAddPermissions cid cap -> do
    logEffect "ConversationStore.UpdateChannelAddPermissions"
    embedClient client $ updateChannelAddPermissions cid cap
  DeleteTeamConversation tid cid -> do
    logEffect "ConversationStore.DeleteTeamConversation"
    embedClient client $ removeTeamConv tid cid
  GetTeamConversation tid cid -> do
    logEffect "ConversationStore.GetTeamConversation"
    embedClient client $ teamConversation tid cid
  GetTeamConversations tid -> do
    logEffect "ConversationStore.GetTeamConversations"
    embedClient client $ getTeamConversations tid
  DeleteTeamConversations tid -> do
    logEffect "ConversationStore.DeleteTeamConversations"
    embedClient client $ deleteTeamConversations tid
  UpsertMembers cid ul -> do
    logEffect "ConversationStore.CreateMembers"
    embedClient client $ addMembers cid ul
  UpsertMembersInRemoteConversation rcid uids -> do
    logEffect "ConversationStore.CreateMembersInRemoteConversation"
    embedClient client $ addLocalMembersToRemoteConv rcid uids
  CreateBotMember sr bid cid -> do
    logEffect "ConversationStore.CreateBotMember"
    embedClient client $ addBotMember sr bid cid
  GetLocalMember cid uid -> do
    logEffect "ConversationStore.GetLocalMember"
    embedClient client $ member cid uid
  GetLocalMembers cid -> do
    logEffect "ConversationStore.GetLocalMembers"
    embedClient client $ members cid
  GetRemoteMember cid uid -> do
    logEffect "ConversationStore.GetRemoteMember"
    embedClient client $ lookupRemoteMember cid (tDomain uid) (tUnqualified uid)
  GetRemoteMembers rcid -> do
    logEffect "ConversationStore.GetRemoteMembers"
    embedClient client $ lookupRemoteMembers rcid
  CheckLocalMemberRemoteConv uid rcnv -> do
    logEffect "ConversationStore.CheckLocalMemberRemoteConv"
    fmap (not . null) $ embedClient client $ lookupLocalMemberRemoteConv uid rcnv
  SelectRemoteMembers uids rcnv -> do
    logEffect "ConversationStore.SelectRemoteMembers"
    embedClient client $ filterRemoteConvMembers uids rcnv
  SetSelfMember qcid luid upd -> do
    logEffect "ConversationStore.SetSelfMember"
    embedClient client $ updateSelfMember qcid luid upd
  SetOtherMember lcid quid upd -> do
    logEffect "ConversationStore.SetOtherMember"
    embedClient client $ updateOtherMemberLocalConv lcid quid upd
  DeleteMembers cnv ul -> do
    logEffect "ConversationStore.DeleteMembers"
    embedClient client $ removeMembersFromLocalConv cnv ul
  DeleteMembersInRemoteConversation rcnv uids -> do
    logEffect "ConversationStore.DeleteMembersInRemoteConversation"
    runEmbedded (runClient client) $
      embed $
        removeLocalMembersFromRemoteConv rcnv uids
  AddMLSClients lcnv quid cs -> do
    logEffect "ConversationStore.AddMLSClients"
    embedClient client $ addMLSClients lcnv quid cs
  PlanClientRemoval lcnv cids -> do
    logEffect "ConversationStore.PlanClientRemoval"
    embedClient client $ planMLSClientRemoval lcnv cids
  RemoveMLSClients lcnv quid cs -> do
    logEffect "ConversationStore.RemoveMLSClients"
    embedClient client $ removeMLSClients lcnv quid cs
  RemoveAllMLSClients gid -> do
    logEffect "ConversationStore.RemoveAllMLSClients"
    embedClient client $ removeAllMLSClients gid
  LookupMLSClients lcnv -> do
    logEffect "ConversationStore.LookupMLSClients"
    embedClient client $ lookupMLSClients lcnv
  LookupMLSClientLeafIndices lcnv -> do
    logEffect "ConversationStore.LookupMLSClientLeafIndices"
    embedClient client $ lookupMLSClientLeafIndices lcnv
  UpsertSubConversation convId subConvId groupId -> do
    logEffect "ConversationStore.CreateSubConversation"
    embedClient client $ insertSubConversation convId subConvId groupId
  GetSubConversation convId subConvId -> do
    logEffect "ConversationStore.GetSubConversation"
    embedClient client $ selectSubConversation convId subConvId
  GetSubConversationGroupInfo convId subConvId -> do
    logEffect "ConversationStore.GetSubConversationGroupInfo"
    embedClient client $ selectSubConvGroupInfo convId subConvId
  GetSubConversationEpoch convId subConvId -> do
    logEffect "ConversationStore.GetSubConversationEpoch"
    embedClient client $ selectSubConvEpoch convId subConvId
  SetSubConversationGroupInfo convId subConvId mPgs -> do
    logEffect "ConversationStore.SetSubConversationGroupInfo"
    embedClient client $ updateSubConvGroupInfo convId subConvId mPgs
  SetSubConversationEpoch cid sconv epoch -> do
    logEffect "ConversationStore.SetSubConversationEpoch"
    embedClient client $ setEpochForSubConversation cid sconv epoch
  SetSubConversationCipherSuite cid sconv cs -> do
    logEffect "ConversationStore.SetSubConversationCipherSuite"
    embedClient client $ setCipherSuiteForSubConversation cid sconv cs
  ListSubConversations cid -> do
    logEffect "ConversationStore.ListSubConversations"
    embedClient client $ listSubConversations cid
  DeleteSubConversation convId subConvId -> do
    logEffect "ConversationStore.DeleteSubConversation"
    embedClient client $ deleteSubConversation convId subConvId
  SearchConversations _ -> do
    logEffect "ConversationStore.SearchConversations"
    pure []
  HaveRemoteConvs uids ->
    embedClient client $ haveRemoteConvs uids

interpretConversationStoreToCassandraAndPostgres ::
  forall r a.
  ( Member TinyLog r,
    PGConstraints r,
    Member Async r,
    Member (Error MigrationError) r,
    Member Race r
  ) =>
  ClientState ->
  Sem (ConversationStore ': r) a ->
  Sem r a
interpretConversationStoreToCassandraAndPostgres client = interpret $ \case
  UpsertConversation lcnv nc -> do
    -- Save new convs in postgresql
    withMigrationLockAndCleanup client LockShared (Left $ tUnqualified lcnv) $
      embedClient client (getConversation (tUnqualified lcnv)) >>= \case
        Nothing -> interpretConversationStoreToPostgres $ ConvStore.upsertConversation lcnv nc
        Just _ -> embedClient client $ createConversation lcnv nc
  GetConversation cid -> do
    logEffect "ConversationStore.GetConversation"
    withMigrationLockAndCleanup client LockShared (Left cid) $
      getConvWithPostgres cid >>= \case
        Nothing -> embedClient client (getConversation cid)
        conv -> pure conv
  GetConversationEpoch cid -> do
    logEffect "ConversationStore.GetConversationEpoch"
    withMigrationLockAndCleanup client LockShared (Left cid) $
      isConvInPostgres cid >>= \case
        False -> embedClient client (getConvEpoch cid)
        True -> interpretConversationStoreToPostgres $ ConvStore.getConversationEpoch cid
  GetConversations cids -> do
    logEffect "ConversationStore.GetConversations"
    withMigrationLocksAndCleanup client LockShared (Seconds 2) (Left <$> cids) $ do
      let indexByConvId = foldr (\storedConv -> Map.insert storedConv.id_ storedConv) Map.empty
      cassConvs <- indexByConvId <$> localConversations client cids
      pgConvs <- indexByConvId <$> interpretConversationStoreToPostgres (ConvStore.getConversations cids)
      pure $ mapMaybe (\cid -> Map.lookup cid pgConvs <|> Map.lookup cid cassConvs) cids
  GetLocalConversationIds uid start maxIds -> do
    logEffect "ConversationStore.GetLocalConversationIds"

    -- [Migration Locking Limitation]
    --
    -- Here we cannot acquire any locks because we do not have convIds to start
    -- with. This could cause consistency problems in two ways:
    -- 1. Duplicate convId will be retrieved from PG
    -- 2. Reading a conv which got deleted (or the user got removed from this
    --    conv) in Postgresql after being migrated but the migration left it
    --    behind in Cassandra.
    --
    -- 1. is solved by de-duplicating the list below
    -- 2. is not solved here. When there is any other action attempted on this
    --    ConvId, it should get cleaned up from Cassandra before that action, so
    --    it _should_ be fine to return it here. But strictly speaking this is
    --    inconsistent behaviour.
    --
    -- A solution could be to keep looping and locking until we get to a stable
    -- situation, but that could run into creating too many sessions with
    -- Postgres
    cassConvIds <- embedClient client $ getLocalConvIds uid start maxIds
    pgConvIds <- interpretConversationStoreToPostgres $ ConvStore.getLocalConversationIds uid start maxIds

    let allResults =
          sortOn (\cid -> (UUID.version $ toUUID cid, cid)) $
            List.nubOrd (pgConvIds.resultSetResult <> cassConvIds.resultSetResult)
        maxIdsInt = (fromIntegral $ fromRange maxIds)
    pure $
      ResultSet
        { resultSetResult = take maxIdsInt allResults,
          resultSetType =
            if cassConvIds.resultSetType == ResultSetTruncated
              || pgConvIds.resultSetType == ResultSetTruncated
              || length allResults > maxIdsInt
              then ResultSetTruncated
              else ResultSetComplete
        }
  GetRemoteConversationIds uid start maxIds -> do
    logEffect "ConversationStore.GetRemoteConversationIds"
    withMigrationLockAndCleanup client LockShared (Right uid) $ do
      isUserInPostgres uid >>= \case
        False -> embedClient client $ getRemoteConvIds uid start maxIds
        True -> interpretConversationStoreToPostgres $ ConvStore.getRemoteConversationIds uid start maxIds
  GetConversationMetadata cid -> do
    logEffect "ConversationStore.GetConversationMetadata"
    withMigrationLockAndCleanup client LockShared (Left cid) $
      interpretConversationStoreToPostgres (ConvStore.getConversationMetadata cid) >>= \case
        Nothing -> embedClient client (conversationMeta cid)
        meta -> pure meta
  GetGroupInfo cid -> do
    logEffect "ConversationStore.GetGroupInfo"
    withMigrationLockAndCleanup client LockShared (Left cid) $
      isConvInPostgres cid >>= \case
        False -> embedClient client (getGroupInfo cid)
        True -> interpretConversationStoreToPostgres (ConvStore.getGroupInfo cid)
  IsConversationAlive cid -> do
    logEffect "ConversationStore.IsConversationAlive"
    withMigrationLockAndCleanup client LockShared (Left cid) $
      isConvInPostgres cid >>= \case
        False -> embedClient client (isConvAlive cid)
        True -> interpretConversationStoreToPostgres (ConvStore.isConversationAlive cid)
  SelectConversations uid cids -> do
    logEffect "ConversationStore.SelectConversations"
    withMigrationLocksAndCleanup client LockShared (Seconds 2) (Left <$> cids) $ do
      cassConvs <- embedClient client $ localConversationIdsOf uid cids
      pgConvs <- interpretConversationStoreToPostgres $ ConvStore.selectConversations uid cids
      pure $ List.nubOrd (pgConvs <> cassConvs)
  GetRemoteConversationStatus uid cids -> do
    logEffect "ConversationStore.GetRemoteConversationStatus"
    withMigrationLockAndCleanup client LockShared (Right uid) $ do
      isUserInPostgres uid >>= \case
        False -> embedClient client $ remoteConversationStatus uid cids
        True -> interpretConversationStoreToPostgres $ ConvStore.getRemoteConversationStatus uid cids
  SetConversationType cid ty -> do
    logEffect "ConversationStore.SetConversationType"
    withMigrationLockAndCleanup client LockShared (Left cid) $
      isConvInPostgres cid >>= \case
        False -> embedClient client $ updateConvType cid ty
        True -> interpretConversationStoreToPostgres (ConvStore.setConversationType cid ty)
  SetConversationName cid value -> do
    logEffect "ConversationStore.SetConversationName"
    withMigrationLockAndCleanup client LockShared (Left cid) $
      isConvInPostgres cid >>= \case
        False -> embedClient client $ updateConvName cid value
        True -> interpretConversationStoreToPostgres (ConvStore.setConversationName cid value)
  SetConversationAccess cid value -> do
    logEffect "ConversationStore.SetConversationAccess"
    withMigrationLockAndCleanup client LockShared (Left cid) $
      isConvInPostgres cid >>= \case
        False -> embedClient client $ updateConvAccess cid value
        True -> interpretConversationStoreToPostgres (ConvStore.setConversationAccess cid value)
  SetConversationReceiptMode cid value -> do
    logEffect "ConversationStore.SetConversationReceiptMode"
    withMigrationLockAndCleanup client LockShared (Left cid) $
      isConvInPostgres cid >>= \case
        False -> embedClient client $ updateConvReceiptMode cid value
        True -> interpretConversationStoreToPostgres (ConvStore.setConversationReceiptMode cid value)
  SetConversationMessageTimer cid value -> do
    logEffect "ConversationStore.SetConversationMessageTimer"
    withMigrationLockAndCleanup client LockShared (Left cid) $
      isConvInPostgres cid >>= \case
        False -> embedClient client $ updateConvMessageTimer cid value
        True -> interpretConversationStoreToPostgres (ConvStore.setConversationMessageTimer cid value)
  SetConversationEpoch cid epoch -> do
    logEffect "ConversationStore.SetConversationEpoch"
    withMigrationLockAndCleanup client LockShared (Left cid) $
      isConvInPostgres cid >>= \case
        False -> embedClient client $ updateConvEpoch cid epoch
        True -> interpretConversationStoreToPostgres (ConvStore.setConversationEpoch cid epoch)
  SetConversationCipherSuite cid cs -> do
    logEffect "ConversationStore.SetConversationCipherSuite"
    withMigrationLockAndCleanup client LockShared (Left cid) $
      isConvInPostgres cid >>= \case
        False -> embedClient client $ updateConvCipherSuite cid cs
        True -> interpretConversationStoreToPostgres (ConvStore.setConversationCipherSuite cid cs)
  SetConversationCellsState cid ps -> do
    logEffect "ConversationStore.SetConversationCellsState"
    withMigrationLockAndCleanup client LockShared (Left cid) $
      isConvInPostgres cid >>= \case
        False -> embedClient client $ updateConvCellsState cid ps
        True -> interpretConversationStoreToPostgres (ConvStore.setConversationCellsState cid ps)
  ResetConversation cid groupId -> do
    logEffect "ConversationStore.ResetConversation"
    withMigrationLockAndCleanup client LockShared (Left cid) $
      isConvInPostgres cid >>= \case
        False -> embedClient client $ resetConversation cid groupId
        True -> interpretConversationStoreToPostgres (ConvStore.resetConversation cid groupId)
  DeleteConversation cid -> do
    logEffect "ConversationStore.DeleteConversation"
    withMigrationLockAndCleanup client LockShared (Left cid) $ do
      embedClient client $ deleteConversation cid
      interpretConversationStoreToPostgres (ConvStore.deleteConversation cid)
  SetGroupInfo cid gib -> do
    logEffect "ConversationStore.SetGroupInfo"
    withMigrationLockAndCleanup client LockShared (Left cid) $
      isConvInPostgres cid >>= \case
        False -> embedClient client $ setGroupInfo cid gib
        True -> interpretConversationStoreToPostgres (ConvStore.setGroupInfo cid gib)
  UpdateToMixedProtocol cid groupId epoch -> do
    logEffect "ConversationStore.UpdateToMixedProtocol"
    withMigrationLockAndCleanup client LockShared (Left cid) $
      isConvInPostgres cid >>= \case
        False -> updateToMixedProtocol client cid groupId epoch
        True -> interpretConversationStoreToPostgres (ConvStore.updateToMixedProtocol cid groupId epoch)
  UpdateToMLSProtocol cid -> do
    logEffect "ConversationStore.UpdateToMLSProtocol"
    withMigrationLockAndCleanup client LockShared (Left cid) $
      isConvInPostgres cid >>= \case
        False -> updateToMLSProtocol client cid
        _ -> interpretConversationStoreToPostgres (ConvStore.updateToMLSProtocol cid)
  UpdateChannelAddPermissions cid cap -> do
    logEffect "ConversationStore.UpdateChannelAddPermissions"
    withMigrationLockAndCleanup client LockShared (Left cid) $
      isConvInPostgres cid >>= \case
        False -> embedClient client $ updateChannelAddPermissions cid cap
        _ -> interpretConversationStoreToPostgres (ConvStore.updateChannelAddPermissions cid cap)
  DeleteTeamConversation tid cid -> do
    logEffect "ConversationStore.DeleteTeamConversation"
    withMigrationLockAndCleanup client LockShared (Left cid) $ do
      embedClient client $ removeTeamConv tid cid
      interpretConversationStoreToPostgres (ConvStore.deleteTeamConversation tid cid)
  GetTeamConversation tid cid -> do
    logEffect "ConversationStore.GetTeamConversation"
    withMigrationLockAndCleanup client LockShared (Left cid) $
      interpretConversationStoreToPostgres (ConvStore.getTeamConversation tid cid) >>= \case
        Just foundCid -> pure $ Just foundCid
        Nothing -> embedClient client $ teamConversation tid cid
  GetTeamConversations tid -> do
    logEffect "ConversationStore.GetTeamConversations"
    -- See [Migration Locking Limitation]
    cassConvs <- embedClient client $ getTeamConversations tid
    pgConvs <- interpretConversationStoreToPostgres $ ConvStore.getTeamConversations tid
    pure $ List.nubOrd (pgConvs <> cassConvs)
  DeleteTeamConversations tid -> do
    logEffect "ConversationStore.DeleteTeamConversations"
    embedClient client $ deleteTeamConversations tid
    interpretConversationStoreToPostgres $ ConvStore.deleteTeamConversations tid
  UpsertMembers cid ul -> do
    logEffect "ConversationStore.CreateMembers"
    withMigrationLockAndCleanup client LockShared (Left cid) $
      isConvInPostgres cid >>= \case
        False -> embedClient client $ addMembers cid ul
        _ -> interpretConversationStoreToPostgres (ConvStore.upsertMembers cid ul)
  UpsertMembersInRemoteConversation rcid uids -> do
    logEffect "ConversationStore.CreateMembersInRemoteConversation"

    -- Save users joining their first remote conv in postgres
    withMigrationLocksAndCleanup client LockShared (Seconds 2) (Right <$> uids) $ do
      filterUsersInPostgres uids >>= \pgUids -> do
        let -- These are not in Postegres, but that doesn't mean they're in
            -- cassandra
            nonPgUids = filter (`notElem` pgUids) uids
        cassUids <- embedClient client $ haveRemoteConvs nonPgUids
        let nonCassUids = filter (`notElem` cassUids) uids
        interpretConversationStoreToPostgres $ ConvStore.upsertMembersInRemoteConversation rcid nonCassUids
        embedClient client $ addLocalMembersToRemoteConv rcid cassUids
  CreateBotMember sr bid cid -> do
    logEffect "ConversationStore.CreateBotMember"
    withMigrationLockAndCleanup client LockShared (Left cid) $
      isConvInPostgres cid >>= \case
        False -> embedClient client $ addBotMember sr bid cid
        _ -> interpretConversationStoreToPostgres (ConvStore.createBotMember sr bid cid)
  GetLocalMember cid uid -> do
    logEffect "ConversationStore.GetLocalMember"
    withMigrationLockAndCleanup client LockShared (Left cid) $
      isConvInPostgres cid >>= \case
        False -> embedClient client $ member cid uid
        True -> interpretConversationStoreToPostgres (ConvStore.getLocalMember cid uid)
  GetLocalMembers cid -> do
    logEffect "ConversationStore.GetLocalMembers"
    withMigrationLockAndCleanup client LockShared (Left cid) $
      isConvInPostgres cid >>= \case
        False -> embedClient client $ members cid
        True -> interpretConversationStoreToPostgres (ConvStore.getLocalMembers cid)
  GetRemoteMember cid uid -> do
    logEffect "ConversationStore.GetRemoteMember"
    withMigrationLockAndCleanup client LockShared (Left cid) $
      isConvInPostgres cid >>= \case
        False -> embedClient client $ lookupRemoteMember cid (tDomain uid) (tUnqualified uid)
        True -> interpretConversationStoreToPostgres (ConvStore.getRemoteMember cid uid)
  GetRemoteMembers cid -> do
    logEffect "ConversationStore.GetRemoteMembers"
    withMigrationLockAndCleanup client LockShared (Left cid) $
      isConvInPostgres cid >>= \case
        False -> embedClient client $ lookupRemoteMembers cid
        True -> interpretConversationStoreToPostgres (ConvStore.getRemoteMembers cid)
  CheckLocalMemberRemoteConv uid rcnv -> do
    logEffect "ConversationStore.CheckLocalMemberRemoteConv"
    withMigrationLockAndCleanup client LockShared (Right uid) $ do
      isUserInPostgres uid >>= \case
        False -> fmap (not . null) $ embedClient client $ lookupLocalMemberRemoteConv uid rcnv
        True -> interpretConversationStoreToPostgres $ ConvStore.checkLocalMemberRemoteConv uid rcnv
  SelectRemoteMembers uids rcnv -> do
    logEffect "ConversationStore.SelectRemoteMembers"
    withMigrationLocksAndCleanup client LockShared (Seconds 2) (Right <$> uids) $ do
      filterUsersInPostgres uids >>= \pgUids -> do
        (pgUsers, _) <- interpretConversationStoreToPostgres $ ConvStore.selectRemoteMembers pgUids rcnv
        (cassUsers, _) <- embedClient client $ filterRemoteConvMembers uids rcnv
        let foundUsers = pgUsers <> cassUsers
        pure (foundUsers, Set.fromList foundUsers == Set.fromList uids)
  SetSelfMember qcid luid upd -> do
    logEffect "ConversationStore.SetSelfMember"
    let localConvFunctions lcid =
          ( withMigrationLockAndCleanup client LockShared (Left (tUnqualified lcid)),
            isConvInPostgres (tUnqualified lcid)
          )
        remoteConvFunctions _ =
          ( withMigrationLockAndCleanup client (LockShared) (Right (tUnqualified luid)),
            isUserInPostgres (tUnqualified luid)
          )
    let (withLock, isInPG) = foldQualified luid localConvFunctions remoteConvFunctions qcid
    withLock $
      isInPG >>= \case
        False -> embedClient client $ updateSelfMember qcid luid upd
        True -> interpretConversationStoreToPostgres $ ConvStore.setSelfMember qcid luid upd
  SetOtherMember lcid quid upd -> do
    logEffect "ConversationStore.SetOtherMember"
    withMigrationLockAndCleanup client LockShared (Left $ tUnqualified lcid) $
      isConvInPostgres (tUnqualified lcid) >>= \case
        False -> embedClient client $ updateOtherMemberLocalConv lcid quid upd
        True -> interpretConversationStoreToPostgres (ConvStore.setOtherMember lcid quid upd)
  DeleteMembers cid ul -> do
    logEffect "ConversationStore.DeleteMembers"
    withMigrationLockAndCleanup client LockShared (Left cid) $ do
      -- No need to check where these are, we just delete them from both places
      embedClient client $ removeMembersFromLocalConv cid ul
      interpretConversationStoreToPostgres $ ConvStore.deleteMembers cid ul
  DeleteMembersInRemoteConversation rcnv uids -> do
    logEffect "ConversationStore.DeleteMembersInRemoteConversation"
    withMigrationLocksAndCleanup client LockShared (Seconds 2) (Right <$> uids) $ do
      -- No need to check where these are, we just delete them from both places
      embedClient client $ removeLocalMembersFromRemoteConv rcnv uids
      interpretConversationStoreToPostgres $ ConvStore.deleteMembersInRemoteConversation rcnv uids
  AddMLSClients groupId quid cs -> do
    logEffect "ConversationStore.AddMLSClients"
    cid <- groupIdToConvId groupId
    withMigrationLockAndCleanup client LockShared (Left cid) $
      isConvInPostgres cid >>= \case
        False -> embedClient client $ addMLSClients groupId quid cs
        True -> interpretConversationStoreToPostgres (ConvStore.addMLSClients groupId quid cs)
  PlanClientRemoval gid clients -> do
    logEffect "ConversationStore.PlanClientRemoval"
    cid <- groupIdToConvId gid
    withMigrationLockAndCleanup client LockShared (Left cid) $
      isConvInPostgres cid >>= \case
        False -> embedClient client $ planMLSClientRemoval gid clients
        True -> interpretConversationStoreToPostgres (ConvStore.planClientRemoval gid clients)
  RemoveMLSClients gid quid cs -> do
    logEffect "ConversationStore.RemoveMLSClients"
    cid <- groupIdToConvId gid
    withMigrationLockAndCleanup client LockShared (Left cid) $ do
      embedClient client $ removeMLSClients gid quid cs
      interpretConversationStoreToPostgres (ConvStore.removeMLSClients gid quid cs)
  RemoveAllMLSClients gid -> do
    logEffect "ConversationStore.RemoveAllMLSClients"
    cid <- groupIdToConvId gid
    withMigrationLockAndCleanup client LockShared (Left cid) $ do
      embedClient client $ removeAllMLSClients gid
      interpretConversationStoreToPostgres (ConvStore.removeAllMLSClients gid)
  LookupMLSClients gid -> do
    logEffect "ConversationStore.LookupMLSClients"
    cid <- groupIdToConvId gid
    withMigrationLockAndCleanup client LockShared (Left cid) $
      isConvInPostgres cid >>= \case
        False -> embedClient client $ lookupMLSClients gid
        True -> interpretConversationStoreToPostgres (ConvStore.lookupMLSClients gid)
  LookupMLSClientLeafIndices gid -> do
    logEffect "ConversationStore.LookupMLSClientLeafIndices"
    cid <- groupIdToConvId gid
    withMigrationLockAndCleanup client LockShared (Left cid) $
      isConvInPostgres cid >>= \case
        False -> embedClient client $ lookupMLSClientLeafIndices gid
        True -> interpretConversationStoreToPostgres (ConvStore.lookupMLSClientLeafIndices gid)
  UpsertSubConversation convId subConvId groupId -> do
    logEffect "ConversationStore.CreateSubConversation"
    withMigrationLockAndCleanup client LockShared (Left convId) $
      isConvInPostgres convId >>= \case
        False -> embedClient client $ insertSubConversation convId subConvId groupId
        True -> interpretConversationStoreToPostgres (ConvStore.upsertSubConversation convId subConvId groupId)
  GetSubConversation convId subConvId -> do
    logEffect "ConversationStore.GetSubConversation"
    withMigrationLockAndCleanup client LockShared (Left convId) $
      isConvInPostgres convId >>= \case
        False -> embedClient client $ selectSubConversation convId subConvId
        True -> interpretConversationStoreToPostgres $ ConvStore.getSubConversation convId subConvId
  GetSubConversationGroupInfo convId subConvId -> do
    logEffect "ConversationStore.GetSubConversationGroupInfo"
    withMigrationLockAndCleanup client LockShared (Left convId) $
      isConvInPostgres convId >>= \case
        False -> embedClient client $ selectSubConvGroupInfo convId subConvId
        True -> interpretConversationStoreToPostgres $ ConvStore.getSubConversationGroupInfo convId subConvId
  GetSubConversationEpoch convId subConvId -> do
    logEffect "ConversationStore.GetSubConversationEpoch"
    withMigrationLockAndCleanup client LockShared (Left convId) $
      isConvInPostgres convId >>= \case
        False -> embedClient client $ selectSubConvEpoch convId subConvId
        True -> interpretConversationStoreToPostgres $ ConvStore.getSubConversationEpoch convId subConvId
  SetSubConversationGroupInfo convId subConvId mPgs -> do
    logEffect "ConversationStore.SetSubConversationGroupInfo"
    withMigrationLockAndCleanup client LockShared (Left convId) $
      isConvInPostgres convId >>= \case
        False -> embedClient client $ updateSubConvGroupInfo convId subConvId mPgs
        True -> interpretConversationStoreToPostgres $ ConvStore.setSubConversationGroupInfo convId subConvId mPgs
  SetSubConversationEpoch cid sconv epoch -> do
    logEffect "ConversationStore.SetSubConversationEpoch"
    withMigrationLockAndCleanup client LockShared (Left cid) $
      isConvInPostgres cid >>= \case
        False -> embedClient client $ setEpochForSubConversation cid sconv epoch
        True -> interpretConversationStoreToPostgres $ ConvStore.setSubConversationEpoch cid sconv epoch
  SetSubConversationCipherSuite cid sconv cs -> do
    logEffect "ConversationStore.SetSubConversationCipherSuite"
    withMigrationLockAndCleanup client LockShared (Left cid) $
      isConvInPostgres cid >>= \case
        False -> embedClient client $ setCipherSuiteForSubConversation cid sconv cs
        True -> interpretConversationStoreToPostgres $ ConvStore.setSubConversationCipherSuite cid sconv cs
  ListSubConversations cid -> do
    logEffect "ConversationStore.ListSubConversations"
    withMigrationLockAndCleanup client LockShared (Left cid) $
      isConvInPostgres cid >>= \case
        False -> embedClient client $ listSubConversations cid
        True -> interpretConversationStoreToPostgres $ ConvStore.listSubConversations cid
  DeleteSubConversation convId subConvId -> do
    logEffect "ConversationStore.DeleteSubConversation"
    withMigrationLockAndCleanup client LockShared (Left convId) $
      isConvInPostgres convId >>= \case
        False -> embedClient client $ deleteSubConversation convId subConvId
        True -> interpretConversationStoreToPostgres $ ConvStore.deleteSubConversation convId subConvId
  SearchConversations _ -> do
    -- In theory, it is possible to make this partially work. But we don't have
    -- to worry so much about this interpreter to be used only during the
    -- transition.
    logEffect "ConversationStore.SearchConversations"
    pure []
  HaveRemoteConvs uids -> do
    logEffect "ConversationStore.DeleteSubConversation"
    withMigrationLocksAndCleanup client LockShared (Seconds 2) (Right <$> uids) $ do
      remotesInCass <- embedClient client $ haveRemoteConvs uids
      remotesInPG <- interpretConversationStoreToPostgres $ ConvStore.haveRemoteConvs uids
      pure $ List.nubOrd (remotesInPG <> remotesInCass)

getConvWithPostgres :: (PGConstraints r) => ConvId -> Sem r (Maybe StoredConversation)
getConvWithPostgres cid = interpretConversationStoreToPostgres $ ConvStore.getConversation cid

isConvInPostgres :: (PGConstraints r) => ConvId -> Sem r Bool
isConvInPostgres cid = interpretConversationStoreToPostgres $ ConvStore.isConversationAlive cid

-- | Here a user being in postgres means that their remote conv memberships have
-- been migrated to Postgres
isUserInPostgres :: (PGConstraints r) => UserId -> Sem r Bool
isUserInPostgres uid = do
  filterUsersInPostgres [uid] >>= \case
    [] -> pure False
    _ -> pure True

filterUsersInPostgres :: (PGConstraints r) => [UserId] -> Sem r [UserId]
filterUsersInPostgres uids = do
  interpretConversationStoreToPostgres (ConvStore.haveRemoteConvs uids)

-- | Assumes that the GroupId is local
groupIdToConvId :: (Member (Error MigrationError) r) => GroupId -> Sem r ConvId
groupIdToConvId gid =
  case groupIdToConv gid of
    Left _ -> throw InvalidGroupId
    Right (_, gidParts) -> pure gidParts.qConvId.qUnqualified.conv

data MigrationError
  = InvalidGroupId
  | FailedToAcquireMigrationLock MigrationLockError
  deriving (Show)

instance APIError MigrationError where
  toResponse _ = waiErrorToJSONResponse $ WaiError.mkError status500 "internal-server-error" "Internal Server Error"

withMigrationLockAndCleanup ::
  (PGConstraints r, Member Async r, Member TinyLog r, Member Race r, Member (Error MigrationError) r) =>
  ClientState ->
  LockType ->
  Either ConvId UserId ->
  Sem (Error MigrationLockError : r) a ->
  Sem r a
withMigrationLockAndCleanup cassClient ty key =
  withMigrationLocksAndCleanup cassClient ty (MilliSeconds 500) [key]

withMigrationLocksAndCleanup ::
  ( PGConstraints r,
    Member Async r,
    Member TinyLog r,
    Member Race r,
    Member (Error MigrationError) r,
    TimeUnit u
  ) =>
  ClientState ->
  LockType ->
  u ->
  [Either ConvId UserId] ->
  Sem (Error MigrationLockError : r) a ->
  Sem r a
withMigrationLocksAndCleanup cassClient lockType maxWait convOrUsers action =
  mapError FailedToAcquireMigrationLock . withMigrationLocks lockType maxWait convOrUsers $ do
    interpretConversationStoreToCassandra cassClient
      . runInputConst cassClient
      $ cleanupIfNecessary convOrUsers
    action
