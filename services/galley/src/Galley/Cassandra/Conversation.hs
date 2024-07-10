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

module Galley.Cassandra.Conversation
  ( createConversation,
    deleteConversation,
    interpretConversationStoreToCassandra,
  )
where

import Cassandra hiding (Set)
import Cassandra qualified as Cql
import Cassandra.Util
import Control.Error.Util
import Control.Monad.Trans.Maybe
import Data.ByteString.Conversion
import Data.Id
import Data.Map qualified as Map
import Data.Misc
import Data.Qualified
import Data.Range
import Data.Set qualified as Set
import Data.Time.Clock
import Data.UUID.V4 (nextRandom)
import Galley.Cassandra.Access
import Galley.Cassandra.Conversation.MLS
import Galley.Cassandra.Conversation.Members
import Galley.Cassandra.Queries qualified as Cql
import Galley.Cassandra.Store
import Galley.Cassandra.Util
import Galley.Data.Conversation
import Galley.Data.Conversation.Types
import Galley.Effects.ConversationStore (ConversationStore (..))
import Galley.Types.Conversations.Members
import Galley.Types.ToUserRole
import Galley.Types.UserList
import Imports
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog
import System.Logger qualified as Log
import UnliftIO qualified
import Wire.API.Conversation hiding (Conversation, Member)
import Wire.API.Conversation.Protocol
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Group.Serialisation
import Wire.API.MLS.GroupInfo
import Wire.API.MLS.SubConversation
import Wire.API.User

createMLSSelfConversation ::
  Local UserId ->
  Client Conversation
createMLSSelfConversation lusr = do
  let cnv = mlsSelfConvId . tUnqualified $ lusr
      usr = tUnqualified lusr
      nc =
        NewConversation
          { ncMetadata =
              (defConversationMetadata (Just usr)) {cnvmType = SelfConv},
            ncUsers = ulFromLocals [toUserRole usr],
            ncProtocol = BaseProtocolMLSTag
          }
      meta = ncMetadata nc
      gid = convToGroupId . groupIdParts meta.cnvmType . fmap Conv . tUntagged . qualifyAs lusr $ cnv
      proto =
        ProtocolMLS
          ConversationMLSData
            { cnvmlsGroupId = gid,
              cnvmlsActiveData = Nothing
            }
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency LocalQuorum
    addPrepQuery
      Cql.insertMLSSelfConv
      ( cnv,
        cnvmType meta,
        cnvmCreator meta,
        Cql.Set (cnvmAccess meta),
        Cql.Set (toList (cnvmAccessRoles meta)),
        cnvmName meta,
        cnvmTeam meta,
        cnvmMessageTimer meta,
        cnvmReceiptMode meta,
        Just gid
      )

  (lmems, rmems) <- addMembers cnv (ncUsers nc)
  pure
    Conversation
      { convId = cnv,
        convLocalMembers = lmems,
        convRemoteMembers = rmems,
        convDeleted = False,
        convMetadata = meta,
        convProtocol = proto
      }

createConversation :: Local ConvId -> NewConversation -> Client Conversation
createConversation lcnv nc = do
  let meta = ncMetadata nc
      (proto, mgid) = case ncProtocol nc of
        BaseProtocolProteusTag -> (ProtocolProteus, Nothing)
        BaseProtocolMLSTag ->
          let gid = convToGroupId . groupIdParts meta.cnvmType $ Conv <$> tUntagged lcnv
           in ( ProtocolMLS
                  ConversationMLSData
                    { cnvmlsGroupId = gid,
                      cnvmlsActiveData = Nothing
                    },
                Just gid
              )
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency LocalQuorum
    addPrepQuery
      Cql.insertConv
      ( tUnqualified lcnv,
        cnvmType meta,
        cnvmCreator meta,
        Cql.Set (cnvmAccess meta),
        Cql.Set (toList (cnvmAccessRoles meta)),
        cnvmName meta,
        cnvmTeam meta,
        cnvmMessageTimer meta,
        cnvmReceiptMode meta,
        baseProtocolToProtocol (ncProtocol nc),
        mgid
      )
    for_ (cnvmTeam meta) $ \tid -> addPrepQuery Cql.insertTeamConv (tid, tUnqualified lcnv)
  (lmems, rmems) <- addMembers (tUnqualified lcnv) (ncUsers nc)
  pure
    Conversation
      { convId = tUnqualified lcnv,
        convLocalMembers = lmems,
        convRemoteMembers = rmems,
        convDeleted = False,
        convMetadata = meta,
        convProtocol = proto
      }

deleteConversation :: ConvId -> Client ()
deleteConversation cid = do
  retry x5 $ write Cql.markConvDeleted (params LocalQuorum (Identity cid))

  localMembers <- members cid
  remoteMembers <- lookupRemoteMembers cid

  removeMembersFromLocalConv cid $
    UserList (lmId <$> localMembers) (rmId <$> remoteMembers)

  retry x5 $ write Cql.deleteConv (params LocalQuorum (Identity cid))

conversationMeta :: ConvId -> Client (Maybe ConversationMetadata)
conversationMeta conv =
  (toConvMeta =<<)
    <$> retry x1 (query1 Cql.selectConv (params LocalQuorum (Identity conv)))
  where
    toConvMeta (t, mc, a, r, r', n, i, _, mt, rm, _, _, _, _, _, _, _) = do
      let mbAccessRolesV2 = Set.fromList . Cql.fromSet <$> r'
          accessRoles = maybeRole t $ parseAccessRoles r mbAccessRolesV2
      pure $ ConversationMetadata t mc (defAccess t a) accessRoles n i mt rm

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

updateConvGroupPicture :: ConvId -> Text -> Text -> Client ()
updateConvGroupPicture cid colour emoji = retry x5 $ write Cql.updateConvGroupPicture (params LocalQuorum (colour, emoji, cid))

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

setGroupInfo :: ConvId -> GroupInfoData -> Client ()
setGroupInfo conv gid =
  write Cql.updateGroupInfo (params LocalQuorum (gid, conv))

getConversation :: ConvId -> Client (Maybe Conversation)
getConversation conv = do
  cdata <- UnliftIO.async $ retry x1 (query1 Cql.selectConv (params LocalQuorum (Identity conv)))
  remoteMems <- UnliftIO.async $ lookupRemoteMembers conv
  mbConv <-
    toConv conv
      <$> members conv
      <*> UnliftIO.wait remoteMems
      <*> UnliftIO.wait cdata
  runMaybeT $ conversationGC =<< maybe mzero pure mbConv

-- | "Garbage collect" a 'Conversation', i.e. if the conversation is
-- marked as deleted, actually remove it from the database and return
-- 'Nothing'.
conversationGC ::
  Conversation ->
  MaybeT Client Conversation
conversationGC conv =
  asum
    [ -- return conversation if not deleted
      guard (not (convDeleted conv)) $> conv,
      -- actually delete it and fail
      lift (deleteConversation (convId conv)) *> mzero
    ]

localConversation :: ConvId -> Client (Maybe Conversation)
localConversation cid =
  UnliftIO.runConcurrently $
    toConv cid
      <$> UnliftIO.Concurrently (members cid)
      <*> UnliftIO.Concurrently (lookupRemoteMembers cid)
      <*> UnliftIO.Concurrently
        ( retry x1 $
            query1 Cql.selectConv (params LocalQuorum (Identity cid))
        )

localConversations ::
  ( Member (Embed IO) r,
    Member (Input ClientState) r,
    Member TinyLog r
  ) =>
  [ConvId] ->
  Sem r [Conversation]
localConversations =
  collectAndLog
    <=< ( embedClient
            . UnliftIO.pooledMapConcurrentlyN 8 localConversation'
        )
  where
    collectAndLog cs = case partitionEithers cs of
      (errs, convs) -> traverse_ (warn . Log.msg) errs $> convs

    localConversation' :: ConvId -> Client (Either ByteString Conversation)
    localConversation' cid =
      note ("No conversation for: " <> toByteString' cid) <$> localConversation cid

-- | Takes a list of conversation ids and returns those found for the given
-- user.
localConversationIdsOf :: UserId -> [ConvId] -> Client [ConvId]
localConversationIdsOf usr cids = do
  runIdentity <$$> retry x1 (query Cql.selectUserConvsIn (params LocalQuorum (usr, cids)))

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

toProtocol ::
  Maybe ProtocolTag ->
  Maybe GroupId ->
  Maybe Epoch ->
  Maybe UTCTime ->
  Maybe CipherSuiteTag ->
  Maybe Protocol
toProtocol Nothing _ _ _ _ = Just ProtocolProteus
toProtocol (Just ProtocolProteusTag) _ _ _ _ = Just ProtocolProteus
toProtocol (Just ProtocolMLSTag) mgid mepoch mtimestamp mcs = ProtocolMLS <$> toConversationMLSData mgid mepoch mtimestamp mcs
toProtocol (Just ProtocolMixedTag) mgid mepoch mtimestamp mcs = ProtocolMixed <$> toConversationMLSData mgid mepoch mtimestamp mcs

toConversationMLSData :: Maybe GroupId -> Maybe Epoch -> Maybe UTCTime -> Maybe CipherSuiteTag -> Maybe ConversationMLSData
toConversationMLSData mgid mepoch mtimestamp mcs =
  ConversationMLSData
    <$> mgid
    <*> pure
      ( ActiveMLSConversationData
          <$> mepoch
          <*> mtimestamp
          <*> mcs
      )

toConv ::
  ConvId ->
  [LocalMember] ->
  [RemoteMember] ->
  Maybe
    ( ConvType,
      Maybe UserId,
      Maybe (Cql.Set Access),
      Maybe AccessRoleLegacy,
      Maybe (Cql.Set AccessRole),
      Maybe Text,
      Maybe TeamId,
      Maybe Bool,
      Maybe Milliseconds,
      Maybe ReceiptMode,
      Maybe ProtocolTag,
      Maybe GroupId,
      Maybe Epoch,
      Maybe (Writetime Epoch),
      Maybe CipherSuiteTag,
      Maybe Text,
      Maybe Text
    ) ->
  Maybe Conversation
toConv cid ms remoteMems mconv = do
  (cty, muid, acc, role, roleV2, nme, ti, del, timer, rm, ptag, mgid, mep, mts, mcs, _mcolour, _memoji) <- mconv
  let mbAccessRolesV2 = Set.fromList . Cql.fromSet <$> roleV2
      accessRoles = maybeRole cty $ parseAccessRoles role mbAccessRolesV2
  proto <- toProtocol ptag mgid mep (writetimeToUTC <$> mts) mcs
  pure
    Conversation
      { convId = cid,
        convDeleted = fromMaybe False del,
        convLocalMembers = ms,
        convRemoteMembers = remoteMems,
        convProtocol = proto,
        convMetadata =
          ConversationMetadata
            { cnvmType = cty,
              cnvmCreator = muid,
              cnvmAccess = defAccess cty acc,
              cnvmAccessRoles = accessRoles,
              cnvmName = nme,
              cnvmTeam = ti,
              cnvmMessageTimer = timer,
              cnvmReceiptMode = rm
            }
      }

updateToMixedProtocol ::
  ( Members
      '[ Embed IO,
         Input ClientState
       ]
      r
  ) =>
  Local ConvId ->
  ConvType ->
  Sem r ()
updateToMixedProtocol lcnv ct = do
  let gid = convToGroupId . groupIdParts ct $ Conv <$> tUntagged lcnv
      epoch = Epoch 0
  embedClient . retry x5 . batch $ do
    setType BatchLogged
    setConsistency LocalQuorum
    addPrepQuery Cql.updateToMixedConv (tUnqualified lcnv, ProtocolMixedTag, gid, epoch)
  pure ()

updateToMLSProtocol ::
  ( Members
      '[ Embed IO,
         Input ClientState
       ]
      r
  ) =>
  Local ConvId ->
  Sem r ()
updateToMLSProtocol lcnv =
  embedClient . retry x5 $
    write Cql.updateToMLSConv (params LocalQuorum (tUnqualified lcnv, ProtocolMLSTag))

interpretConversationStoreToCassandra ::
  ( Member (Embed IO) r,
    Member (Input ClientState) r,
    Member TinyLog r
  ) =>
  Sem (ConversationStore ': r) a ->
  Sem r a
interpretConversationStoreToCassandra = interpret $ \case
  CreateConversationId -> do
    logEffect "ConversationStore.CreateConversationId"
    Id <$> embed nextRandom
  CreateConversation loc nc -> do
    logEffect "ConversationStore.CreateConversation"
    embedClient $ createConversation loc nc
  CreateMLSSelfConversation lusr -> do
    logEffect "ConversationStore.CreateMLSSelfConversation"
    embedClient $ createMLSSelfConversation lusr
  GetConversation cid -> do
    logEffect "ConversationStore.GetConversation"
    embedClient $ getConversation cid
  GetConversationEpoch cid -> do
    logEffect "ConversationStore.GetConversationEpoch"
    embedClient $ getConvEpoch cid
  GetConversations cids -> do
    logEffect "ConversationStore.GetConversations"
    localConversations cids
  GetConversationMetadata cid -> do
    logEffect "ConversationStore.GetConversationMetadata"
    embedClient $ conversationMeta cid
  GetGroupInfo cid -> do
    logEffect "ConversationStore.GetGroupInfo"
    embedClient $ getGroupInfo cid
  IsConversationAlive cid -> do
    logEffect "ConversationStore.IsConversationAlive"
    embedClient $ isConvAlive cid
  SelectConversations uid cids -> do
    logEffect "ConversationStore.SelectConversations"
    embedClient $ localConversationIdsOf uid cids
  GetRemoteConversationStatus uid cids -> do
    logEffect "ConversationStore.GetRemoteConversationStatus"
    embedClient $ remoteConversationStatus uid cids
  SetConversationType cid ty -> do
    logEffect "ConversationStore.SetConversationType"
    embedClient $ updateConvType cid ty
  SetConversationName cid value -> do
    logEffect "ConversationStore.SetConversationName"
    embedClient $ updateConvName cid value
  SetConversationAccess cid value -> do
    logEffect "ConversationStore.SetConversationAccess"
    embedClient $ updateConvAccess cid value
  SetConversationReceiptMode cid value -> do
    logEffect "ConversationStore.SetConversationReceiptMode"
    embedClient $ updateConvReceiptMode cid value
  SetConversationMessageTimer cid value -> do
    logEffect "ConversationStore.SetConversationMessageTimer"
    embedClient $ updateConvMessageTimer cid value
  SetConversationGroupPicture cid colour emoji -> do
    logEffect "ConversationStore.SetConversationGroupPicture"
    embedClient $ updateConvGroupPicture cid colour emoji
  SetConversationEpoch cid epoch -> do
    logEffect "ConversationStore.SetConversationEpoch"
    embedClient $ updateConvEpoch cid epoch
  SetConversationCipherSuite cid cs -> do
    logEffect "ConversationStore.SetConversationCipherSuite"
    embedClient $ updateConvCipherSuite cid cs
  DeleteConversation cid -> do
    logEffect "ConversationStore.DeleteConversation"
    embedClient $ deleteConversation cid
  SetGroupInfo cid gib -> do
    logEffect "ConversationStore.SetGroupInfo"
    embedClient $ setGroupInfo cid gib
  AcquireCommitLock gId epoch ttl -> do
    logEffect "ConversationStore.AcquireCommitLock"
    embedClient $ acquireCommitLock gId epoch ttl
  ReleaseCommitLock gId epoch -> do
    logEffect "ConversationStore.ReleaseCommitLock"
    embedClient $ releaseCommitLock gId epoch
  UpdateToMixedProtocol cid ct -> do
    logEffect "ConversationStore.UpdateToMixedProtocol"
    updateToMixedProtocol cid ct
  UpdateToMLSProtocol cid -> do
    logEffect "ConversationStore.UpdateToMLSProtocol"
    updateToMLSProtocol cid
