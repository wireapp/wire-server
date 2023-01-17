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
import qualified Cassandra as Cql
import Cassandra.Util
import Control.Error.Util
import Control.Monad.Trans.Maybe
import Data.ByteString.Conversion
import Data.Domain
import Data.Id
import qualified Data.Map as Map
import Data.Misc
import Data.Qualified
import Data.Range
import qualified Data.Set as Set
import Data.Time.Clock
import Data.UUID.V4 (nextRandom)
import Galley.Cassandra.Access
import Galley.Cassandra.Conversation.MLS
import Galley.Cassandra.Conversation.Members
import qualified Galley.Cassandra.Queries as Cql
import Galley.Cassandra.Store
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
import qualified System.Logger as Log
import qualified UnliftIO
import Wire.API.Conversation hiding (Conversation, Member)
import Wire.API.Conversation.Protocol
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Group
import Wire.API.MLS.PublicGroupState
import Wire.API.MLS.SubConversation

createMLSSelfConversation ::
  Local UserId ->
  Client Conversation
createMLSSelfConversation lusr = do
  let cnv = mlsSelfConvId . tUnqualified $ lusr
      usr = tUnqualified lusr
      nc =
        NewConversation
          { ncMetadata =
              (defConversationMetadata usr) {cnvmType = SelfConv},
            ncUsers = ulFromLocals [toUserRole usr],
            ncProtocol = ProtocolMLSTag
          }
      meta = ncMetadata nc
      gid = convToGroupId . qualifyAs lusr $ cnv
      -- FUTUREWORK: Stop hard-coding the cipher suite
      --
      -- 'CipherSuite 1' corresponds to
      -- 'MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519'.
      cs = MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519
      proto =
        ProtocolMLS
          ConversationMLSData
            { cnvmlsGroupId = gid,
              cnvmlsEpoch = Epoch 0,
              cnvmlsEpochTimestamp = Nothing,
              cnvmlsCipherSuite = cs
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
        Just gid,
        Just cs
      )
    addPrepQuery Cql.insertGroupIdForConversation (gid, cnv, tDomain lusr)

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
      (proto, mgid, mep, mcs) = case ncProtocol nc of
        ProtocolProteusTag -> (ProtocolProteus, Nothing, Nothing, Nothing)
        ProtocolMLSTag ->
          let gid = convToGroupId lcnv
              ep = Epoch 0
              cs = MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519
           in ( ProtocolMLS
                  ConversationMLSData
                    { cnvmlsGroupId = gid,
                      cnvmlsEpoch = ep,
                      cnvmlsEpochTimestamp = Nothing,
                      cnvmlsCipherSuite = cs
                    },
                Just gid,
                Just ep,
                -- FUTUREWORK: Make the cipher suite be a record field in
                -- 'NewConversation' instead of hard-coding it here.
                --
                -- 'CipherSuite 1' corresponds to
                -- 'MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519'.
                Just cs
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
        ncProtocol nc,
        mgid,
        mep,
        mcs
      )
    for_ (cnvmTeam meta) $ \tid -> addPrepQuery Cql.insertTeamConv (tid, tUnqualified lcnv)
    for_ mgid $ \gid -> addPrepQuery Cql.insertGroupIdForConversation (gid, tUnqualified lcnv, tDomain lcnv)
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
    toConvMeta (t, mc, a, r, r', n, i, _, mt, rm, _, _, _, _, _) = do
      c <- mc
      let mbAccessRolesV2 = Set.fromList . Cql.fromSet <$> r'
          accessRoles = maybeRole t $ parseAccessRoles r mbAccessRolesV2
      pure $ ConversationMetadata t c (defAccess t a) accessRoles n i mt rm

getPublicGroupState :: ConvId -> Client (Maybe OpaquePublicGroupState)
getPublicGroupState cid = do
  fmap join $
    runIdentity
      <$$> retry
        x1
        ( query1
            Cql.selectPublicGroupState
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

updateConvEpoch :: ConvId -> Epoch -> Client ()
updateConvEpoch cid epoch = retry x5 $ write Cql.updateConvEpoch (params LocalQuorum (epoch, cid))

setPublicGroupState :: ConvId -> OpaquePublicGroupState -> Client ()
setPublicGroupState conv gib =
  write Cql.updatePublicGroupState (params LocalQuorum (gib, conv))

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
  Members '[Embed IO, Input ClientState, TinyLog] r =>
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
toProtocol (Just ProtocolMLSTag) mgid mepoch mtimestamp mcs =
  ProtocolMLS
    <$> ( ConversationMLSData
            <$> mgid
            -- If there is no epoch in the database, assume the epoch is 0
            <*> (mepoch <|> Just (Epoch 0))
            <*> pure (mepoch `toTimestamp` mtimestamp)
            <*> mcs
        )
  where
    toTimestamp :: Maybe Epoch -> Maybe UTCTime -> Maybe UTCTime
    toTimestamp Nothing _ = Nothing
    toTimestamp (Just (Epoch 0)) _ = Nothing
    toTimestamp (Just _) ts = ts

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
      Maybe CipherSuiteTag
    ) ->
  Maybe Conversation
toConv cid ms remoteMems mconv = do
  (cty, muid, acc, role, roleV2, nme, ti, del, timer, rm, ptag, mgid, mep, mts, mcs) <- mconv
  uid <- muid
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
              cnvmCreator = uid,
              cnvmAccess = defAccess cty acc,
              cnvmAccessRoles = accessRoles,
              cnvmName = nme,
              cnvmTeam = ti,
              cnvmMessageTimer = timer,
              cnvmReceiptMode = rm
            }
      }

setGroupIdForConversation :: GroupId -> Qualified ConvId -> Client ()
setGroupIdForConversation gId conv =
  write Cql.insertGroupIdForConversation (params LocalQuorum (gId, qUnqualified conv, qDomain conv))

lookupConvByGroupId :: GroupId -> Client (Maybe (Qualified ConvOrSubConvId))
lookupConvByGroupId gId =
  toConvOrSubConv <$$> retry x1 (query1 Cql.lookupGroupId (params LocalQuorum (Identity gId)))
  where
    toConvOrSubConv :: (ConvId, Domain, Maybe SubConvId) -> Qualified ConvOrSubConvId
    toConvOrSubConv (convId, domain, mbSubConvId) =
      case mbSubConvId of
        Nothing -> Qualified (Conv convId) domain
        Just subConvId -> Qualified (SubConv convId subConvId) domain

interpretConversationStoreToCassandra ::
  Members '[Embed IO, Input ClientState, TinyLog] r =>
  Sem (ConversationStore ': r) a ->
  Sem r a
interpretConversationStoreToCassandra = interpret $ \case
  CreateConversationId -> Id <$> embed nextRandom
  CreateConversation loc nc -> embedClient $ createConversation loc nc
  CreateMLSSelfConversation lusr -> embedClient $ createMLSSelfConversation lusr
  GetConversation cid -> embedClient $ getConversation cid
  LookupConvByGroupId gId -> embedClient $ lookupConvByGroupId gId
  GetConversations cids -> localConversations cids
  GetConversationMetadata cid -> embedClient $ conversationMeta cid
  GetPublicGroupState cid -> embedClient $ getPublicGroupState cid
  IsConversationAlive cid -> embedClient $ isConvAlive cid
  SelectConversations uid cids -> embedClient $ localConversationIdsOf uid cids
  GetRemoteConversationStatus uid cids -> embedClient $ remoteConversationStatus uid cids
  SetConversationType cid ty -> embedClient $ updateConvType cid ty
  SetConversationName cid value -> embedClient $ updateConvName cid value
  SetConversationAccess cid value -> embedClient $ updateConvAccess cid value
  SetConversationReceiptMode cid value -> embedClient $ updateConvReceiptMode cid value
  SetConversationMessageTimer cid value -> embedClient $ updateConvMessageTimer cid value
  SetConversationEpoch cid epoch -> embedClient $ updateConvEpoch cid epoch
  DeleteConversation cid -> embedClient $ deleteConversation cid
  SetGroupIdForConversation gId cid -> embedClient $ setGroupIdForConversation gId cid
  SetPublicGroupState cid gib -> embedClient $ setPublicGroupState cid gib
  AcquireCommitLock gId epoch ttl -> embedClient $ acquireCommitLock gId epoch ttl
  ReleaseCommitLock gId epoch -> embedClient $ releaseCommitLock gId epoch
