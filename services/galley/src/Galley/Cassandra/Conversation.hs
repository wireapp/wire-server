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
import qualified Crypto.Hash.BLAKE2.BLAKE2b as Blake2
import Data.ByteString.Conversion
import Data.Domain
import Data.Id
import qualified Data.Map as Map
import Data.Misc
import Data.Qualified
import Data.Range
import qualified Data.Set as Set
import Data.UUID.V4 (nextRandom)
import Galley.Cassandra.Access
import Galley.Cassandra.Conversation.Members
import qualified Galley.Cassandra.Queries as Cql
import Galley.Cassandra.Store
import Galley.Data.Conversation
import Galley.Data.Conversation.Types
import Galley.Effects.ConversationStore (ConversationStore (..))
import Galley.Types.Conversations.Members
import Galley.Types.UserList
import Imports
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog
import qualified System.Logger as Log
import System.Random
import qualified UnliftIO
import Wire.API.Conversation hiding (Conversation, Member)
import Wire.API.Conversation.Protocol

createConversation :: Local ConvId -> NewConversation -> Client Conversation
createConversation lcnv nc = do
  let meta = ncMetadata nc
  (proto, mgid, mep) <- case ncProtocol nc of
    ProtocolProteusTag -> pure (ProtocolProteus, Nothing, Nothing)
    ProtocolMLSTag -> do
      gid <- liftIO $ toGroupId (tUnqualified lcnv, tDomain lcnv)
      pure
        ( ProtocolMLS
            ConversationMLSData
              { cnvmlsGroupId = gid,
                cnvmlsEpoch = Epoch 0
              },
          Just gid,
          Just (Epoch 0)
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
        mep
      )
    for_ (cnvmTeam meta) $ \tid -> addPrepQuery Cql.insertTeamConv (tid, tUnqualified lcnv)
    for_ mgid $ \gid -> addPrepQuery Cql.insertGroupId (gid, tUnqualified lcnv, tDomain lcnv)
  (lmems, rmems) <- addMembers (tUnqualified lcnv) (ncUsers nc)
  pure
    Conversation
      { convId = tUnqualified lcnv,
        convLocalMembers = lmems,
        convRemoteMembers = rmems,
        convDeleted = Nothing,
        convMetadata = meta,
        convProtocol = proto
      }
  where
    toGroupId :: MonadIO m => (ConvId, Domain) -> m GroupId
    toGroupId (cId, d) = do
      g <- newStdGen
      let (len, _) = genWord8 g
      -- The length can be at most 256 bytes
      pure
        . GroupId
        . Blake2.hash (fromIntegral len) mempty
        $ toByteString' cId <> toByteString' d

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
    toConvMeta (t, c, a, r, r', n, i, _, mt, rm, _, _, _) = do
      let mbAccessRolesV2 = Set.fromList . Cql.fromSet <$> r'
          accessRoles = maybeRole t $ parseAccessRoles r mbAccessRolesV2
      pure $ ConversationMetadata t c (defAccess t a) accessRoles n i mt rm

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

getConversation :: ConvId -> Client (Maybe Conversation)
getConversation conv = do
  cdata <- UnliftIO.async $ retry x1 (query1 Cql.selectConv (params LocalQuorum (Identity conv)))
  remoteMems <- UnliftIO.async $ lookupRemoteMembers conv
  mbConv <-
    toConv conv
      <$> members conv
      <*> UnliftIO.wait remoteMems
      <*> UnliftIO.wait cdata
  conversationGC mbConv

{- "Garbage collect" the conversation, i.e. the conversation may be
   marked as deleted, in which case we delete it and return Nothing -}
conversationGC ::
  Maybe Conversation ->
  Client (Maybe Conversation)
conversationGC conv = case join (convDeleted <$> conv) of
  (Just True) -> do
    sequence_ $ deleteConversation . convId <$> conv
    return Nothing
  _ -> return conv

localConversations ::
  (Members '[Embed IO, Input ClientState, TinyLog] r) =>
  [ConvId] ->
  Sem r [Conversation]
localConversations [] = return []
localConversations ids = do
  cs <- embedClient $ do
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
      cs <- retry x1 $ query Cql.selectConvs (params LocalQuorum (Identity ids))
      let m =
            Map.fromList $
              map
                ( \(cId, cType, uId, access, aRolesFromLegacy, aRoles, name, tId, del, timer, rm, p, gid, mep) ->
                    (cId, (cType, uId, access, aRolesFromLegacy, aRoles, name, tId, del, timer, rm, p, gid, mep))
                )
                cs
      return $ map (`Map.lookup` m) ids
    flatten (i, c) cc = case c of
      Nothing -> do
        warn $ Log.msg ("No conversation for: " <> toByteString i)
        return cc
      Just c' -> return (c' : cc)

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

toProtocol :: Maybe ProtocolTag -> Maybe GroupId -> Maybe Epoch -> Maybe Protocol
toProtocol Nothing _ _ = Just ProtocolProteus
toProtocol (Just ProtocolProteusTag) _ _ = Just ProtocolProteus
toProtocol (Just ProtocolMLSTag) mgid mepoch =
  ProtocolMLS <$> (ConversationMLSData <$> mgid <*> mepoch)

toConv ::
  ConvId ->
  [LocalMember] ->
  [RemoteMember] ->
  Maybe (ConvType, UserId, Maybe (Cql.Set Access), Maybe AccessRoleLegacy, Maybe (Cql.Set AccessRoleV2), Maybe Text, Maybe TeamId, Maybe Bool, Maybe Milliseconds, Maybe ReceiptMode, Maybe ProtocolTag, Maybe GroupId, Maybe Epoch) ->
  Maybe Conversation
toConv cid ms remoteMems mconv = do
  (cty, uid, acc, role, roleV2, nme, ti, del, timer, rm, ptag, mgid, mep) <- mconv
  let mbAccessRolesV2 = Set.fromList . Cql.fromSet <$> roleV2
      accessRoles = maybeRole cty $ parseAccessRoles role mbAccessRolesV2
  proto <- toProtocol ptag mgid mep
  pure
    Conversation
      { convId = cid,
        convDeleted = del,
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

mapGroupId :: GroupId -> Qualified ConvId -> Client ()
mapGroupId gId conv =
  write Cql.insertGroupId (params LocalQuorum (gId, qUnqualified conv, qDomain conv))

lookupGroupId :: GroupId -> Client (Maybe (Qualified ConvId))
lookupGroupId gId =
  uncurry Qualified <$$> retry x1 (query1 Cql.lookupGroupId (params LocalQuorum (Identity gId)))

interpretConversationStoreToCassandra ::
  Members '[Embed IO, Input ClientState, TinyLog] r =>
  Sem (ConversationStore ': r) a ->
  Sem r a
interpretConversationStoreToCassandra = interpret $ \case
  CreateConversationId -> Id <$> embed nextRandom
  CreateConversation loc nc -> embedClient $ createConversation loc nc
  GetConversation cid -> embedClient $ getConversation cid
  GetConversations cids -> localConversations cids
  GetConversationMetadata cid -> embedClient $ conversationMeta cid
  IsConversationAlive cid -> embedClient $ isConvAlive cid
  SelectConversations uid cids -> embedClient $ localConversationIdsOf uid cids
  GetRemoteConversationStatus uid cids -> embedClient $ remoteConversationStatus uid cids
  SetConversationType cid ty -> embedClient $ updateConvType cid ty
  SetConversationName cid value -> embedClient $ updateConvName cid value
  SetConversationAccess cid value -> embedClient $ updateConvAccess cid value
  SetConversationReceiptMode cid value -> embedClient $ updateConvReceiptMode cid value
  SetConversationMessageTimer cid value -> embedClient $ updateConvMessageTimer cid value
  DeleteConversation cid -> embedClient $ deleteConversation cid
  GetConversationIdByGroupId gId -> embedClient $ lookupGroupId gId
  SetGroupId gId cid -> embedClient $ mapGroupId gId cid
