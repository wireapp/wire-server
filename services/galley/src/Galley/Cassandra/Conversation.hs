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
import Data.ByteString.Conversion
import Data.Id
import qualified Data.Map as Map
import Data.Misc
import Data.Qualified
import Data.Range
import qualified Data.Set as Set
import qualified Data.UUID.Tagged as U
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
import Galley.Validation
import Imports
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog
import qualified System.Logger as Log
import qualified UnliftIO
import Wire.API.Conversation hiding (Conversation, Member)
import Wire.API.Conversation.Role (roleNameWireAdmin)

createConversation :: NewConversation -> Client Conversation
createConversation (NewConversation ty usr acc arole name mtid mtimer recpt users role) = do
  conv <- Id <$> liftIO nextRandom
  retry x5 $ case mtid of
    Nothing ->
      write Cql.insertConv (params LocalQuorum (conv, ty, usr, Cql.Set (toList acc), Cql.Set (toList arole), fmap fromRange name, Nothing, mtimer, recpt))
    Just tid -> batch $ do
      setType BatchLogged
      setConsistency LocalQuorum
      addPrepQuery Cql.insertConv (conv, ty, usr, Cql.Set (toList acc), Cql.Set (toList arole), fmap fromRange name, Just tid, mtimer, recpt)
      addPrepQuery Cql.insertTeamConv (tid, conv)
  let newUsers = fmap (,role) (fromConvSize users)
  (lmems, rmems) <- addMembers conv (ulAddLocal (usr, roleNameWireAdmin) newUsers)
  pure $
    Conversation
      { convId = conv,
        convType = ty,
        convCreator = usr,
        convName = fmap fromRange name,
        convAccess = acc,
        convAccessRoles = arole,
        convLocalMembers = lmems,
        convRemoteMembers = rmems,
        convTeam = mtid,
        convDeleted = Nothing,
        convMessageTimer = mtimer,
        convReceiptMode = recpt
      }

createConnectConversation ::
  U.UUID U.V4 ->
  U.UUID U.V4 ->
  Maybe (Range 1 256 Text) ->
  Client Conversation
createConnectConversation a b name = do
  let conv = localOne2OneConvId a b
      a' = Id . U.unpack $ a
  retry x5 $
    write Cql.insertConv (params LocalQuorum (conv, ConnectConv, a', privateOnly, Cql.Set [], fromRange <$> name, Nothing, Nothing, Nothing))
  -- We add only one member, second one gets added later,
  -- when the other user accepts the connection request.
  (lmems, rmems) <- addMembers conv (UserList [a'] [])
  pure
    Conversation
      { convId = conv,
        convType = ConnectConv,
        convCreator = a',
        convName = fmap fromRange name,
        convAccess = [PrivateAccess],
        convAccessRoles = Set.empty,
        convLocalMembers = lmems,
        convRemoteMembers = rmems,
        convTeam = Nothing,
        convDeleted = Nothing,
        convMessageTimer = Nothing,
        convReceiptMode = Nothing
      }

createConnectConversationWithRemote ::
  ConvId ->
  UserId ->
  UserList UserId ->
  Client Conversation
createConnectConversationWithRemote cid creator m = do
  retry x5 $
    write Cql.insertConv (params LocalQuorum (cid, ConnectConv, creator, privateOnly, Cql.Set [], Nothing, Nothing, Nothing, Nothing))
  -- We add only one member, second one gets added later,
  -- when the other user accepts the connection request.
  (lmems, rmems) <- addMembers cid m
  pure
    Conversation
      { convId = cid,
        convType = ConnectConv,
        convCreator = creator,
        convName = Nothing,
        convAccess = [PrivateAccess],
        convAccessRoles = Set.empty,
        convLocalMembers = lmems,
        convRemoteMembers = rmems,
        convTeam = Nothing,
        convDeleted = Nothing,
        convMessageTimer = Nothing,
        convReceiptMode = Nothing
      }

createLegacyOne2OneConversation ::
  Local x ->
  U.UUID U.V4 ->
  U.UUID U.V4 ->
  Maybe (Range 1 256 Text) ->
  Maybe TeamId ->
  Client Conversation
createLegacyOne2OneConversation loc a b name ti = do
  let conv = localOne2OneConvId a b
      a' = Id (U.unpack a)
      b' = Id (U.unpack b)
  createOne2OneConversation
    conv
    (qualifyAs loc a')
    (qUntagged (qualifyAs loc b'))
    name
    ti

createOne2OneConversation ::
  ConvId ->
  Local UserId ->
  Qualified UserId ->
  Maybe (Range 1 256 Text) ->
  Maybe TeamId ->
  Client Conversation
createOne2OneConversation conv self other name mtid = do
  retry x5 $ case mtid of
    Nothing -> write Cql.insertConv (params LocalQuorum (conv, One2OneConv, tUnqualified self, privateOnly, Cql.Set [], fromRange <$> name, Nothing, Nothing, Nothing))
    Just tid -> batch $ do
      setType BatchLogged
      setConsistency LocalQuorum
      addPrepQuery Cql.insertConv (conv, One2OneConv, tUnqualified self, privateOnly, Cql.Set [], fromRange <$> name, Just tid, Nothing, Nothing)
      addPrepQuery Cql.insertTeamConv (tid, conv)
  (lmems, rmems) <- addMembers conv (toUserList self [qUntagged self, other])
  pure
    Conversation
      { convId = conv,
        convType = ConnectConv,
        convCreator = tUnqualified self,
        convName = fmap fromRange name,
        convAccess = [PrivateAccess],
        convAccessRoles = Set.empty,
        convLocalMembers = lmems,
        convRemoteMembers = rmems,
        convTeam = Nothing,
        convDeleted = Nothing,
        convMessageTimer = Nothing,
        convReceiptMode = Nothing
      }

createSelfConversation :: Local UserId -> Maybe (Range 1 256 Text) -> Client Conversation
createSelfConversation lusr name = do
  let usr = tUnqualified lusr
      conv = selfConv usr
      lconv = qualifyAs lusr conv
  retry x5 $
    write Cql.insertConv (params LocalQuorum (conv, SelfConv, usr, privateOnly, Cql.Set [], fromRange <$> name, Nothing, Nothing, Nothing))
  (lmems, rmems) <- addMembers (tUnqualified lconv) (UserList [tUnqualified lusr] [])
  pure
    Conversation
      { convId = conv,
        convType = SelfConv,
        convCreator = usr,
        convName = fmap fromRange name,
        convAccess = [PrivateAccess],
        convAccessRoles = Set.empty,
        convLocalMembers = lmems,
        convRemoteMembers = rmems,
        convTeam = Nothing,
        convDeleted = Nothing,
        convMessageTimer = Nothing,
        convReceiptMode = Nothing
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
  fmap toConvMeta
    <$> retry x1 (query1 Cql.selectConv (params LocalQuorum (Identity conv)))
  where
    toConvMeta (t, c, a, r, r', n, i, _, mt, rm) =
      let mbAccessRolesV2 = Set.fromList . Cql.fromSet <$> r'
          accessRoles = maybeRole t $ parseAccessRoles r mbAccessRolesV2
       in ConversationMetadata t c (defAccess t a) accessRoles n i mt rm

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
                ( \(cId, cType, uId, access, aRolesFromLegacy, aRoles, name, tId, del, timer, rm) ->
                    (cId, (cType, uId, access, aRolesFromLegacy, aRoles, name, tId, del, timer, rm))
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

toConv ::
  ConvId ->
  [LocalMember] ->
  [RemoteMember] ->
  Maybe (ConvType, UserId, Maybe (Cql.Set Access), Maybe AccessRoleLegacy, Maybe (Cql.Set AccessRoleV2), Maybe Text, Maybe TeamId, Maybe Bool, Maybe Milliseconds, Maybe ReceiptMode) ->
  Maybe Conversation
toConv cid mms remoteMems conv =
  f mms <$> conv
  where
    f ms (cty, uid, acc, role, roleV2, nme, ti, del, timer, rm) =
      let mbAccessRolesV2 = Set.fromList . Cql.fromSet <$> roleV2
          accessRoles = maybeRole cty $ parseAccessRoles role mbAccessRolesV2
       in Conversation cid cty uid nme (defAccess cty acc) accessRoles ms remoteMems ti del timer rm

interpretConversationStoreToCassandra ::
  Members '[Embed IO, Input ClientState, TinyLog] r =>
  Sem (ConversationStore ': r) a ->
  Sem r a
interpretConversationStoreToCassandra = interpret $ \case
  CreateConversation nc -> embedClient $ createConversation nc
  CreateConnectConversation x y name ->
    embedClient $ createConnectConversation x y name
  CreateConnectConversationWithRemote cid lusr mems ->
    embedClient $ createConnectConversationWithRemote cid lusr mems
  CreateLegacyOne2OneConversation loc x y name tid ->
    embedClient $ createLegacyOne2OneConversation loc x y name tid
  CreateOne2OneConversation conv self other name mtid ->
    embedClient $ createOne2OneConversation conv self other name mtid
  CreateSelfConversation lusr name ->
    embedClient $ createSelfConversation lusr name
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
