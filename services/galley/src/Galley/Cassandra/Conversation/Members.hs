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

module Galley.Cassandra.Conversation.Members where

import Cassandra
import Data.Domain
import Data.Id
import qualified Data.List.Extra as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as Map
import Data.Qualified
import Galley.Data.Instances ()
import qualified Galley.Data.Queries as Cql
import Galley.Types.Conversations.Members
import Galley.Types.ToUserRole
import Galley.Types.UserList
import Imports
import Wire.API.Conversation.Member
import Wire.API.Conversation.Role
import Wire.API.Provider.Service

-- | Add members to a local conversation.
-- Conversation is local, so we can add any member to it (including remote ones).
-- When the role is not specified, it defaults to admin.
-- Please make sure the conversation doesn't exceed the maximum size!
addMembers ::
  ToUserRole a =>
  ConvId ->
  UserList a ->
  Client ([LocalMember], [RemoteMember])
addMembers conv (fmap toUserRole -> UserList lusers rusers) = do
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

removeLocalMembersFromLocalConv :: ConvId -> NonEmpty UserId -> Client ()
removeLocalMembersFromLocalConv cnv victims = do
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency Quorum
    for_ victims $ \victim -> do
      addPrepQuery Cql.removeMember (cnv, victim)
      addPrepQuery Cql.deleteUserConv (victim, cnv)

removeRemoteMembersFromLocalConv :: ConvId -> NonEmpty (Remote UserId) -> Client ()
removeRemoteMembersFromLocalConv cnv victims = do
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency Quorum
    for_ victims $ \(qUntagged -> Qualified uid domain) ->
      addPrepQuery Cql.removeRemoteMember (cnv, domain, uid)

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

members :: ConvId -> Client [LocalMember]
members = fmap concat . liftClient . memberLists . pure

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

toRemoteMember :: UserId -> Domain -> RoleName -> RemoteMember
toRemoteMember u d = RemoteMember (toRemoteUnsafe d u)

newRemoteMemberWithRole :: Remote (UserId, RoleName) -> RemoteMember
newRemoteMemberWithRole ur@(qUntagged -> (Qualified (u, r) _)) =
  RemoteMember
    { rmId = qualifyAs ur u,
      rmConvRoleName = r
    }

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

lookupRemoteMembers :: ConvId -> Client [RemoteMember]
lookupRemoteMembers conv = join <$> remoteMemberLists [conv]
