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

module Galley.API.BackgroundProcesses where

import Cassandra (ClientState, Consistency (LocalQuorum), Page (hasMore, nextPage, result), paginate, paramsP)
import Control.Lens hiding (Getter, Setter, (.=))
import Data.List.NonEmpty qualified as N
import Data.Map qualified as Map
import Data.Qualified
import Data.Range
import Galley.API.Federation (insertIntoMap)
import Galley.API.Util
import Galley.Cassandra.Queries qualified as Q
import Galley.Cassandra.Store (embedClient)
import Galley.Data.Conversation (convTeam)
import Galley.Data.Conversation.Types
import Galley.Data.Scope (Scope (ReusableCode))
import Galley.Effects
import Galley.Effects.CodeStore qualified as E
import Galley.Effects.ConversationStore
import Galley.Effects.ConversationStore qualified as E
import Galley.Effects.MemberStore qualified as E
import Galley.Effects.TeamStore qualified as E
import Galley.Types.Conversations.Members (RemoteMember (RemoteMember, rmId))
import Galley.Types.UserList
import Imports hiding (head)
import Polysemy
import Polysemy.Input
import Wire.API.Conversation hiding (Member)

-- | Remove all conversations and members from the local domain. This must only be run in a background process
unsafeRemoveRemoteMembersFromLocalConversation ::
  forall r a.
  ( Member (Input ClientState) r,
    Member (Embed IO) r,
    Member MemberStore r,
    Member ConversationStore r,
    Member CodeStore r,
    Member TeamStore r
  ) =>
  Range 1 2000 Int32 ->
  Local a ->
  Remote a ->
  Sem r ()
unsafeRemoveRemoteMembersFromLocalConversation (fromRange -> maxPage) lDom rDom = do
  let dom = tDomain rDom
  remoteUsers <-
    mkConvMem <$$> do
      page <-
        embedClient $
          paginate Q.selectRemoteMembersByDomain $
            paramsP LocalQuorum (Identity dom) maxPage
      getPaginatedData page
  let lCnvMap = foldr insertIntoMap mempty remoteUsers
  for_ (Map.toList lCnvMap) $ \(cnvId, rUsers) -> do
    getConversation cnvId
      >>= maybe (pure () {- conv already gone, nothing to do -}) (delConv rUsers)
  where
    mkConvMem (convId, usr, role) = (convId, RemoteMember (rDom $> usr) role)
    delConv ::
      N.NonEmpty RemoteMember ->
      Galley.Data.Conversation.Types.Conversation ->
      Sem r ()
    delConv rUsers conv =
      do
        let lConv = lDom $> conv
        let presentVictims = filter (isConvMemberL lConv) (toList (tUntagged . rmId <$> rUsers))
        E.deleteMembers conv.convId (toUserList lConv presentVictims)
        -- Check if the conversation if type 2 or 3, one-on-one conversations.
        -- If it is, then we need to remove the entire conversation as users
        -- aren't able to delete those types of conversations themselves.
        -- Check that we are in a type 2 or a type 3 conversation
        when (cnvmType (convMetadata conv) `elem` [One2OneConv, ConnectConv]) $ do
          -- If we are, delete it.
          key <- E.makeKey conv.convId
          E.deleteCode key ReusableCode
          case convTeam conv of
            Nothing -> E.deleteConversation conv.convId
            Just tid -> E.deleteTeamConversation tid conv.convId

-- | Remove all conversations and members from the remote domain. This must only be run in a background process
unsafeRemoveLocalMembersFromRemoteConversation ::
  ( Member (Input ClientState) r,
    Member (Embed IO) r,
    Member MemberStore r
  ) =>
  Range 1 2000 Int32 ->
  Remote a ->
  Sem r ()
unsafeRemoveLocalMembersFromRemoteConversation (fromRange -> maxPage) rDom = do
  let dom = tDomain rDom
  remoteConvs <-
    foldr insertIntoMap mempty <$> do
      page <-
        embedClient $
          paginate Q.selectLocalMembersByDomain $
            paramsP LocalQuorum (Identity dom) maxPage
      getPaginatedData page
  for_ (Map.toList remoteConvs) $ \(cnv, lUsers) -> do
    E.deleteMembersInRemoteConversation (toRemoteUnsafe dom cnv) (N.toList lUsers)

getPaginatedData ::
  ( Member (Input ClientState) r,
    Member (Embed IO) r
  ) =>
  Page a ->
  Sem r [a]
getPaginatedData page
  | hasMore page =
      (result page <>) <$> do
        getPaginatedData <=< embedClient $ nextPage page
  | otherwise = pure $ result page
