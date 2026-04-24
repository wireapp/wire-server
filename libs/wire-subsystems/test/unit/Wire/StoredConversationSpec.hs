{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

module Wire.StoredConversationSpec where

import Data.Containers.ListUtils (nubOrdOn)
import Data.Domain
import Data.Id
import Data.Qualified
import Data.Set qualified as Set
import Imports
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (Arbitrary (..), Gen, listOf, (==>))
import Wire.API.Conversation
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role
import Wire.API.Federation.API.Galley
  ( RemoteConvMembers (..),
    RemoteConversationView (..),
  )
import Wire.StoredConversation

spec :: Spec
spec = describe "ConversationMapping" do
  prop "conversation view V9 for a valid user is non-empty" $
    \(ConvWithLocalUser c luid) -> isJust (ownConversationView luid c)
  prop "conversation view V10 for a valid user is non-empty" $
    \(ConvWithLocalUser c luid) -> isJust (pure $ conversationView (qualifyAs luid ()) (Just luid) c)
  prop "self user in conversation view is correct" $
    \(ConvWithLocalUser c luid) ->
      fmap (memId . cmSelf . cnvMembers) (ownConversationView luid c)
        == Just (tUntagged luid)
  prop "conversation view metadata is correct" $
    \(ConvWithLocalUser c luid) ->
      fmap cnvMetadata (ownConversationView luid c)
        == Just c.metadata
  prop "other members in conversation view do not contain self" $
    \(ConvWithLocalUser c luid) -> case ownConversationView luid c of
      Nothing -> False
      Just cnv ->
        tUntagged luid
          `notElem` map omQualifiedId (cmOthers (cnvMembers cnv))
  prop "conversation view contains all users" $
    \(ConvWithLocalUser c luid) ->
      fmap (sort . cnvUids) (ownConversationView luid c)
        == Just (sort (convUids (tDomain luid) c))
  prop "conversation view for an invalid user is empty" $
    \(RandomConversation c) luid ->
      notElem (tUnqualified luid) (map (.id_) c.localMembers) ==>
        isNothing (ownConversationView luid c)
  prop "remote conversation view for a valid user is non-empty" $
    \(ConvWithRemoteUser c ruid) dom ->
      qDomain (tUntagged ruid)
        /= dom
        ==> isJust (conversationToRemote dom ruid c)
  prop "self user role in remote conversation view is correct" $
    \(ConvWithRemoteUser c ruid) dom ->
      qDomain (tUntagged ruid)
        /= dom
        ==> fmap (selfRole . (.members)) (conversationToRemote dom ruid c)
          == Just roleNameWireMember
  prop "remote conversation view metadata is correct" $
    \(ConvWithRemoteUser c ruid) dom ->
      qDomain (tUntagged ruid)
        /= dom
        ==> fmap (.metadata) (conversationToRemote dom ruid c)
          == Just c.metadata
  prop "remote conversation view does not contain self" $
    \(ConvWithRemoteUser c ruid) dom -> case conversationToRemote dom ruid c of
      Nothing -> False
      Just rcnv ->
        tUntagged ruid
          `notElem` map omQualifiedId rcnv.members.others

cnvUids :: OwnConversation -> [Qualified UserId]
cnvUids c =
  let mems = cnvMembers c
   in memId (cmSelf mems)
        : map omQualifiedId (cmOthers mems)

convUids :: Domain -> StoredConversation -> [Qualified UserId]
convUids dom c =
  map ((`Qualified` dom) . (.id_)) c.localMembers
    <> map (tUntagged . (.id_)) c.remoteMembers

genLocalMember :: Gen LocalMember
genLocalMember =
  LocalMember
    <$> arbitrary
    <*> pure defMemberStatus
    <*> pure Nothing
    <*> arbitrary

genRemoteMember :: Gen RemoteMember
genRemoteMember = RemoteMember <$> arbitrary <*> pure roleNameWireMember

genConversation :: Gen StoredConversation
genConversation =
  StoredConversation
    <$> arbitrary
    <*> listOf genLocalMember
    <*> listOf genRemoteMember
    <*> genConversationMetadata
    <*> pure ProtocolProteus

genConversationMetadata :: Gen ConversationMetadata
genConversationMetadata =
  ConversationMetadata RegularConv
    <$> arbitrary
    <*> pure []
    <*> pure (Set.fromList [TeamMemberAccessRole, NonTeamMemberAccessRole])
    <*> arbitrary
    <*> pure Nothing
    <*> pure Nothing
    <*> pure Nothing
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

newtype RandomConversation = RandomConversation
  {unRandomConversation :: StoredConversation}
  deriving (Show)

instance Arbitrary RandomConversation where
  arbitrary = RandomConversation <$> genConversation

data ConvWithLocalUser = ConvWithLocalUser StoredConversation (Local UserId)
  deriving (Show)

instance Arbitrary ConvWithLocalUser where
  arbitrary = do
    member <- genLocalMember
    ConvWithLocalUser <$> genConv member <*> genLocal member.id_
    where
      genLocal :: x -> Gen (Local x)
      genLocal v = flip toLocalUnsafe v <$> arbitrary
      genConv m = uniqueMembers m . unRandomConversation <$> arbitrary
      uniqueMembers :: LocalMember -> StoredConversation -> StoredConversation
      uniqueMembers m c =
        c {localMembers = nubOrdOn (.id_) (m : c.localMembers)}

data ConvWithRemoteUser = ConvWithRemoteUser StoredConversation (Remote UserId)
  deriving (Show)

instance Arbitrary ConvWithRemoteUser where
  arbitrary = do
    member <- genRemoteMember
    ConvWithRemoteUser <$> genConv member <*> pure member.id_
    where
      genConv m = uniqueMembers m . unRandomConversation <$> arbitrary
      uniqueMembers :: RemoteMember -> StoredConversation -> StoredConversation
      uniqueMembers m c =
        c {remoteMembers = nubOrdOn (.id_) (m : c.remoteMembers)}
