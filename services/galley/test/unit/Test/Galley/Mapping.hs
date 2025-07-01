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

module Test.Galley.Mapping where

import Data.Containers.ListUtils (nubOrdOn)
import Data.Domain
import Data.Id
import Data.Qualified
import Data.Set qualified as Set
import Galley.API.Error (InternalError)
import Galley.API.Mapping
import Galley.Data.Conversation qualified as Data
import Galley.Types.Conversations.Members
import Imports
import Polysemy (Sem)
import Polysemy qualified as P
import Polysemy.Error qualified as P
import Polysemy.TinyLog qualified as P
import Test.Tasty
import Test.Tasty.QuickCheck
import Wire.API.Conversation
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role
import Wire.API.Federation.API.Galley
  ( RemoteConvMembers (..),
    RemoteConversationV2 (..),
  )
import Wire.Sem.Logger qualified as P

run :: Sem '[P.TinyLog, P.Error InternalError] a -> Either InternalError a
run = P.run . P.runError . P.discardLogs

tests :: TestTree
tests =
  testGroup
    "ConversationMapping"
    [ testProperty "conversation view V9 for a valid user is non-empty" $
        \(ConvWithLocalUser c luid) -> isRight (run (conversationViewV9 luid c)),
      testProperty "conversation view V10 for a valid user is non-empty" $
        \(ConvWithLocalUser c luid) -> isRight (run (pure $ conversationView luid c)),
      testProperty "self user in conversation view is correct" $
        \(ConvWithLocalUser c luid) ->
          fmap (memId . cmSelf . cnvMembers) (run (conversationViewV9 luid c))
            == Right (tUntagged luid),
      testProperty "conversation view metadata is correct" $
        \(ConvWithLocalUser c luid) ->
          fmap cnvMetadata (run (conversationViewV9 luid c))
            == Right (Data.convMetadata c),
      testProperty "other members in conversation view do not contain self" $
        \(ConvWithLocalUser c luid) -> case run $ conversationViewV9 luid c of
          Left _ -> False
          Right cnv ->
            tUntagged luid
              `notElem` map omQualifiedId (cmOthers (cnvMembers cnv)),
      testProperty "conversation view contains all users" $
        \(ConvWithLocalUser c luid) ->
          fmap (sort . cnvUids) (run (conversationViewV9 luid c))
            == Right (sort (convUids (tDomain luid) c)),
      testProperty "conversation view for an invalid user is empty" $
        \(RandomConversation c) luid ->
          notElem (tUnqualified luid) (map lmId (Data.convLocalMembers c)) ==>
            isLeft (run (conversationViewV9 luid c)),
      testProperty "remote conversation view for a valid user is non-empty" $
        \(ConvWithRemoteUser c ruid) dom ->
          qDomain (tUntagged ruid) /= dom ==>
            isJust (conversationToRemote dom ruid c),
      testProperty "self user role in remote conversation view is correct" $
        \(ConvWithRemoteUser c ruid) dom ->
          qDomain (tUntagged ruid) /= dom ==>
            fmap (selfRole . (.members)) (conversationToRemote dom ruid c)
              == Just roleNameWireMember,
      testProperty "remote conversation view metadata is correct" $
        \(ConvWithRemoteUser c ruid) dom ->
          qDomain (tUntagged ruid) /= dom ==>
            fmap (.metadata) (conversationToRemote dom ruid c)
              == Just (Data.convMetadata c),
      testProperty "remote conversation view does not contain self" $
        \(ConvWithRemoteUser c ruid) dom -> case conversationToRemote dom ruid c of
          Nothing -> False
          Just rcnv ->
            tUntagged ruid
              `notElem` map omQualifiedId rcnv.members.others
    ]

cnvUids :: ConversationV9 -> [Qualified UserId]
cnvUids c =
  let mems = cnvMembers c
   in memId (cmSelf mems)
        : map omQualifiedId (cmOthers mems)

convUids :: Domain -> Data.Conversation -> [Qualified UserId]
convUids dom c =
  map ((`Qualified` dom) . lmId) (Data.convLocalMembers c)
    <> map (tUntagged . rmId) (Data.convRemoteMembers c)

genLocalMember :: Gen LocalMember
genLocalMember =
  LocalMember
    <$> arbitrary
    <*> pure defMemberStatus
    <*> pure Nothing
    <*> arbitrary

genRemoteMember :: Gen RemoteMember
genRemoteMember = RemoteMember <$> arbitrary <*> pure roleNameWireMember

genConversation :: Gen Data.Conversation
genConversation =
  Data.Conversation
    <$> arbitrary
    <*> listOf genLocalMember
    <*> listOf genRemoteMember
    <*> pure False
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

newtype RandomConversation = RandomConversation
  {unRandomConversation :: Data.Conversation}
  deriving (Show)

instance Arbitrary RandomConversation where
  arbitrary = RandomConversation <$> genConversation

data ConvWithLocalUser = ConvWithLocalUser Data.Conversation (Local UserId)
  deriving (Show)

instance Arbitrary ConvWithLocalUser where
  arbitrary = do
    member <- genLocalMember
    ConvWithLocalUser <$> genConv member <*> genLocal (lmId member)
    where
      genLocal :: x -> Gen (Local x)
      genLocal v = flip toLocalUnsafe v <$> arbitrary
      genConv m = uniqueMembers m . unRandomConversation <$> arbitrary
      uniqueMembers :: LocalMember -> Data.Conversation -> Data.Conversation
      uniqueMembers m c =
        c {Data.convLocalMembers = nubOrdOn lmId (m : Data.convLocalMembers c)}

data ConvWithRemoteUser = ConvWithRemoteUser Data.Conversation (Remote UserId)
  deriving (Show)

instance Arbitrary ConvWithRemoteUser where
  arbitrary = do
    member <- genRemoteMember
    ConvWithRemoteUser <$> genConv member <*> pure (rmId member)
    where
      genConv m = uniqueMembers m . unRandomConversation <$> arbitrary
      uniqueMembers :: RemoteMember -> Data.Conversation -> Data.Conversation
      uniqueMembers m c =
        c {Data.convRemoteMembers = nubOrdOn rmId (m : Data.convRemoteMembers c)}
