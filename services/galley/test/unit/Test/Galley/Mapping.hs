{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

module Test.Galley.Mapping where

import Data.Containers.ListUtils (nubOrdOn)
import Data.Domain
import Data.Id
import Data.Qualified
import Galley.API.Mapping
import qualified Galley.Data as Data
import Galley.Types.Conversations.Members
import Imports
import Test.Tasty
import Test.Tasty.QuickCheck
import Wire.API.Conversation
import Wire.API.Conversation.Role
import Wire.API.Federation.API.Galley
  ( RemoteConvMembers (..),
    RemoteConversation (..),
  )

tests :: TestTree
tests =
  testGroup
    "ConversationMapping"
    [ testProperty "conversation view for a valid user is non-empty" $
        \(ConvWithLocalUser c luid) -> isJust (conversationViewMaybe luid c),
      testProperty "self user in conversation view is correct" $
        \(ConvWithLocalUser c luid) ->
          fmap (memId . cmSelf . cnvMembers) (conversationViewMaybe luid c)
            == Just (qUntagged luid),
      testProperty "conversation view metadata is correct" $
        \(ConvWithLocalUser c luid) ->
          fmap cnvMetadata (conversationViewMaybe luid c)
            == Just (Data.convMetadata c),
      testProperty "other members in conversation view do not contain self" $
        \(ConvWithLocalUser c luid) -> case conversationViewMaybe luid c of
          Nothing -> False
          Just cnv ->
            not
              ( qUntagged luid
                  `elem` (map omQualifiedId (cmOthers (cnvMembers cnv)))
              ),
      testProperty "conversation view contains all users" $
        \(ConvWithLocalUser c luid) ->
          fmap (sort . cnvUids) (conversationViewMaybe luid c)
            == Just (sort (convUids (tDomain luid) c)),
      testProperty "conversation view for an invalid user is empty" $
        \(RandomConversation c) luid ->
          not (elem (tUnqualified luid) (map lmId (Data.convLocalMembers c)))
            ==> isNothing (conversationViewMaybe luid c),
      testProperty "remote conversation view for a valid user is non-empty" $
        \(ConvWithRemoteUser c ruid) dom ->
          qDomain (qUntagged ruid) /= dom
            ==> isJust (conversationToRemote dom ruid c),
      testProperty "self user role in remote conversation view is correct" $
        \(ConvWithRemoteUser c ruid) dom ->
          qDomain (qUntagged ruid) /= dom
            ==> fmap (rcmSelfRole . rcnvMembers) (conversationToRemote dom ruid c)
              == Just roleNameWireMember,
      testProperty "remote conversation view metadata is correct" $
        \(ConvWithRemoteUser c ruid) dom ->
          qDomain (qUntagged ruid) /= dom
            ==> fmap (rcnvMetadata) (conversationToRemote dom ruid c)
              == Just (Data.convMetadata c),
      testProperty "remote conversation view does not contain self" $
        \(ConvWithRemoteUser c ruid) dom -> case conversationToRemote dom ruid c of
          Nothing -> False
          Just rcnv ->
            not
              ( qUntagged ruid
                  `elem` (map omQualifiedId (rcmOthers (rcnvMembers rcnv)))
              )
    ]

cnvUids :: Conversation -> [Qualified UserId]
cnvUids c =
  let mems = cnvMembers c
   in memId (cmSelf mems) :
      map omQualifiedId (cmOthers mems)

convUids :: Domain -> Data.Conversation -> [Qualified UserId]
convUids dom c =
  map ((`Qualified` dom) . lmId) (Data.convLocalMembers c)
    <> map (qUntagged . rmId) (Data.convRemoteMembers c)

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
    <*> pure RegularConv
    <*> arbitrary
    <*> arbitrary
    <*> pure []
    <*> pure ActivatedAccessRole
    <*> listOf genLocalMember
    <*> listOf genRemoteMember
    <*> pure Nothing
    <*> pure (Just False)
    <*> pure Nothing
    <*> pure Nothing

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
