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

import Data.Domain
import Data.Id
import Data.Qualified
import Data.Tagged
import Galley.API.Mapping
import qualified Galley.Data as Data
import Galley.Types.Conversations.Members
import Imports
import Test.Tasty
import Test.Tasty.QuickCheck
-- import Test.Tasty.HUnit
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
        \(ConvWithLocalUser c uid) dom -> isJust (conversationViewMaybe dom uid c),
      testProperty "self user in conversation view is correct" $
        \(ConvWithLocalUser c uid) dom ->
          fmap (memId . cmSelf . cnvMembers) (conversationViewMaybe dom uid c)
            == Just uid,
      testProperty "conversation view metadata is correct" $
        \(ConvWithLocalUser c uid) dom ->
          fmap cnvMetadata (conversationViewMaybe dom uid c)
            == Just (Data.convMetadata dom c),
      testProperty "other members in conversation view do not contain self" $
        \(ConvWithLocalUser c uid) dom -> case conversationViewMaybe dom uid c of
          Nothing -> False
          Just cnv ->
            not
              ( Qualified uid dom
                  `elem` (map omQualifiedId (cmOthers (cnvMembers cnv)))
              ),
      testProperty "conversation view contains all users" $
        \(ConvWithLocalUser c uid) dom ->
          fmap (sort . cnvUids dom) (conversationViewMaybe dom uid c)
            == Just (sort (convUids dom c)),
      testProperty "conversation view for an invalid user is empty" $
        \(RandomConversation c) dom uid ->
          not (elem uid (map lmId (Data.convLocalMembers c)))
            ==> isNothing (conversationViewMaybe dom uid c),
      testProperty "remote conversation view for a valid user is non-empty" $
        \(ConvWithRemoteUser c ruid) dom ->
          qDomain (unTagged ruid) /= dom
            ==> isJust (conversationToRemote dom ruid c),
      testProperty "self user role in remote conversation view is correct" $
        \(ConvWithRemoteUser c ruid) dom ->
          qDomain (unTagged ruid) /= dom
            ==> fmap (rcmSelfRole . rcnvMembers) (conversationToRemote dom ruid c)
              == Just roleNameWireMember,
      testProperty "remote conversation view metadata is correct" $
        \(ConvWithRemoteUser c ruid) dom ->
          qDomain (unTagged ruid) /= dom
            ==> fmap (rcnvMetadata) (conversationToRemote dom ruid c)
              == Just (Data.convMetadata dom c),
      testProperty "remote conversation view does not contain self" $
        \(ConvWithRemoteUser c ruid) dom -> case conversationToRemote dom ruid c of
          Nothing -> False
          Just rcnv ->
            not
              ( unTagged ruid
                  `elem` (map omQualifiedId (rcmOthers (rcnvMembers rcnv)))
              )
    ]

cnvUids :: Domain -> Conversation -> [Qualified UserId]
cnvUids dom c =
  let mems = cnvMembers c
   in Qualified (memId (cmSelf mems)) dom :
      map omQualifiedId (cmOthers mems)

convUids :: Domain -> Data.Conversation -> [Qualified UserId]
convUids dom c =
  map ((`Qualified` dom) . lmId) (Data.convLocalMembers c)
    <> map (unTagged . rmId) (Data.convRemoteMembers c)

genLocalMember :: Gen LocalMember
genLocalMember =
  LocalMember
    <$> arbitrary
    <*> pure defMemberStatus
    <*> pure Nothing
    <*> arbitrary

genRemoteMember :: Gen RemoteMember
genRemoteMember = RemoteMember <$> arbitrary <*> pure roleNameWireMember

genConversation :: [LocalMember] -> [RemoteMember] -> Gen Data.Conversation
genConversation locals remotes =
  Data.Conversation
    <$> arbitrary
    <*> pure RegularConv
    <*> arbitrary
    <*> arbitrary
    <*> pure []
    <*> pure ActivatedAccessRole
    <*> pure locals
    <*> pure remotes
    <*> pure Nothing
    <*> pure (Just False)
    <*> pure Nothing
    <*> pure Nothing

newtype RandomConversation = RandomConversation Data.Conversation
  deriving (Show)

instance Arbitrary RandomConversation where
  arbitrary =
    RandomConversation <$> do
      locals <- listOf genLocalMember
      remotes <- listOf genRemoteMember
      genConversation locals remotes

data ConvWithLocalUser = ConvWithLocalUser Data.Conversation UserId
  deriving (Show)

instance Arbitrary ConvWithLocalUser where
  arbitrary = do
    RandomConversation conv <- arbitrary
    member <- genLocalMember
    let conv'
          | lmId member `elem` map lmId (Data.convLocalMembers conv) =
            conv
          | otherwise =
            conv {Data.convLocalMembers = member : Data.convLocalMembers conv}
    pure $ ConvWithLocalUser conv' (lmId member)

data ConvWithRemoteUser = ConvWithRemoteUser Data.Conversation (Remote UserId)
  deriving (Show)

instance Arbitrary ConvWithRemoteUser where
  arbitrary = do
    RandomConversation conv <- arbitrary
    member <- genRemoteMember
    let conv'
          | rmId member `elem` map rmId (Data.convRemoteMembers conv) =
            conv
          | otherwise =
            conv {Data.convRemoteMembers = member : Data.convRemoteMembers conv}
    pure $ ConvWithRemoteUser conv' (rmId member)
