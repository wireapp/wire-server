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
import Galley.API ()
import Galley.API.Mapping
import qualified Galley.Data as Data
import Galley.Types (LocalMember, RemoteMember)
import qualified Galley.Types.Conversations.Members as I
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.Conversation
import Wire.API.Conversation.Role (roleNameWireAdmin)

tests :: TestTree
tests =
  testGroup
    "ConversationMapping"
    [ testCase "Alice@A Conv@A" runMappingSimple,
      testCase "Alice@A Conv@A requester=not a member@A" runMappingNotAMemberA,
      testCase "Alice@A Conv@A requester=not a member@B" runMappingNotAMemberB,
      testCase "Alice@A Conv@A Bob@B" runMappingRemoteUser,
      testCase "Alice@A Conv@B Bob@B" runMappingRemoteConv,
      testCase "Alice@A Conv@B Bob@B bobUUID=aliceUUID" runMappingSameUnqualifiedUUID
    ]

runMappingSimple :: HasCallStack => IO ()
runMappingSimple = do
  let convDomain = Domain "backendA.example.com"
  let userDomain = Domain "backendA.example.com"
  alice <- randomId
  let requester = Qualified alice userDomain
  let expectedSelf = Just $ mkMember requester
  let expectedOthers = Just []

  let locals = [mkInternalMember requester]
  let remotes = []
  conv <- mkInternalConv locals remotes
  let actual = cnvMembers <$> conversationViewMaybeQualified convDomain requester conv

  assertEqual "self:" expectedSelf (cmSelf <$> actual)
  assertEqual "others:" expectedOthers (cmOthers <$> actual)

runMappingNotAMemberA :: HasCallStack => IO ()
runMappingNotAMemberA = do
  let convDomain = Domain "backendA.example.com"
  let aliceDomain = Domain "backendA.example.com"
  alice <- flip Qualified aliceDomain <$> randomId
  requester <- flip Qualified aliceDomain <$> randomId

  let locals = [mkInternalMember alice]
  let remotes = []
  conv <- mkInternalConv locals remotes
  let actual = cnvMembers <$> conversationViewMaybeQualified convDomain requester conv

  assertEqual "members:" Nothing actual

runMappingNotAMemberB :: HasCallStack => IO ()
runMappingNotAMemberB = do
  let convDomain = Domain "backendA.example.com"
  let aliceDomain = Domain "backendA.example.com"
  let requesterDomain = Domain "backendB.example.com"
  alice <- flip Qualified aliceDomain <$> randomId
  requester <- flip Qualified requesterDomain <$> randomId

  let locals = [mkInternalMember alice]
  let remotes = []
  conv <- mkInternalConv locals remotes
  let actual = cnvMembers <$> conversationViewMaybeQualified convDomain requester conv

  assertEqual "members:" Nothing actual

runMappingRemoteUser :: HasCallStack => IO ()
runMappingRemoteUser = do
  let aliceDomain = Domain "backendA.example.com"
  let convDomain = Domain "backendA.example.com"
  let bobDomain = Domain "backendB.example.com"
  alice <- flip Qualified aliceDomain <$> randomId
  bob <- flip Qualified bobDomain <$> randomId
  let expectedSelf = Just $ mkMember alice
  let expectedOthers = Just [mkOtherMember bob]

  let locals = [mkInternalMember alice]
  let remotes = [mkRemoteMember bob]
  conv <- mkInternalConv locals remotes
  let actual = cnvMembers <$> conversationViewMaybeQualified convDomain alice conv

  assertEqual "self:" expectedSelf (cmSelf <$> actual)
  assertEqual "others:" expectedOthers (cmOthers <$> actual)

runMappingRemoteConv :: HasCallStack => IO ()
runMappingRemoteConv = do
  let aliceDomain = Domain "backendA.example.com"
  let convDomain = Domain "backendB.example.com"
  let bobDomain = Domain "backendB.example.com"
  alice <- flip Qualified aliceDomain <$> randomId
  bob <- flip Qualified bobDomain <$> randomId
  let expectedSelf = Just $ mkMember alice
  let expectedOthers = Just [mkOtherMember bob]

  let locals = [mkInternalMember bob]
  let remotes = [mkRemoteMember alice]
  conv <- mkInternalConv locals remotes
  let actual = cnvMembers <$> conversationViewMaybeQualified convDomain alice conv

  assertEqual "self:" expectedSelf (cmSelf <$> actual)
  assertEqual "others:" expectedOthers (cmOthers <$> actual)

-- Here we expect the conversationView to return nothing, because Alice (the
-- requester) is not part of the conversation (Her unqualified UUID is part of
-- the conversation, but the function should catch this possibly malicious
-- edge case)
runMappingSameUnqualifiedUUID :: HasCallStack => IO ()
runMappingSameUnqualifiedUUID = do
  let aliceDomain = Domain "backendA.example.com"
  let convDomain = Domain "backendB.example.com"
  let bobDomain = Domain "backendB.example.com"
  uuid <- randomId
  let alice = Qualified uuid aliceDomain
  let bob = Qualified uuid bobDomain

  let locals = [mkInternalMember bob]
  let remotes = []
  conv <- mkInternalConv locals remotes
  let actual = cnvMembers <$> conversationViewMaybeQualified convDomain alice conv

  assertEqual "members:" Nothing actual

--------------------------------------------------------------

mkOtherMember :: Qualified UserId -> OtherMember
mkOtherMember u = OtherMember u Nothing roleNameWireAdmin

mkRemoteMember :: Qualified UserId -> RemoteMember
mkRemoteMember u = I.RemoteMember (toRemote u) roleNameWireAdmin

mkInternalConv :: [LocalMember] -> [RemoteMember] -> IO Data.Conversation
mkInternalConv locals remotes = do
  -- for the conversationView unit tests, the creator plays no importance, so for simplicity this is set to a random value.
  creator <- randomId
  cnv <- randomId
  pure $
    Data.Conversation
      { Data.convId = cnv,
        Data.convType = RegularConv,
        Data.convCreator = creator,
        Data.convName = Just "unit testing gossip",
        Data.convAccess = [],
        Data.convAccessRole = ActivatedAccessRole,
        Data.convLocalMembers = locals,
        Data.convRemoteMembers = remotes,
        Data.convTeam = Nothing,
        Data.convDeleted = Just False,
        Data.convMessageTimer = Nothing,
        Data.convReceiptMode = Nothing
      }

mkMember :: Qualified UserId -> Member
mkMember (Qualified userId _domain) =
  Member
    { memId = userId,
      memService = Nothing,
      memOtrMutedStatus = Nothing,
      memOtrMutedRef = Nothing,
      memOtrArchived = False,
      memOtrArchivedRef = Nothing,
      memHidden = False,
      memHiddenRef = Nothing,
      memConvRoleName = roleNameWireAdmin
    }

mkInternalMember :: Qualified UserId -> LocalMember
mkInternalMember (Qualified userId _domain) =
  I.InternalMember
    { I.memId = userId,
      I.memService = Nothing,
      I.memOtrMutedStatus = Nothing,
      I.memOtrMutedRef = Nothing,
      I.memOtrArchived = False,
      I.memOtrArchivedRef = Nothing,
      I.memHidden = False,
      I.memHiddenRef = Nothing,
      I.memConvRoleName = roleNameWireAdmin
    }
