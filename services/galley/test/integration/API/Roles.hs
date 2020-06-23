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

module API.Roles where

import API.Util
import Bilge hiding (timeout)
import Bilge.Assert
import Control.Lens (view)
import Data.Id
import Data.List1
import Galley.Types
import Galley.Types.Conversations.Roles
import Imports
import Network.Wai.Utilities.Error
import Test.Tasty
import Test.Tasty.Cannon ((#), TimeoutUnit (..))
import qualified Test.Tasty.Cannon as WS
import TestHelpers
import TestSetup

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "Conversation roles"
    [ test s "conversation roles admin (and downgrade)" handleConversationRoleAdmin,
      test s "conversation roles member (and upgrade)" handleConversationRoleMember
    ]

handleConversationRoleAdmin :: TestM ()
handleConversationRoleAdmin = do
  c <- view tsCannon
  alice <- randomUser
  bob <- randomUser
  chuck <- randomUser
  eve <- randomUser
  jack <- randomUser
  connectUsers alice (list1 bob [chuck, eve, jack])
  connectUsers eve (singleton bob)
  connectUsers bob (singleton jack)
  let role = roleNameWireAdmin
  cid <- WS.bracketR3 c alice bob chuck $ \(wsA, wsB, wsC) -> do
    rsp <- postConvWithRole alice [bob, chuck] (Just "gossip") [] Nothing Nothing role
    void $ assertConvWithRole rsp RegularConv alice alice [bob, chuck] (Just "gossip") Nothing role
    let cid = decodeConvId rsp
    -- Make sure everyone gets the correct event
    postMembersWithRole alice (singleton eve) cid role !!! const 200 === statusCode
    void . liftIO $ WS.assertMatchN (5 # Second) [wsA, wsB, wsC] $
      wsAssertMemberJoinWithRole cid alice [eve] role
    -- Add a member to help out with testing
    postMembersWithRole alice (singleton jack) cid roleNameWireMember !!! const 200 === statusCode
    void . liftIO $ WS.assertMatchN (5 # Second) [wsA, wsB, wsC] $
      wsAssertMemberJoinWithRole cid alice [jack] roleNameWireMember
    return cid
  -- Added bob as a wire_admin and do the checks
  wireAdminChecks cid alice bob jack
  -- Demote bob and run the member checks
  WS.bracketR3 c alice bob chuck $ \(wsA, wsB, wsC) -> do
    let updateDown = OtherMemberUpdate (Just roleNameWireMember)
    putOtherMember alice bob updateDown cid !!! assertActionSucceeded
    void . liftIO . WS.assertMatchN (5 # Second) [wsA, wsB, wsC] $ do
      wsAssertMemberUpdateWithRole cid alice bob roleNameWireMember
  wireMemberChecks cid bob alice jack

handleConversationRoleMember :: TestM ()
handleConversationRoleMember = do
  c <- view tsCannon
  alice <- randomUser
  bob <- randomUser
  chuck <- randomUser
  eve <- randomUser
  jack <- randomUser
  connectUsers alice (list1 bob [chuck, eve])
  connectUsers bob (singleton chuck)
  connectUsers eve (list1 bob [jack])
  let role = roleNameWireMember
  cid <- WS.bracketR3 c alice bob chuck $ \(wsA, wsB, wsC) -> do
    rsp <- postConvWithRole alice [bob, chuck] (Just "gossip") [] Nothing Nothing role
    void $ assertConvWithRole rsp RegularConv alice alice [bob, chuck] (Just "gossip") Nothing role
    let cid = decodeConvId rsp
    -- Make sure everyone gets the correct event
    postMembersWithRole alice (singleton eve) cid role !!! const 200 === statusCode
    void . liftIO $ WS.assertMatchN (5 # Second) [wsA, wsB, wsC] $
      wsAssertMemberJoinWithRole cid alice [eve] role
    return cid
  -- Added bob as a wire_member and do the checks
  wireMemberChecks cid bob alice chuck
  -- Let's promote bob
  WS.bracketR3 c alice bob chuck $ \(wsA, wsB, wsC) -> do
    let updateUp = OtherMemberUpdate (Just roleNameWireAdmin)
    -- Chuck cannot update, member only
    putOtherMember chuck bob updateUp cid !!! assertActionDenied
    putOtherMember alice bob updateUp cid !!! assertActionSucceeded
    void . liftIO . WS.assertMatchN (5 # Second) [wsA, wsB, wsC] $ do
      wsAssertMemberUpdateWithRole cid alice bob roleNameWireAdmin
  wireAdminChecks cid bob alice chuck

-- | Given an admin, another admin and a member run all
--   the necessary checks targeting the admin
wireAdminChecks ::
  ConvId ->
  UserId ->
  UserId ->
  UserId ->
  TestM ()
wireAdminChecks cid admin otherAdmin mem = do
  let role = roleNameWireAdmin
  other <- randomUser
  connectUsers admin (singleton other)
  -- Admins can perform all operations on the conversation; creator is not relevant

  -- Add members
  postMembers admin (singleton other) cid !!! assertActionSucceeded
  -- Remove members, regardless of who they are
  forM_ [otherAdmin, mem] $ \victim -> do
    deleteMember admin victim cid !!! assertActionSucceeded
    postMembersWithRole admin (singleton victim) cid role !!! assertActionSucceeded
  -- Modify the conversation name
  void $ putConversationName admin cid "gossip++" !!! assertActionSucceeded
  -- Modify other members roles
  forM_ [otherAdmin, mem] $ \victim -> do
    let updateDown = OtherMemberUpdate (Just roleNameWireMember)
    putOtherMember admin victim updateDown cid !!! assertActionSucceeded
    let updateUp = OtherMemberUpdate (Just roleNameWireAdmin)
    putOtherMember admin victim updateUp cid !!! assertActionSucceeded
  -- Updates for message timer, receipt mode or access
  putMessageTimerUpdate admin cid (ConversationMessageTimerUpdate $ Just 1000) !!! assertActionSucceeded
  putMessageTimerUpdate admin cid (ConversationMessageTimerUpdate $ Just 2000) !!! assertActionSucceeded
  putReceiptMode admin cid (ReceiptMode 0) !!! assertActionSucceeded
  putReceiptMode admin cid (ReceiptMode 1) !!! assertActionSucceeded
  let nonActivatedAccess = ConversationAccessUpdate [CodeAccess] NonActivatedAccessRole
  putAccessUpdate admin cid nonActivatedAccess !!! assertActionSucceeded
  let activatedAccess = ConversationAccessUpdate [InviteAccess] NonActivatedAccessRole
  putAccessUpdate admin cid activatedAccess !!! assertActionSucceeded
  -- Update your own member state
  let memUpdate = memberUpdate {mupOtrMute = Just True}
  putMember admin memUpdate cid !!! assertActionSucceeded
  -- You can also leave a conversation
  deleteMember admin admin cid !!! assertActionSucceeded
  -- Readding the user
  postMembersWithRole otherAdmin (singleton admin) cid role !!! const 200 === statusCode

-- | Given a member, admin and otherMem, run all the necessary checks
--   targeting mem
wireMemberChecks ::
  ConvId ->
  UserId ->
  UserId ->
  UserId ->
  TestM ()
wireMemberChecks cid mem admin otherMem = do
  let role = roleNameWireMember
  other <- randomUser
  connectUsers mem (singleton other)
  -- Members cannot perform pretty much any action on the conversation

  -- Cannot add members, regardless of their role
  postMembers mem (singleton other) cid !!! assertActionDenied
  -- Cannot remove members, regardless of who they are
  forM_ [admin, otherMem] $ \victim -> deleteMember mem victim cid !!! assertActionDenied
  -- Cannot modify the conversation name
  void $ putConversationName mem cid "gossip++" !!! assertActionDenied
  -- Cannot modify other members roles
  forM_ [admin, otherMem] $ \victim -> do
    let update = OtherMemberUpdate (Just roleNameWireMember)
    putOtherMember mem victim update cid !!! assertActionDenied
  -- Make sure you cannot elevate your own role
  let sneakyOtherMemberUpdate = OtherMemberUpdate (Just roleNameWireAdmin)
  putOtherMember mem mem sneakyOtherMemberUpdate cid !!! do
    const 403 === statusCode
    const (Just "invalid-op") === fmap label . responseJsonUnsafe
  let selfMemberUpdate = memberUpdate {mupConvRoleName = Just roleNameWireAdmin}
  putMember mem selfMemberUpdate cid !!! do
    const 403 === statusCode
    const (Just "invalid-actions") === fmap label . responseJsonUnsafe
  -- No updates for message timer, receipt mode or access
  putMessageTimerUpdate mem cid (ConversationMessageTimerUpdate Nothing) !!! assertActionDenied
  putReceiptMode mem cid (ReceiptMode 0) !!! assertActionDenied
  let nonActivatedAccess = ConversationAccessUpdate [CodeAccess] NonActivatedAccessRole
  putAccessUpdate mem cid nonActivatedAccess !!! assertActionDenied
  -- Finally, you can still do the following actions:

  -- Update your own member state
  let memUpdate = memberUpdate {mupOtrMute = Just True}
  putMember mem memUpdate cid !!! assertActionSucceeded
  -- Last option is to leave a conversation
  deleteMember mem mem cid !!! assertActionSucceeded
  -- Let's readd the user to make tests easier
  postMembersWithRole admin (singleton mem) cid role !!! const 200 === statusCode

assertActionSucceeded :: HasCallStack => Assertions ()
assertActionSucceeded = const 200 === statusCode

assertActionDenied :: HasCallStack => Assertions ()
assertActionDenied = do
  const 403 === statusCode
  const (Just "action-denied") === fmap label . responseJsonUnsafe
