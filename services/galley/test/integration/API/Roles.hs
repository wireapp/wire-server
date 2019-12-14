module API.Roles where

import Imports
import API.Util
import Bilge hiding (timeout)
import Bilge.Assert
import Control.Lens (view)
import Data.Id
import Data.List1
import Galley.Types
import Galley.Types.Conversations.Roles
import Network.Wai.Utilities.Error
import Test.Tasty
import Test.Tasty.Cannon (TimeoutUnit (..), (#))
import TestHelpers
import TestSetup

import qualified Test.Tasty.Cannon        as WS

tests :: IO TestSetup -> TestTree
tests s = testGroup "Conversation roles"
        [ test s "conversation roles admin" handleConversationRoleAllAdmins
        , test s "conversation roles member" handleConversationRoleMember
        , test s "conversation roles member upgrade" handleConversationRoleMemberUpgrade
        ]

handleConversationRoleAllAdmins :: TestM ()
handleConversationRoleAllAdmins = do
    c     <- view tsCannon
    alice <- randomUser
    bob   <- randomUser
    chuck <- randomUser
    eve   <- randomUser
    jack  <- randomUser
    connectUsers alice (list1 bob [chuck, eve])
    connectUsers eve (singleton bob)
    connectUsers bob (singleton jack)

    let role = roleNameWireAdmin
    WS.bracketR3 c alice bob chuck $ \(wsA, wsB, wsC) -> do
        cid <- decodeConvId <$> postConvWithRole alice [bob, chuck] (Just "gossip") [] Nothing Nothing role

        -- Make sure everyone gets the correct event
        postMembersWithRole alice (singleton eve) cid role !!! const 200 === statusCode
        void . liftIO $ WS.assertMatchN (5 # Second) [wsA, wsB, wsC] $
            wsAssertMemberJoinWithRole cid alice [eve] role

        -- Admins can perform all operations on the conversation; creator is not relevant

        -- Add members
        postMembers bob (singleton jack) cid !!! assertActionSucceeded

        -- Remove members, regardless of who they are
        forM_ [chuck, alice] $ \mem -> deleteMember bob mem cid !!! assertActionSucceeded
        -- Modify the conversation name
        void $ putConversationName bob cid "gossip++" !!! assertActionSucceeded
        -- Modify other members roles
        forM_ [eve, jack] $ \mem -> do
            let update = OtherMemberUpdate (Just roleNameWireMember)
            putOtherMember bob mem update cid !!! assertActionSucceeded
        -- No updates for message timer, receipt mode or access
        putMessageTimerUpdate bob cid (ConversationMessageTimerUpdate Nothing) !!! assertActionSucceeded

        putReceiptMode bob cid (ReceiptMode 0) !!! assertActionSucceeded

        let nonActivatedAccess = ConversationAccessUpdate [CodeAccess] NonActivatedAccessRole
        putAccessUpdate bob cid nonActivatedAccess !!! assertActionSucceeded

        -- Finally, you and still do the following actions:

        -- Update your own member state
        let memUpdate = memberUpdate { mupOtrMute = Just True }
        putMember bob memUpdate cid !!! assertActionSucceeded
        -- Last option is to leave a conversation
        deleteMember bob bob cid !!! assertActionSucceeded

handleConversationRoleMember :: TestM ()
handleConversationRoleMember = do
    c     <- view tsCannon
    alice <- randomUser
    bob   <- randomUser
    chuck <- randomUser
    eve   <- randomUser
    jack  <- randomUser
    connectUsers alice (list1 bob [chuck, eve])
    connectUsers eve (list1 bob [jack])

    memberChecks c alice bob chuck eve jack roleNameWireMember

handleConversationRoleMemberUpgrade :: TestM ()
handleConversationRoleMemberUpgrade = do
    c     <- view tsCannon
    alice <- randomUser
    bob   <- randomUser
    chuck <- randomUser
    eve   <- randomUser
    jack  <- randomUser
    connectUsers alice (list1 bob [chuck, eve])
    connectUsers eve (list1 bob [jack])

    memberChecks c alice bob chuck eve jack roleNameWireMember

memberChecks :: WS.Cannon
             -> UserId
             -> UserId
             -> UserId
             -> UserId
             -> UserId
             -> RoleName
             -> TestM ()
memberChecks c alice bob chuck eve jack role = do
    WS.bracketR3 c alice bob chuck $ \(wsA, wsB, wsC) -> do
        cid <- decodeConvId <$> postConvWithRole alice [bob, chuck] (Just "gossip") [] Nothing Nothing role

        -- Make sure everyone gets the correct event
        postMembersWithRole alice (singleton eve) cid role !!! const 200 === statusCode
        void . liftIO $ WS.assertMatchN (5 # Second) [wsA, wsB, wsC] $
            wsAssertMemberJoinWithRole cid alice [eve] role

        -- Members cannot perform pretty much any action on the conversation

        -- Cannot add members, regardless of their role
        postMembers bob (singleton jack) cid !!! assertActionDenied

        -- Cannot remove members, regardless of who they are
        forM_ [chuck, alice] $ \mem -> deleteMember bob mem cid !!! assertActionDenied
        -- Cannot modify the conversation name
        void $ putConversationName bob cid "gossip++" !!! assertActionDenied
        -- Cannot modify other members roles
        forM_ [chuck, alice] $ \mem -> do
            let update = OtherMemberUpdate (Just roleNameWireMember)
            putOtherMember bob mem update cid !!! assertActionDenied
        -- Make sure you cannot elevate your own role
        let sneakyOtherMemberUpdate = OtherMemberUpdate (Just roleNameWireAdmin)
        putOtherMember bob bob sneakyOtherMemberUpdate cid !!! do
            const 403 === statusCode
            const (Just "invalid-op") === fmap label . responseJsonUnsafe

        let selfMemberUpdate = memberUpdate { mupConvRoleName = Just roleNameWireAdmin }
        putMember bob selfMemberUpdate cid !!! do
            const 403 === statusCode
            const (Just "invalid-actions") === fmap label . responseJsonUnsafe
        -- No updates for message timer, receipt mode or access
        putMessageTimerUpdate bob cid (ConversationMessageTimerUpdate Nothing) !!! assertActionDenied

        putReceiptMode bob cid (ReceiptMode 0) !!! assertActionDenied

        let nonActivatedAccess = ConversationAccessUpdate [CodeAccess] NonActivatedAccessRole
        putAccessUpdate bob cid nonActivatedAccess !!! assertActionDenied

        -- Finally, you and still do the following actions:

        -- Update your own member state
        let memUpdate = memberUpdate { mupOtrMute = Just True }
        putMember bob memUpdate cid !!! assertActionSucceeded
        -- Last option is to leave a conversation
        deleteMember bob bob cid !!! assertActionSucceeded

assertActionSucceeded :: HasCallStack => Assertions ()
assertActionSucceeded = const 200 === statusCode

assertActionDenied :: HasCallStack => Assertions ()
assertActionDenied = do
    const 403 === statusCode
    const (Just "action-denied") === fmap label . responseJsonUnsafe

-- Not sure we should make the tests this "generic", TBD
-- handleConversationRoles :: TestM ()
-- handleConversationRoles = do
--     c     <- view tsCannon
--     alice <- randomUser
--     bob   <- randomUser
--     chuck <- randomUser
--     eve   <- randomUser
--     connectUsers alice (list1 bob [chuck, eve])
--     connectUsers eve (singleton bob)

--     forM_ wireConvRoleNames $ \role -> WS.bracketR3 c alice bob chuck $ \(wsA, wsB, wsC) -> do
--         cid <- decodeConvId <$> postConvWithRole alice [bob, chuck] (Just "gossip") [] Nothing Nothing role

--         -- Make sure everyone gets the correct event
--         postMembersWithRole alice (singleton eve) cid role !!! const 200 === statusCode
--         void . liftIO $ WS.assertMatchN (5 # Second) [wsA, wsB, wsC] $
--             wsAssertMemberJoinWithRole cid alice [eve] role
