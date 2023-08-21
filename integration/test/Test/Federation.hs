{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Federation where

import API.Brig qualified as API
import API.Galley
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Codensity
import Control.Monad.Reader
import Data.ByteString.Lazy qualified as LBS
import Data.ProtoLens qualified as Proto
import Data.ProtoLens.Labels ()
import Data.UUID qualified as UUID
import Notifications
import Numeric.Lens
import Proto.Otr qualified as Proto
import Proto.Otr_Fields qualified as Proto
import SetupHelpers
import Testlib.Prelude
import Testlib.ResourcePool

testNotificationsForOfflineBackends :: HasCallStack => App ()
testNotificationsForOfflineBackends = do
  resourcePool <- asks (.resourcePool)
  -- `delUser` will eventually get deleted.
  [delUser, otherUser] <- createAndConnectUsers [OwnDomain, OtherDomain]
  delClient <- objId $ bindResponse (API.addClient delUser def) $ getJSON 201
  otherClient <- objId $ bindResponse (API.addClient otherUser def) $ getJSON 201

  -- We call it 'downBackend' because it is down for the most of this test
  -- except for setup and assertions. Perhaps there is a better name.
  runCodensity (acquireResources 1 resourcePool) $ \[downBackend] -> do
    (downUser1, downClient1, downUser2, upBackendConv, downBackendConv) <- runCodensity (startDynamicBackend downBackend mempty mempty) $ \_ -> do
      downUser1 <- randomUser downBackend.berDomain def
      downUser2 <- randomUser downBackend.berDomain def
      downClient1 <- objId $ bindResponse (API.addClient downUser1 def) $ getJSON 201
      connectUsers delUser downUser1
      connectUsers delUser downUser2
      connectUsers otherUser downUser1
      upBackendConv <- bindResponse (postConversation delUser (defProteus {qualifiedUsers = [otherUser, downUser1]})) $ getJSON 201
      downBackendConv <- bindResponse (postConversation downUser1 (defProteus {qualifiedUsers = [otherUser, delUser]})) $ getJSON 201
      pure (downUser1, downClient1, downUser2, upBackendConv, downBackendConv)

    -- Even when a participating backend is down, messages to conversations
    -- owned by other backends should go.
    successfulMsgForOtherUser <- mkProteusRecipient otherUser otherClient "success message for other user"
    successfulMsgForDownUser <- mkProteusRecipient downUser1 downClient1 "success message for down user"
    let successfulMsg =
          Proto.defMessage @Proto.QualifiedNewOtrMessage
            & #sender . Proto.client .~ (delClient ^?! hex)
            & #recipients .~ [successfulMsgForOtherUser, successfulMsgForDownUser]
            & #reportAll .~ Proto.defMessage
    bindResponse (postProteusMessage delUser upBackendConv successfulMsg) assertSuccess

    -- When conversation owning backend is down, messages will fail to be sent.
    failedMsgForOtherUser <- mkProteusRecipient otherUser otherClient "failed message for other user"
    failedMsgForDownUser <- mkProteusRecipient downUser1 downClient1 "failed message for down user"
    let failedMsg =
          Proto.defMessage @Proto.QualifiedNewOtrMessage
            & #sender . Proto.client .~ (delClient ^?! hex)
            & #recipients .~ [failedMsgForOtherUser, failedMsgForDownUser]
            & #reportAll .~ Proto.defMessage
    bindResponse (postProteusMessage delUser downBackendConv failedMsg) $ \resp ->
      -- Due to the way federation breaks in local env vs K8s, it can return 521
      -- (local) or 533 (K8s).
      resp.status `shouldMatchOneOf` [Number 521, Number 533]

    -- Conversation creation with people from down backend should fail
    bindResponse (postConversation delUser (defProteus {qualifiedUsers = [otherUser, downUser1]})) $ \resp ->
      resp.status `shouldMatchInt` 533

    -- Adding users to an up backend conversation should work even when one of
    -- the participating backends is down
    otherUser2 <- randomUser OtherDomain def
    connectUsers delUser otherUser2
    bindResponse (addMembers delUser upBackendConv [otherUser2]) $ \resp ->
      resp.status `shouldMatchInt` 200

    -- Adding users from down backend to a conversation should also fail
    bindResponse (addMembers delUser upBackendConv [downUser2]) $ \resp ->
      resp.status `shouldMatchInt` 533

    -- Removing users from an up backend conversation should work even when one
    -- of the participating backends is down.
    bindResponse (removeMember delUser upBackendConv otherUser2) $ \resp ->
      resp.status `shouldMatchInt` 200

    -- User deletions should eventually make it to the other backend.
    deleteUser delUser
    do
      newMsgNotif <- awaitNotification otherUser otherClient noValue 1 isNewMessageNotif
      newMsgNotif %. "payload.0.qualified_conversation" `shouldMatch` objQidObject upBackendConv
      newMsgNotif %. "payload.0.data.text" `shouldMatchBase64` "success message for other user"

      memberJoinNotif <- awaitNotification otherUser otherClient (Just newMsgNotif) 1 isMemberJoinNotif
      memberJoinNotif %. "payload.0.qualified_conversation" `shouldMatch` objQidObject upBackendConv
      asListOf objQidObject (memberJoinNotif %. "payload.0.data.users") `shouldMatch` mapM objQidObject [otherUser2]

      -- TODO: Broken
      -- delUserLeftDownConvNotif <- nPayload $ awaitNotification otherUser otherClient (Just newMsgNotif) 1 (allPreds [isConvLeaveNotif, isNotifConv downBackendConv])
      -- delUserLeftDownConvNotif %. "qualified_conversation" `shouldMatch` objQidObject downBackendConv
      -- delUserLeftDownConvNotif %. "data.qualified_user_ids.0" `shouldMatch` objQidObject delUser

      delUserDeletedNotif <- nPayload $ awaitNotification otherUser otherClient (Just newMsgNotif) 1 isDeleteUserNotif
      objQid delUserDeletedNotif `shouldMatch` objQid delUser

    runCodensity (startDynamicBackend downBackend mempty mempty) $ \_ -> do
      newMsgNotif <- awaitNotification downUser1 downClient1 noValue 5 isNewMessageNotif
      newMsgNotif %. "payload.0.qualified_conversation" `shouldMatch` objQidObject upBackendConv
      newMsgNotif %. "payload.0.data.text" `shouldMatchBase64` "success message for down user"

      -- FUTUREWORK: Uncomment after fixing this bug: https://wearezeta.atlassian.net/browse/WPB-3664
      -- memberJoinNotif <- awaitNotification downUser1 downClient1 (Just newMsgNotif) 1 isMemberJoinNotif
      -- memberJoinNotif %. "payload.0.qualified_conversation" `shouldMatch` objQidObject upBackendConv
      -- asListOf objQidObject (memberJoinNotif %. "payload.0.data.users") `shouldMatch` mapM objQidObject [downUser2]

      let isDelUserLeaveDownConvNotif =
            allPreds
              [ isConvLeaveNotif,
                isNotifConv downBackendConv,
                isNotifForUser delUser
              ]
      void $ awaitNotification downUser1 downClient1 (Just newMsgNotif) 1 isDelUserLeaveDownConvNotif

      -- FUTUREWORK: Uncomment after fixing this bug: https://wearezeta.atlassian.net/browse/WPB-3664
      -- void $ awaitNotification downUser1 downClient1 (Just newMsgNotif) 1 (allPreds [isConvLeaveNotif, isNotifConv upBackendConv, isNotifForUser otherUser])
      -- void $ awaitNotification downUser1 downClient1 (Just newMsgNotif) 1 (allPreds [isConvLeaveNotif, isNotifConv upBackendConv, isNotifForUser delUser])

      delUserDeletedNotif <- nPayload $ awaitNotification downUser1 downClient1 (Just newMsgNotif) 1 isDeleteUserNotif
      objQid delUserDeletedNotif `shouldMatch` objQid delUser

allPreds :: (Applicative f) => [a -> f Bool] -> a -> f Bool
allPreds [] _ = pure True
allPreds [p] x = p x
allPreds (p1 : ps) x = (&&) <$> p1 x <*> allPreds ps x

isDeleteUserNotif :: MakesValue a => a -> App Bool
isDeleteUserNotif n =
  nPayload n %. "type" `isEqual` "user.delete"

isNewMessageNotif :: MakesValue a => a -> App Bool
isNewMessageNotif n = fieldEquals n "payload.0.type" "conversation.otr-message-add"

isMemberJoinNotif :: MakesValue a => a -> App Bool
isMemberJoinNotif n = fieldEquals n "payload.0.type" "conversation.member-join"

isConvLeaveNotif :: MakesValue a => a -> App Bool
isConvLeaveNotif n = fieldEquals n "payload.0.type" "conversation.member-leave"

isNotifConv :: (MakesValue conv, MakesValue a) => conv -> a -> App Bool
isNotifConv conv n = fieldEquals n "payload.0.qualified_conversation" (objQidObject conv)

isNotifForUser :: (MakesValue user, MakesValue a) => user -> a -> App Bool
isNotifForUser user n = fieldEquals n "payload.0.data.qualified_user_ids.0" (objQidObject user)

fieldEquals :: (MakesValue a, MakesValue b) => a -> String -> b -> App Bool
fieldEquals a fieldSelector b = do
  ma <- lookupField a fieldSelector `catchAll` const (pure Nothing)
  case ma of
    Nothing -> pure False
    Just f ->
      f `isEqual` b

mkProteusRecipient :: (HasCallStack, MakesValue user, MakesValue client) => user -> client -> String -> App Proto.QualifiedUserEntry
mkProteusRecipient user client msg = do
  userDomain <- objDomain user
  userId <- LBS.toStrict . UUID.toByteString . fromJust . UUID.fromString <$> objId user
  clientId <- (^?! hex) <$> objId client
  pure $
    Proto.defMessage
      & #domain .~ fromString userDomain
      & #entries
        .~ [ Proto.defMessage
               & #user . #uuid .~ userId
               & #clients
                 .~ [ Proto.defMessage
                        & #client . #client .~ clientId
                        & #text .~ fromString msg
                    ]
           ]
