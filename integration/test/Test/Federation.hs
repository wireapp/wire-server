{-# LANGUAGE OverloadedLabels #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Federation where

import qualified API.Brig as BrigP
import API.Galley
import Control.Lens hiding ((&&~))
import Control.Monad.Codensity
import Control.Monad.Reader
import qualified Data.ProtoLens as Proto
import Data.ProtoLens.Labels ()
import MLS.Util
import Notifications
import Numeric.Lens
import qualified Proto.Otr as Proto
import qualified Proto.Otr_Fields as Proto
import SetupHelpers
import Testlib.Prelude
import Testlib.ResourcePool
import Testlib.VersionedFed

-- | Basic federation operations across every distinct pair of enabled legacy
-- and current backend versions.
testBasicFederationAcrossVersions :: (HasCallStack) => FedVersionPair -> App ()
testBasicFederationAcrossVersions pair = do
  withFederationDomains pair $ federationTestActionMLS cs
  where
    cs :: Ciphersuite
    cs =
      if anyVersionSatisfies (maybe False (\i -> i < 2))
        then Ciphersuite "0x0001"
        else def
      where
        anyVersionSatisfies :: (Maybe Integer -> Bool) -> Bool
        anyVersionSatisfies p = any p [versionToInt pair.localFedVersion, versionToInt pair.remoteFedVersion]

        versionToInt :: FedVersion -> Maybe Integer
        versionToInt (FedVersionLegacy i) = Just i
        versionToInt _ = Nothing

    -- Wire a pair of federation domains into the test action.
    withFederationDomains :: (HasCallStack) => FedVersionPair -> (Value -> Value -> App a) -> App a
    withFederationDomains pair' action =
      if FedVersionCurrentStable `elem` [pair'.localFedVersion, pair'.remoteFedVersion]
        then startDynamicBackends [currentVersionSettings ["development"]] $ \[stable] -> runWithStableDomain (Just stable)
        else runWithStableDomain Nothing
      where
        runWithStableDomain stableDomain = do
          localDomain <- resolveDomain pair'.localFedVersion stableDomain
          remoteDomain <- resolveDomain pair'.remoteFedVersion stableDomain
          action localDomain remoteDomain

        resolveDomain :: (HasCallStack) => FedVersion -> Maybe String -> App Value
        resolveDomain (FedVersionLegacy version) _ = make (AnyFedDomain version)
        resolveDomain FedVersionCurrentStable (Just stableDomain) = make stableDomain
        -- BackendA already enables the development federation API version.
        resolveDomain FedVersionCurrentDevelopment _ = make OwnDomain
        resolveDomain FedVersionCurrentStable Nothing = error "stable federation version has no dynamic backend"

    currentVersionSettings :: [String] -> ServiceOverrides
    currentVersionSettings disabledVersions =
      def
        { brigCfg = setField "optSettings.setDisabledFederationAPIVersions" disabledVersions,
          galleyCfg = setField "settings.disabledFederationAPIVersions" disabledVersions,
          cargoholdCfg = setField "settings.disabledFederationAPIVersions" disabledVersions
        }

-- | Test action independent of backend startup and domain resolution.
federationTestActionMLS :: Ciphersuite -> Value -> Value -> App ()
federationTestActionMLS cs localDomain remoteDomain = do
  (localAdmin, localTeam, [localMember]) <- createTeam localDomain 2
  (remoteAdmin, _remoteTeam, [remoteMember]) <- createTeam remoteDomain 2

  for_ [remoteAdmin, remoteMember] (connectTwoUsers localAdmin)

  clients@[localAdmin1, _, remoteAdmin1, _] <- traverse (createMLSClient def {ciphersuites = [cs]}) [localAdmin, localMember, remoteAdmin, remoteMember]
  for_ clients (uploadNewKeyPackage cs)

  convId <- createNewGroupWith cs localAdmin1 defMLS {team = Just localTeam}
  void
    $ createAddCommit localAdmin1 convId [localMember, remoteAdmin, remoteMember]
    >>= sendAndConsumeCommitBundle

  withWebSockets [localMember, remoteAdmin, remoteMember] $ \wss -> do
    localMessage <- createApplicationMessage convId localAdmin1 "message from the local team"
    postMLSMessage localMessage.sender localMessage.message >>= assertSuccess
    for_ wss (awaitMatch (isMessageForConversation convId))

  withWebSockets [localAdmin, localMember, remoteMember] $ \wss -> do
    remoteMessage <- createApplicationMessage convId remoteAdmin1 "message from the remote team"
    postMLSMessage remoteMessage.sender remoteMessage.message >>= assertSuccess
    for_ wss (awaitMatch (isMessageForConversation convId))
  where
    isMessageForConversation :: ConvId -> Value -> App Bool
    isMessageForConversation convId' notification =
      isNewMLSMessageNotif notification &&~ isNotifConvId convId' notification

testNotificationsForOfflineBackends :: (HasCallStack) => App ()
testNotificationsForOfflineBackends = do
  resourcePool <- asks (.resourcePool)
  -- `delUser` will eventually get deleted.
  [delUser, otherUser, otherUser2] <- createUsers [OwnDomain, OtherDomain, OtherDomain]
  delClient <- objId $ bindResponse (BrigP.addClient delUser def) $ getJSON 201
  otherClient <- objId $ bindResponse (BrigP.addClient otherUser def) $ getJSON 201
  otherClient2 <- objId $ bindResponse (BrigP.addClient otherUser2 def) $ getJSON 201

  -- We call it 'downBackend' because it is down for most of this test
  -- except for setup and assertions. Perhaps there is a better name.
  runCodensity (acquireResources 1 resourcePool) $ \[downBackend] -> do
    (downUser1, downClient1, downUser2, upBackendConv, downBackendConv) <- runCodensity (startDynamicBackend downBackend mempty) $ \_ -> do
      downUser1 <- randomUser downBackend.berDomain def
      downUser2 <- randomUser downBackend.berDomain def
      downClient1 <- objId $ bindResponse (BrigP.addClient downUser1 def) $ getJSON 201

      connectTwoUsers delUser otherUser
      connectTwoUsers delUser otherUser2
      connectTwoUsers delUser downUser1
      connectTwoUsers delUser downUser2
      connectTwoUsers downUser1 otherUser

      upBackendConv <- bindResponse (postConversation delUser (defProteus {qualifiedUsers = [otherUser, otherUser2, downUser1]})) $ getJSON 201
      downBackendConv <- bindResponse (postConversation downUser1 (defProteus {qualifiedUsers = [otherUser, delUser]})) $ getJSON 201
      pure (downUser1, downClient1, downUser2, upBackendConv, downBackendConv)

    withWebSocket otherUser $ \ws -> do
      -- Even when a participating backend is down, messages to conversations
      -- owned by other backends should go.
      successfulMsgForOtherUsers <- mkProteusRecipients otherUser [(otherUser, [otherClient]), (otherUser2, [otherClient2])] "success message for other user"
      successfulMsgForDownUser <- mkProteusRecipient downUser1 downClient1 "success message for down user"
      let successfulMsg =
            Proto.defMessage @Proto.QualifiedNewOtrMessage
              & #sender . Proto.client .~ (delClient ^?! hex)
              & #recipients .~ [successfulMsgForOtherUsers, successfulMsgForDownUser]
              & #reportAll .~ Proto.defMessage
      bindResponse (postProteusMessage delUser upBackendConv successfulMsg) assertSuccess

      -- When the conversation owning backend is down, messages will fail to be sent.
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

      -- Adding users to an up backend conversation should not work when one of
      -- the participating backends is down. This is due to not being able to
      -- check non-fully connected graph between all participating backends
      -- however, if the backend of the user to be added is already part of the conversation, we do not need to do the check
      -- and the user can be added as long as the backend is reachable
      otherUser3 <- randomUser OtherDomain def
      connectTwoUsers delUser otherUser3
      bindResponse (addMembers delUser upBackendConv def {users = [otherUser3]}) $ \resp ->
        resp.status `shouldMatchInt` 200

      -- Adding users from down backend to a conversation should fail
      bindResponse (addMembers delUser upBackendConv def {users = [downUser2]}) $ \resp ->
        resp.status `shouldMatchInt` 533

      -- Removing users from an up backend conversation should work even when one
      -- of the participating backends is down.
      bindResponse (removeMember delUser upBackendConv otherUser2) $ \resp ->
        resp.status `shouldMatchInt` 200

      -- Even removing a user from the down backend itself should work.
      bindResponse (removeMember delUser upBackendConv delUser) $ \resp ->
        resp.status `shouldMatchInt` 200

      -- User deletions should eventually make it to the other backend.
      deleteUser delUser

      let isOtherUser2LeaveUpConvNotif = allPreds [isConvLeaveNotif, isNotifConv upBackendConv, isNotifForUser otherUser2]
          isDelUserLeaveUpConvNotif = allPreds [isConvLeaveNotif, isNotifConv upBackendConv, isNotifForUser delUser]

      do
        newMsgNotif <- awaitMatch isNewMessageNotif ws
        newMsgNotif %. "payload.0.qualified_conversation" `shouldMatch` objQidObject upBackendConv
        newMsgNotif %. "payload.0.data.text" `shouldMatchBase64` fromString "success message for other user"

        void $ awaitMatch isOtherUser2LeaveUpConvNotif ws
        void $ awaitMatch isDelUserLeaveUpConvNotif ws

        delUserDeletedNotif <- nPayload $ awaitMatch isDeleteUserNotif ws
        objQid delUserDeletedNotif `shouldMatch` objQid delUser

    runCodensity (startDynamicBackend downBackend mempty) $ \_ -> do
      newMsgNotif <- awaitNotificationClient downUser1 downClient1 noValue isNewMessageNotif
      newMsgNotif %. "payload.0.qualified_conversation" `shouldMatch` objQidObject upBackendConv
      newMsgNotif %. "payload.0.data.text" `shouldMatchBase64` fromString "success message for down user"

      let isDelUserLeaveDownConvNotif =
            allPreds
              [ isConvLeaveNotif,
                isNotifConv downBackendConv,
                isNotifForUser delUser
              ]
      void $ awaitNotificationClient downUser1 (Just downClient1) (Just newMsgNotif) isDelUserLeaveDownConvNotif

      -- FUTUREWORK: Uncomment after fixing this bug: https://wearezeta.atlassian.net/browse/WPB-3664
      -- void $ awaitNotification downUser1 downClient1 (Just newMsgNotif) 1 isOtherUser2LeaveUpConvNotif
      -- void $ awaitNotification otherUser otherClient (Just newMsgNotif) isDelUserLeaveDownConvNotif

      delUserDeletedNotif <- nPayload $ awaitNotificationClient downUser1 downClient1 (Just newMsgNotif) isDeleteUserNotif
      objQid delUserDeletedNotif `shouldMatch` objQid delUser
