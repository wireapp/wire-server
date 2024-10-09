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

module Galley.API.LegalHold
  ( createSettings,
    getSettings,
    removeSettingsInternalPaging,
    removeSettings,
    removeSettings',
    getUserStatus,
    grantConsent,
    requestDevice,
    approveDevice,
    disableForUser,
    unsetTeamLegalholdWhitelistedH,
  )
where

import Brig.Types.Connection (UpdateConnectionsInternal (..))
import Brig.Types.Team.LegalHold (legalHoldService, viewLegalHoldService)
import Control.Exception (assert)
import Control.Lens (view, (^.))
import Data.ByteString.Conversion (toByteString)
import Data.Id
import Data.LegalHold (UserLegalHoldStatus (..), defUserLegalHoldStatus)
import Data.List.Split (chunksOf)
import Data.Misc
import Data.Proxy (Proxy (Proxy))
import Data.Qualified
import Data.Range (toRange)
import Data.Time.Clock
import Galley.API.Error
import Galley.API.LegalHold.Get
import Galley.API.LegalHold.Team
import Galley.API.Query (iterateConversations)
import Galley.API.Update (removeMemberFromLocalConv)
import Galley.API.Util
import Galley.App
import Galley.Data.Conversation qualified as Data
import Galley.Effects
import Galley.Effects.BrigAccess
import Galley.Effects.FireAndForget
import Galley.Effects.LegalHoldStore qualified as LegalHoldData
import Galley.Effects.TeamMemberStore
import Galley.Effects.TeamStore
import Galley.External.LegalHoldService qualified as LHService
import Galley.Types.Conversations.Members
import Galley.Types.Teams as Team
import Imports
import Network.HTTP.Types.Status (status200)
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog qualified as P
import System.Logger.Class qualified as Log
import Wire.API.Conversation (ConvType (..))
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Federation.Error
import Wire.API.Provider.Service
import Wire.API.Routes.Internal.Brig.Connection
import Wire.API.Routes.Public.Galley.LegalHold
import Wire.API.Team.LegalHold
import Wire.API.Team.LegalHold qualified as Public
import Wire.API.Team.LegalHold.External hiding (userId)
import Wire.API.Team.Member
import Wire.API.User.Client.Prekey
import Wire.NotificationSubsystem
import Wire.Sem.Paging
import Wire.Sem.Paging.Cassandra

createSettings ::
  forall r.
  ( Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS OperationDenied) r,
    Member (ErrorS 'LegalHoldNotEnabled) r,
    Member (ErrorS 'LegalHoldServiceInvalidKey) r,
    Member (ErrorS 'LegalHoldServiceBadResponse) r,
    Member LegalHoldStore r,
    Member TeamFeatureStore r,
    Member TeamStore r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  TeamId ->
  Public.NewLegalHoldService ->
  Sem r Public.ViewLegalHoldService
createSettings lzusr tid newService = do
  let zusr = tUnqualified lzusr
  assertLegalHoldEnabledForTeam tid
  zusrMembership <- getTeamMember tid zusr
  -- let zothers = map (view userId) membs
  -- Log.debug $
  --   Log.field "targets" (toByteString . show $ toByteString <$> zothers)
  --     . Log.field "action" (Log.val "LegalHold.createSettings")
  void $ permissionCheck ChangeLegalHoldTeamSettings zusrMembership
  (key :: ServiceKey, fpr :: Fingerprint Rsa) <-
    LegalHoldData.validateServiceKey newService.newLegalHoldServiceKey
      >>= noteS @'LegalHoldServiceInvalidKey
  LHService.checkLegalHoldServiceStatus fpr newService.newLegalHoldServiceUrl
  let service = legalHoldService tid fpr newService key
  LegalHoldData.createSettings service
  pure . viewLegalHoldService $ service

getSettings ::
  forall r.
  ( Member (ErrorS 'NotATeamMember) r,
    Member LegalHoldStore r,
    Member TeamFeatureStore r,
    Member TeamStore r
  ) =>
  Local UserId ->
  TeamId ->
  Sem r Public.ViewLegalHoldService
getSettings lzusr tid = do
  let zusr = tUnqualified lzusr
  zusrMembership <- getTeamMember tid zusr
  void $ maybe (throwS @'NotATeamMember) pure zusrMembership
  isenabled <- isLegalHoldEnabledForTeam tid
  mresult <- LegalHoldData.getSettings tid
  pure $ case (isenabled, mresult) of
    (False, _) -> Public.ViewLegalHoldServiceDisabled
    (True, Nothing) -> Public.ViewLegalHoldServiceNotConfigured
    (True, Just result) -> viewLegalHoldService result

removeSettingsInternalPaging ::
  forall r.
  ( Member BackendNotificationQueueAccess r,
    Member BrigAccess r,
    Member ConversationStore r,
    Member (Error AuthenticationError) r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member (ErrorS ('ActionDenied 'RemoveConversationMember)) r,
    Member (ErrorS 'LegalHoldCouldNotBlockConnections) r,
    Member (ErrorS 'LegalHoldDisableUnimplemented) r,
    Member (ErrorS 'LegalHoldNotEnabled) r,
    Member (ErrorS 'LegalHoldServiceNotRegistered) r,
    Member (ErrorS 'LegalHoldServiceBadResponse) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS OperationDenied) r,
    Member (ErrorS 'UserLegalHoldIllegalOperation) r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member FireAndForget r,
    Member NotificationSubsystem r,
    Member (Input Env) r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member LegalHoldStore r,
    Member (ListItems LegacyPaging ConvId) r,
    Member MemberStore r,
    Member ProposalStore r,
    Member P.TinyLog r,
    Member Random r,
    Member SubConversationStore r,
    Member TeamFeatureStore r,
    Member (TeamMemberStore InternalPaging) r,
    Member TeamStore r
  ) =>
  Local UserId ->
  TeamId ->
  Public.RemoveLegalHoldSettingsRequest ->
  Sem r ()
removeSettingsInternalPaging lzusr = removeSettings @InternalPaging (tUnqualified lzusr)

removeSettings ::
  forall p r.
  ( Paging p,
    Bounded (PagingBounds p TeamMember),
    Member TeamFeatureStore r,
    Member (TeamMemberStore p) r,
    Member TeamStore r,
    Member BackendNotificationQueueAccess r,
    Member BrigAccess r,
    Member ConversationStore r,
    Member (Error AuthenticationError) r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member (ErrorS ('ActionDenied 'RemoveConversationMember)) r,
    Member (ErrorS 'LegalHoldCouldNotBlockConnections) r,
    Member (ErrorS 'LegalHoldDisableUnimplemented) r,
    Member (ErrorS 'LegalHoldNotEnabled) r,
    Member (ErrorS 'LegalHoldServiceNotRegistered) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS OperationDenied) r,
    Member (ErrorS 'UserLegalHoldIllegalOperation) r,
    Member (ErrorS 'LegalHoldServiceBadResponse) r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member FireAndForget r,
    Member NotificationSubsystem r,
    Member (Input Env) r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member LegalHoldStore r,
    Member (ListItems LegacyPaging ConvId) r,
    Member MemberStore r,
    Member ProposalStore r,
    Member P.TinyLog r,
    Member Random r,
    Member SubConversationStore r
  ) =>
  UserId ->
  TeamId ->
  Public.RemoveLegalHoldSettingsRequest ->
  Sem r ()
removeSettings zusr tid (Public.RemoveLegalHoldSettingsRequest mPassword) = do
  assertNotWhitelisting
  assertLegalHoldEnabledForTeam tid
  zusrMembership <- getTeamMember tid zusr
  -- let zothers = map (view userId) membs
  -- Log.debug $
  --   Log.field "targets" (toByteString . show $ toByteString <$> zothers)
  --     . Log.field "action" (Log.val "LegalHold.removeSettings")
  void $ permissionCheck ChangeLegalHoldTeamSettings zusrMembership
  ensureReAuthorised zusr mPassword Nothing Nothing
  removeSettings' @p tid
  where
    assertNotWhitelisting :: Sem r ()
    assertNotWhitelisting = do
      getLegalHoldFlag >>= \case
        FeatureLegalHoldDisabledPermanently -> pure ()
        FeatureLegalHoldDisabledByDefault -> pure ()
        FeatureLegalHoldWhitelistTeamsAndImplicitConsent ->
          throwS @'LegalHoldDisableUnimplemented

-- | Remove legal hold settings from team; also disabling for all users and removing LH devices
removeSettings' ::
  forall p r.
  ( Paging p,
    Bounded (PagingBounds p TeamMember),
    Member BackendNotificationQueueAccess r,
    Member BrigAccess r,
    Member ConversationStore r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member (ErrorS ('ActionDenied 'RemoveConversationMember)) r,
    Member (ErrorS 'LegalHoldServiceNotRegistered) r,
    Member (ErrorS 'UserLegalHoldIllegalOperation) r,
    Member (ErrorS 'LegalHoldCouldNotBlockConnections) r,
    Member (ErrorS 'LegalHoldServiceBadResponse) r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member FireAndForget r,
    Member NotificationSubsystem r,
    Member (Input UTCTime) r,
    Member (Input (Local ())) r,
    Member (Input Env) r,
    Member LegalHoldStore r,
    Member (ListItems LegacyPaging ConvId) r,
    Member MemberStore r,
    Member (TeamMemberStore p) r,
    Member TeamStore r,
    Member ProposalStore r,
    Member Random r,
    Member P.TinyLog r,
    Member SubConversationStore r
  ) =>
  TeamId ->
  Sem r ()
removeSettings' tid =
  withChunks
    (\mps -> listTeamMembers @p tid mps maxBound)
    action
  where
    action :: [TeamMember] -> Sem r ()
    action membs = do
      let zothers = map (view userId) membs
      let lhMembers = filter ((== UserLegalHoldEnabled) . view legalHoldStatus) membs
      P.debug $
        Log.field "targets" (toByteString . show $ toByteString <$> zothers)
          . Log.field "action" (Log.val "LegalHold.removeSettings'")
      spawnMany (map removeLHForUser lhMembers)
    removeLHForUser :: TeamMember -> Sem r ()
    removeLHForUser member = do
      luid <- qualifyLocal (member ^. userId)
      removeLegalHoldClientFromUser (tUnqualified luid)
      LHService.removeLegalHold tid luid
      changeLegalholdStatusAndHandlePolicyConflicts tid luid (member ^. legalHoldStatus) UserLegalHoldDisabled -- (support for withdrawing consent is not planned yet.)

-- | Change 'UserLegalHoldStatus' from no consent to disabled.  FUTUREWORK:
-- @withdrawExplicitConsentH@ (lots of corner cases we'd have to implement for that to pan
-- out).
grantConsent ::
  ( Member BackendNotificationQueueAccess r,
    Member BrigAccess r,
    Member ConversationStore r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member (ErrorS ('ActionDenied 'RemoveConversationMember)) r,
    Member (ErrorS 'LegalHoldCouldNotBlockConnections) r,
    Member (ErrorS 'TeamMemberNotFound) r,
    Member (ErrorS 'UserLegalHoldIllegalOperation) r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member NotificationSubsystem r,
    Member (Input Env) r,
    Member (Input UTCTime) r,
    Member LegalHoldStore r,
    Member (ListItems LegacyPaging ConvId) r,
    Member MemberStore r,
    Member ProposalStore r,
    Member P.TinyLog r,
    Member Random r,
    Member SubConversationStore r,
    Member TeamStore r
  ) =>
  Local UserId ->
  TeamId ->
  Sem r GrantConsentResult
grantConsent lusr tid = do
  userLHStatus <-
    noteS @'TeamMemberNotFound
      =<< fmap (view legalHoldStatus) <$> getTeamMember tid (tUnqualified lusr)
  case userLHStatus of
    lhs@UserLegalHoldNoConsent ->
      changeLegalholdStatusAndHandlePolicyConflicts tid lusr lhs UserLegalHoldDisabled $> GrantConsentSuccess
    UserLegalHoldEnabled -> pure GrantConsentAlreadyGranted
    UserLegalHoldPending -> pure GrantConsentAlreadyGranted
    UserLegalHoldDisabled -> pure GrantConsentAlreadyGranted

-- | Request to provision a device on the legal hold service for a user
requestDevice ::
  forall r.
  ( Member BackendNotificationQueueAccess r,
    Member BrigAccess r,
    Member ConversationStore r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member (ErrorS ('ActionDenied 'RemoveConversationMember)) r,
    Member (ErrorS 'LegalHoldCouldNotBlockConnections) r,
    Member (ErrorS 'LegalHoldNotEnabled) r,
    Member (ErrorS 'LegalHoldServiceBadResponse) r,
    Member (ErrorS 'LegalHoldServiceNotRegistered) r,
    Member (ErrorS 'MLSLegalholdIncompatible) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS 'NoUserLegalHoldConsent) r,
    Member (ErrorS OperationDenied) r,
    Member (ErrorS 'TeamMemberNotFound) r,
    Member (ErrorS 'UserLegalHoldAlreadyEnabled) r,
    Member (ErrorS 'UserLegalHoldIllegalOperation) r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member NotificationSubsystem r,
    Member (Input (Local ())) r,
    Member (Input Env) r,
    Member (Input UTCTime) r,
    Member LegalHoldStore r,
    Member (ListItems LegacyPaging ConvId) r,
    Member MemberStore r,
    Member ProposalStore r,
    Member P.TinyLog r,
    Member Random r,
    Member SubConversationStore r,
    Member TeamFeatureStore r,
    Member TeamStore r
  ) =>
  Local UserId ->
  TeamId ->
  UserId ->
  Sem r RequestDeviceResult
requestDevice lzusr tid uid = do
  let zusr = tUnqualified lzusr
  luid <- qualifyLocal uid
  assertLegalHoldEnabledForTeam tid
  P.debug $
    Log.field "targets" (toByteString (tUnqualified luid))
      . Log.field "action" (Log.val "LegalHold.requestDevice")
  zusrMembership <- getTeamMember tid zusr
  void $ permissionCheck ChangeLegalHoldUserSettings zusrMembership
  member <- noteS @'TeamMemberNotFound =<< getTeamMember tid uid
  case member ^. legalHoldStatus of
    UserLegalHoldEnabled -> throwS @'UserLegalHoldAlreadyEnabled
    lhs@UserLegalHoldPending ->
      -- FUTUREWORK: we create a new device if a pending one is found.  this helps with
      -- recovering from lost credentials (but where would that happen?).  on the other
      -- hand. do we properly gc the old pending device?  maybe we should just throw an error
      -- here?
      RequestDeviceAlreadyPending <$ provisionLHDevice zusr luid lhs
    lhs@UserLegalHoldDisabled -> RequestDeviceSuccess <$ provisionLHDevice zusr luid lhs
    UserLegalHoldNoConsent -> throwS @'NoUserLegalHoldConsent
  where
    disallowIfMLSUser :: Local UserId -> Sem r ()
    disallowIfMLSUser luid = do
      void $ iterateConversations luid (toRange (Proxy @500)) $ \convs -> do
        when (any (\c -> c.convProtocol /= ProtocolProteus) convs) $ do
          throwS @'MLSLegalholdIncompatible

    -- Wire's LH service that galley is usually calling here is idempotent in device creation,
    -- ie. it returns the existing device on multiple calls to `/init`, like here:
    -- https://github.com/wireapp/legalhold/blob/e0a241162b9dbc841f12fbc57c8a1e1093c7e83a/src/main/java/com/wire/bots/hold/resource/InitiateResource.java#L42
    --
    -- This will still work if the LH service creates two new device on two consecutive calls
    -- to `/init`, but there may be race conditions, eg. when updating and enabling a pending
    -- device at (almost) the same time.
    provisionLHDevice :: UserId -> Local UserId -> UserLegalHoldStatus -> Sem r ()
    provisionLHDevice zusr luid userLHStatus = do
      disallowIfMLSUser luid
      (lastPrekey', prekeys) <- requestDeviceFromService luid
      -- We don't distinguish the last key here; brig will do so when the device is added
      LegalHoldData.insertPendingPrekeys (tUnqualified luid) (unpackLastPrekey lastPrekey' : prekeys)
      changeLegalholdStatusAndHandlePolicyConflicts tid luid userLHStatus UserLegalHoldPending
      notifyClientsAboutLegalHoldRequest zusr (tUnqualified luid) lastPrekey'

    requestDeviceFromService :: Local UserId -> Sem r (LastPrekey, [Prekey])
    requestDeviceFromService luid = do
      LegalHoldData.dropPendingPrekeys (tUnqualified luid)
      lhDevice <- LHService.requestNewDevice tid luid
      let NewLegalHoldClient prekeys lastKey = lhDevice
      pure (lastKey, prekeys)

-- | Approve the adding of a Legal Hold device to the user.
--
-- We don't delete pending prekeys during this flow just in case
-- it gets interupted. There's really no reason to delete them anyways
-- since they are replaced if needed when registering new LH devices.
approveDevice ::
  forall r.
  ( Member BackendNotificationQueueAccess r,
    Member BrigAccess r,
    Member ConversationStore r,
    Member (Error AuthenticationError) r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member (ErrorS 'AccessDenied) r,
    Member (ErrorS ('ActionDenied 'RemoveConversationMember)) r,
    Member (ErrorS 'LegalHoldCouldNotBlockConnections) r,
    Member (ErrorS 'LegalHoldNotEnabled) r,
    Member (ErrorS 'LegalHoldServiceNotRegistered) r,
    Member (ErrorS 'NoLegalHoldDeviceAllocated) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS 'UserLegalHoldAlreadyEnabled) r,
    Member (ErrorS 'UserLegalHoldIllegalOperation) r,
    Member (ErrorS 'UserLegalHoldNotPending) r,
    Member (ErrorS 'LegalHoldServiceBadResponse) r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member NotificationSubsystem r,
    Member (Input (Local ())) r,
    Member (Input Env) r,
    Member (Input UTCTime) r,
    Member LegalHoldStore r,
    Member (ListItems LegacyPaging ConvId) r,
    Member MemberStore r,
    Member ProposalStore r,
    Member P.TinyLog r,
    Member Random r,
    Member SubConversationStore r,
    Member TeamFeatureStore r,
    Member TeamStore r
  ) =>
  Local UserId ->
  ConnId ->
  TeamId ->
  UserId ->
  Public.ApproveLegalHoldForUserRequest ->
  Sem r ()
approveDevice lzusr connId tid uid (Public.ApproveLegalHoldForUserRequest mPassword) = do
  let zusr = tUnqualified lzusr
  luid <- qualifyLocal uid
  assertLegalHoldEnabledForTeam tid
  P.debug $
    Log.field "targets" (toByteString (tUnqualified luid))
      . Log.field "action" (Log.val "LegalHold.approveDevice")
  unless (zusr == tUnqualified luid) $ throwS @'AccessDenied
  assertOnTeam (tUnqualified luid) tid
  ensureReAuthorised zusr mPassword Nothing Nothing
  userLHStatus <-
    maybe defUserLegalHoldStatus (view legalHoldStatus) <$> getTeamMember tid (tUnqualified luid)
  assertUserLHPending userLHStatus
  mPreKeys <- LegalHoldData.selectPendingPrekeys (tUnqualified luid)
  (prekeys, lastPrekey') <- case mPreKeys of
    Nothing -> do
      P.info $ Log.msg @Text "No prekeys found"
      throwS @'NoLegalHoldDeviceAllocated
    Just keys -> pure keys
  clientId <- addLegalHoldClientToUser (tUnqualified luid) connId prekeys lastPrekey'
  -- Note: teamId could be passed in the getLegalHoldAuthToken request instead of lookup up again
  -- Note: both 'getLegalHoldToken' and 'ensureReAuthorized' check the password
  -- Note: both 'getLegalHoldToken' and this function in 'assertOnTeam' above
  --       checks that the user is part of a binding team
  -- FUTUREWORK: reduce double checks
  legalHoldAuthToken <- getLegalHoldAuthToken (tUnqualified luid) mPassword
  LHService.confirmLegalHold clientId tid luid legalHoldAuthToken
  -- TODO: send event at this point (see also:
  -- https://github.com/wireapp/wire-server/pull/802#pullrequestreview-262280386)
  changeLegalholdStatusAndHandlePolicyConflicts tid luid userLHStatus UserLegalHoldEnabled
  where
    assertUserLHPending ::
      UserLegalHoldStatus ->
      Sem r ()
    assertUserLHPending userLHStatus = do
      case userLHStatus of
        UserLegalHoldEnabled -> throwS @'UserLegalHoldAlreadyEnabled
        UserLegalHoldPending -> pure ()
        UserLegalHoldDisabled -> throwS @'UserLegalHoldNotPending
        UserLegalHoldNoConsent -> throwS @'UserLegalHoldNotPending

disableForUser ::
  forall r.
  ( Member BackendNotificationQueueAccess r,
    Member BrigAccess r,
    Member ConversationStore r,
    Member (Error AuthenticationError) r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member (ErrorS ('ActionDenied 'RemoveConversationMember)) r,
    Member (ErrorS 'LegalHoldCouldNotBlockConnections) r,
    Member (ErrorS 'LegalHoldServiceNotRegistered) r,
    Member (ErrorS 'LegalHoldServiceBadResponse) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS OperationDenied) r,
    Member (ErrorS 'UserLegalHoldIllegalOperation) r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member NotificationSubsystem r,
    Member (Input Env) r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member LegalHoldStore r,
    Member (ListItems LegacyPaging ConvId) r,
    Member MemberStore r,
    Member ProposalStore r,
    Member P.TinyLog r,
    Member Random r,
    Member SubConversationStore r,
    Member TeamStore r
  ) =>
  Local UserId ->
  TeamId ->
  UserId ->
  Public.DisableLegalHoldForUserRequest ->
  Sem r DisableLegalHoldForUserResponse
disableForUser lzusr tid uid (Public.DisableLegalHoldForUserRequest mPassword) = do
  luid <- qualifyLocal uid
  P.debug $
    Log.field "targets" (toByteString (tUnqualified luid))
      . Log.field "action" (Log.val "LegalHold.disableForUser")
  zusrMembership <- getTeamMember tid (tUnqualified lzusr)
  void $ permissionCheck ChangeLegalHoldUserSettings zusrMembership

  userLHStatus <-
    maybe defUserLegalHoldStatus (view legalHoldStatus) <$> getTeamMember tid (tUnqualified luid)

  let doDisable = disableLH (tUnqualified lzusr) luid userLHStatus $> DisableLegalHoldSuccess
  case userLHStatus of
    -- no state change necessary
    UserLegalHoldDisabled -> pure DisableLegalHoldWasNotEnabled
    UserLegalHoldNoConsent ->
      -- no state change allowed
      -- we cannot go to disabled because that would subsume consent
      pure DisableLegalHoldWasNotEnabled
    -- LH is enabled or pending, we can disable (change state) without issue
    UserLegalHoldEnabled -> doDisable
    UserLegalHoldPending -> doDisable
  where
    disableLH :: UserId -> Local UserId -> UserLegalHoldStatus -> Sem r ()
    disableLH zusr luid userLHStatus = do
      ensureReAuthorised zusr mPassword Nothing Nothing
      removeLegalHoldClientFromUser uid
      LHService.removeLegalHold tid luid
      -- TODO: send event at this point (see also: related TODO in this module in
      -- 'approveDevice' and
      -- https://github.com/wireapp/wire-server/pull/802#pullrequestreview-262280386)
      changeLegalholdStatusAndHandlePolicyConflicts tid luid userLHStatus UserLegalHoldDisabled

-- | Allow no-consent or requested => consent without further changes.  If LH device is
-- enabled, or disabled, make sure the affected connections are screened for policy conflict
-- (anybody with no-consent), and put those connections in the appropriate blocked state.
changeLegalholdStatusAndHandlePolicyConflicts ::
  ( Member BackendNotificationQueueAccess r,
    Member BrigAccess r,
    Member ConversationStore r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member (ErrorS ('ActionDenied 'RemoveConversationMember)) r,
    Member (ErrorS 'LegalHoldCouldNotBlockConnections) r,
    Member (ErrorS 'UserLegalHoldIllegalOperation) r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member NotificationSubsystem r,
    Member (Input Env) r,
    Member (Input UTCTime) r,
    Member LegalHoldStore r,
    Member (ListItems LegacyPaging ConvId) r,
    Member MemberStore r,
    Member TeamStore r,
    Member ProposalStore r,
    Member Random r,
    Member P.TinyLog r,
    Member SubConversationStore r
  ) =>
  TeamId ->
  Local UserId ->
  UserLegalHoldStatus ->
  UserLegalHoldStatus ->
  Sem r ()
changeLegalholdStatusAndHandlePolicyConflicts tid luid old new = do
  case old of
    UserLegalHoldEnabled -> case new of
      UserLegalHoldEnabled -> noop
      UserLegalHoldPending -> illegal
      UserLegalHoldDisabled -> update >> removeBlocks
      UserLegalHoldNoConsent -> illegal
    --
    UserLegalHoldPending -> case new of
      UserLegalHoldEnabled -> addBlocks >> update
      UserLegalHoldPending -> noop
      UserLegalHoldDisabled -> update >> removeBlocks
      UserLegalHoldNoConsent -> illegal
    --
    UserLegalHoldDisabled -> case new of
      UserLegalHoldEnabled -> illegal
      UserLegalHoldPending -> update
      UserLegalHoldDisabled -> {- in case the last attempt crashed -} removeBlocks
      UserLegalHoldNoConsent -> {- withdrawing consent is not (yet?) implemented -} illegal
    --
    UserLegalHoldNoConsent -> case new of
      UserLegalHoldEnabled -> illegal
      UserLegalHoldPending -> illegal
      UserLegalHoldDisabled -> update
      UserLegalHoldNoConsent -> noop
  where
    update = LegalHoldData.setUserLegalHoldStatus tid (tUnqualified luid) new
    removeBlocks = void $ putConnectionInternal (RemoveLHBlocksInvolving (tUnqualified luid))
    addBlocks = do
      blockNonConsentingConnections (tUnqualified luid)
      handleGroupConvPolicyConflicts luid new
    noop = pure ()
    illegal = throwS @'UserLegalHoldIllegalOperation

-- FUTUREWORK: make this async?
blockNonConsentingConnections ::
  forall r.
  ( Member BrigAccess r,
    Member TeamStore r,
    Member P.TinyLog r,
    Member (ErrorS 'LegalHoldCouldNotBlockConnections) r
  ) =>
  UserId ->
  Sem r ()
blockNonConsentingConnections uid = do
  conns <- getConnectionsUnqualified [uid] Nothing Nothing
  errmsgs <- do
    conflicts <- mconcat <$> findConflicts conns
    blockConflicts uid conflicts
  case mconcat errmsgs of
    [] -> pure ()
    msgs@(_ : _) -> do
      P.warn $ Log.msg @String msgs
      throwS @'LegalHoldCouldNotBlockConnections
  where
    findConflicts :: [ConnectionStatus] -> Sem r [[UserId]]
    findConflicts conns = do
      let (FutureWork @'Public.LegalholdPlusFederationNotImplemented -> _remoteUids, localUids) = (undefined, csTo <$> conns)
      -- FUTUREWORK: Handle remoteUsers here when federation is implemented
      for (chunksOf 32 localUids) $ \others -> do
        teamsOfUsers <- getUsersTeams others
        filterM (fmap (== ConsentNotGiven) . checkConsent teamsOfUsers) others

    blockConflicts :: UserId -> [UserId] -> Sem r [String]
    blockConflicts _ [] = pure []
    blockConflicts userLegalhold othersToBlock@(_ : _) = do
      status <- putConnectionInternal (BlockForMissingLHConsent userLegalhold othersToBlock)
      pure $ ["blocking users failed: " <> show (status, othersToBlock) | status /= status200]

unsetTeamLegalholdWhitelistedH :: (Member LegalHoldStore r) => TeamId -> Sem r ()
unsetTeamLegalholdWhitelistedH tid = do
  () <-
    error
      "FUTUREWORK: if we remove entries from the list, that means removing an unknown \
      \number of LH devices as well, and possibly other things.  think this through \
      \before you enable the end-point."
  LegalHoldData.unsetTeamLegalholdWhitelisted tid

-- | Make sure that enough people are removed from all conversations that contain user `uid`
-- that no policy conflict arises.
--
-- It is guaranteed that no group will ever end up without a group admin because of a policy
-- conflict: If at least one group admin has 'ConsentGiven', non-consenting users are removed.
-- Otherwise, we assume that the group is dominated by people not interested in giving
-- consent, and users carrying LH devices are removed instead.
--
-- The first argument to this function needs explaining: in order to guarantee that this
-- function terminates before we set the LH of user `uid` on pending, we need to call it
-- first.  This means that user `uid` has outdated LH status while this function is running,
-- which may cause wrong behavior.  In order to guarantee correct behavior, the first argument
-- contains the hypothetical new LH status of `uid`'s so it can be consulted instead of the
-- one from the database.
handleGroupConvPolicyConflicts ::
  ( Member BackendNotificationQueueAccess r,
    Member ConversationStore r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member (ErrorS ('ActionDenied 'RemoveConversationMember)) r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member NotificationSubsystem r,
    Member (Input Env) r,
    Member (Input UTCTime) r,
    Member (ListItems LegacyPaging ConvId) r,
    Member MemberStore r,
    Member ProposalStore r,
    Member P.TinyLog r,
    Member Random r,
    Member SubConversationStore r,
    Member TeamStore r
  ) =>
  Local UserId ->
  UserLegalHoldStatus ->
  Sem r ()
handleGroupConvPolicyConflicts luid hypotheticalLHStatus = do
  void $
    iterateConversations luid (toRange (Proxy @500)) $ \convs -> do
      for_ (filter ((== RegularConv) . Data.convType) convs) $ \conv -> do
        let FutureWork _convRemoteMembers' = FutureWork @'LegalholdPlusFederationNotImplemented Data.convRemoteMembers

        membersAndLHStatus :: [(LocalMember, UserLegalHoldStatus)] <- do
          let mems = Data.convLocalMembers conv
          uidsLHStatus <- getLHStatusForUsers (lmId <$> mems)
          pure $
            zipWith
              ( \mem (mid, status) ->
                  assert (lmId mem == mid) $
                    if lmId mem == tUnqualified luid
                      then (mem, hypotheticalLHStatus)
                      else (mem, status)
              )
              mems
              uidsLHStatus

        let lcnv = qualifyAs luid (Data.convId conv)
        -- we know that this is a group conversation, so invalid operation
        -- and conversation not found errors cannot actually be thrown
        mapToRuntimeError @'InvalidOperation
          (InternalErrorWithDescription "expected group conversation while handling policy conflicts")
          . mapToRuntimeError @'ConvNotFound
            (InternalErrorWithDescription "conversation disappeared while iterating on a list of conversations")
          . mapErrorS @('ActionDenied 'LeaveConversation) @('ActionDenied 'RemoveConversationMember)
          $ if any
            ((== ConsentGiven) . consentGiven . snd)
            (filter ((== roleNameWireAdmin) . lmConvRoleName . fst) membersAndLHStatus)
            then do
              for_ (filter ((== ConsentNotGiven) . consentGiven . snd) membersAndLHStatus) $ \(memberNoConsent, _) -> do
                let lusr = qualifyAs luid (lmId memberNoConsent)
                removeMemberFromLocalConv lcnv lusr Nothing (tUntagged lusr)
            else do
              for_ (filter (userLHEnabled . snd) membersAndLHStatus) $ \(legalholder, _) -> do
                let lusr = qualifyAs luid (lmId legalholder)
                removeMemberFromLocalConv lcnv lusr Nothing (tUntagged lusr)
