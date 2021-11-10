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

module Galley.API.LegalHold
  ( createSettingsH,
    getSettingsH,
    removeSettingsH,
    removeSettings',
    getUserStatusH,
    grantConsentH,
    requestDeviceH,
    approveDeviceH,
    disableForUserH,
    isLegalHoldEnabledForTeam,
    setTeamLegalholdWhitelistedH,
    unsetTeamLegalholdWhitelistedH,
    getTeamLegalholdWhitelistedH,
  )
where

import Brig.Types.Client.Prekey
import Brig.Types.Connection (UpdateConnectionsInternal (..))
import Brig.Types.Provider
import Brig.Types.Team.LegalHold hiding (userId)
import Control.Exception (assert)
import Control.Lens (view, (^.))
import Data.ByteString.Conversion (toByteString, toByteString')
import Data.Id
import Data.LegalHold (UserLegalHoldStatus (..), defUserLegalHoldStatus)
import Data.List.Split (chunksOf)
import Data.Misc
import Data.Proxy (Proxy (Proxy))
import Data.Qualified
import Data.Range (toRange)
import Data.Time.Clock
import Galley.API.Error
import Galley.API.Query (iterateConversations)
import Galley.API.Update (removeMemberFromLocalConv)
import Galley.API.Util
import Galley.App
import Galley.Cassandra.Paging
import qualified Galley.Data.Conversation as Data
import Galley.Effects
import Galley.Effects.BrigAccess
import qualified Galley.Effects.LegalHoldStore as LegalHoldData
import Galley.Effects.Paging
import qualified Galley.Effects.TeamFeatureStore as TeamFeatures
import Galley.Effects.TeamMemberStore
import Galley.Effects.TeamStore
import Galley.Effects.WaiRoutes
import qualified Galley.External.LegalHoldService as LHService
import Galley.Types (LocalMember, lmConvRoleName, lmId)
import Galley.Types.Teams as Team
import Imports
import Network.HTTP.Types (status200, status404)
import Network.HTTP.Types.Status (status201, status204)
import Network.Wai
import Network.Wai.Predicate hiding (Error, or, result, setStatus, _3)
import Network.Wai.Utilities as Wai hiding (Error)
import Polysemy
import Polysemy.Error
import Polysemy.Input
import qualified Polysemy.TinyLog as P
import qualified System.Logger.Class as Log
import Wire.API.Conversation (ConvType (..))
import Wire.API.Conversation.Role (roleNameWireAdmin)
import Wire.API.ErrorDescription
import Wire.API.Federation.Client
import Wire.API.Routes.Internal.Brig.Connection
import qualified Wire.API.Team.Feature as Public
import Wire.API.Team.LegalHold (LegalholdProtectee (LegalholdPlusFederationNotImplemented))
import qualified Wire.API.Team.LegalHold as Public

assertLegalHoldEnabledForTeam ::
  Members '[Error LegalHoldError, Error NotATeamMember, LegalHoldStore, TeamStore, TeamFeatureStore] r =>
  TeamId ->
  Galley r ()
assertLegalHoldEnabledForTeam tid =
  unlessM (isLegalHoldEnabledForTeam tid) $
    liftSem $ throw LegalHoldNotEnabled

isLegalHoldEnabledForTeam ::
  Members '[LegalHoldStore, TeamStore, TeamFeatureStore] r =>
  TeamId ->
  Galley r Bool
isLegalHoldEnabledForTeam tid = do
  liftSem getLegalHoldFlag >>= \case
    FeatureLegalHoldDisabledPermanently -> do
      pure False
    FeatureLegalHoldDisabledByDefault -> do
      statusValue <-
        liftSem $
          Public.tfwoStatus <$$> TeamFeatures.getFeatureStatusNoConfig @'Public.TeamFeatureLegalHold tid
      return $ case statusValue of
        Just Public.TeamFeatureEnabled -> True
        Just Public.TeamFeatureDisabled -> False
        Nothing -> False
    FeatureLegalHoldWhitelistTeamsAndImplicitConsent ->
      liftSem $ LegalHoldData.isTeamLegalholdWhitelisted tid

createSettingsH ::
  Members
    '[ Error ActionError,
       Error InvalidInput,
       Error LegalHoldError,
       Error NotATeamMember,
       Error TeamError,
       LegalHoldStore,
       TeamFeatureStore,
       TeamStore,
       P.TinyLog,
       WaiRoutes
     ]
    r =>
  UserId ::: TeamId ::: JsonRequest Public.NewLegalHoldService ::: JSON ->
  Galley r Response
createSettingsH (zusr ::: tid ::: req ::: _) = do
  newService <- liftSem $ fromJsonBody req
  setStatus status201 . json <$> createSettings zusr tid newService

createSettings ::
  Members
    '[ Error ActionError,
       Error InvalidInput,
       Error LegalHoldError,
       Error NotATeamMember,
       Error TeamError,
       LegalHoldStore,
       TeamFeatureStore,
       TeamStore,
       P.TinyLog
     ]
    r =>
  UserId ->
  TeamId ->
  Public.NewLegalHoldService ->
  Galley r Public.ViewLegalHoldService
createSettings zusr tid newService = do
  assertLegalHoldEnabledForTeam tid
  zusrMembership <- liftSem $ getTeamMember tid zusr
  -- let zothers = map (view userId) membs
  -- Log.debug $
  --   Log.field "targets" (toByteString . show $ toByteString <$> zothers)
  --     . Log.field "action" (Log.val "LegalHold.createSettings")
  void $ permissionCheck ChangeLegalHoldTeamSettings zusrMembership
  (key :: ServiceKey, fpr :: Fingerprint Rsa) <-
    liftSem $
      LegalHoldData.validateServiceKey (newLegalHoldServiceKey newService)
        >>= note LegalHoldServiceInvalidKey
  LHService.checkLegalHoldServiceStatus fpr (newLegalHoldServiceUrl newService)
  let service = legalHoldService tid fpr newService key
  liftSem $ LegalHoldData.createSettings service
  pure . viewLegalHoldService $ service

getSettingsH ::
  Members
    '[ Error ActionError,
       Error InvalidInput,
       Error TeamError,
       Error NotATeamMember,
       LegalHoldStore,
       TeamFeatureStore,
       TeamStore,
       P.TinyLog
     ]
    r =>
  UserId ::: TeamId ::: JSON ->
  Galley r Response
getSettingsH (zusr ::: tid ::: _) = do
  json <$> getSettings zusr tid

getSettings ::
  Members
    '[ Error ActionError,
       Error InvalidInput,
       Error TeamError,
       Error NotATeamMember,
       LegalHoldStore,
       TeamFeatureStore,
       TeamStore,
       P.TinyLog
     ]
    r =>
  UserId ->
  TeamId ->
  Galley r Public.ViewLegalHoldService
getSettings zusr tid = do
  zusrMembership <- liftSem $ getTeamMember tid zusr
  void $ permissionCheck (ViewTeamFeature Public.TeamFeatureLegalHold) zusrMembership
  isenabled <- isLegalHoldEnabledForTeam tid
  mresult <- liftSem $ LegalHoldData.getSettings tid
  pure $ case (isenabled, mresult) of
    (False, _) -> Public.ViewLegalHoldServiceDisabled
    (True, Nothing) -> Public.ViewLegalHoldServiceNotConfigured
    (True, Just result) -> viewLegalHoldService result

removeSettingsH ::
  Members
    '[ BotAccess,
       BrigAccess,
       CodeStore,
       ConversationStore,
       Error ActionError,
       Error InvalidInput,
       Error AuthenticationError,
       Error ConversationError,
       Error FederationError,
       Error LegalHoldError,
       Error NotATeamMember,
       Error TeamError,
       ExternalAccess,
       FederatorAccess,
       FireAndForget,
       GundeckAccess,
       Input UTCTime,
       Input (Local ()),
       LegalHoldStore,
       ListItems LegacyPaging ConvId,
       MemberStore,
       TeamFeatureStore,
       TeamMemberStore InternalPaging,
       TeamStore,
       P.TinyLog,
       WaiRoutes
     ]
    r =>
  UserId ::: TeamId ::: JsonRequest Public.RemoveLegalHoldSettingsRequest ::: JSON ->
  Galley r Response
removeSettingsH (zusr ::: tid ::: req ::: _) = do
  removeSettingsRequest <- liftSem $ fromJsonBody req
  removeSettings zusr tid removeSettingsRequest
  pure noContent

removeSettings ::
  ( Paging p,
    Bounded (PagingBounds p TeamMember),
    Members
      '[ BotAccess,
         BrigAccess,
         CodeStore,
         ConversationStore,
         Error ActionError,
         Error InvalidInput,
         Error AuthenticationError,
         Error ConversationError,
         Error FederationError,
         Error LegalHoldError,
         Error NotATeamMember,
         Error TeamError,
         ExternalAccess,
         FederatorAccess,
         FireAndForget,
         GundeckAccess,
         Input UTCTime,
         Input (Local ()),
         LegalHoldStore,
         ListItems LegacyPaging ConvId,
         MemberStore,
         TeamFeatureStore,
         TeamMemberStore p,
         TeamStore,
         P.TinyLog
       ]
      r
  ) =>
  UserId ->
  TeamId ->
  Public.RemoveLegalHoldSettingsRequest ->
  Galley r ()
removeSettings zusr tid (Public.RemoveLegalHoldSettingsRequest mPassword) = do
  liftSem assertNotWhitelisting
  assertLegalHoldEnabledForTeam tid
  zusrMembership <- liftSem $ getTeamMember tid zusr
  -- let zothers = map (view userId) membs
  -- Log.debug $
  --   Log.field "targets" (toByteString . show $ toByteString <$> zothers)
  --     . Log.field "action" (Log.val "LegalHold.removeSettings")
  void $ permissionCheck ChangeLegalHoldTeamSettings zusrMembership
  ensureReAuthorised zusr mPassword
  removeSettings' tid
  where
    assertNotWhitelisting :: Members '[Error LegalHoldError, TeamStore] r => Sem r ()
    assertNotWhitelisting = do
      getLegalHoldFlag >>= \case
        FeatureLegalHoldDisabledPermanently -> pure ()
        FeatureLegalHoldDisabledByDefault -> pure ()
        FeatureLegalHoldWhitelistTeamsAndImplicitConsent ->
          throw LegalHoldDisableUnimplemented

-- | Remove legal hold settings from team; also disabling for all users and removing LH devices
removeSettings' ::
  forall p r.
  ( Paging p,
    Bounded (PagingBounds p TeamMember),
    Members
      '[ BotAccess,
         BrigAccess,
         CodeStore,
         ConversationStore,
         Error ActionError,
         Error InvalidInput,
         Error AuthenticationError,
         Error ConversationError,
         Error FederationError,
         Error LegalHoldError,
         Error NotATeamMember,
         Error TeamError,
         ExternalAccess,
         FederatorAccess,
         FireAndForget,
         GundeckAccess,
         Input UTCTime,
         Input (Local ()),
         LegalHoldStore,
         ListItems LegacyPaging ConvId,
         MemberStore,
         TeamMemberStore p,
         TeamStore,
         P.TinyLog
       ]
      r
  ) =>
  TeamId ->
  Galley r ()
removeSettings' tid =
  withChunks
    (\mps -> liftSem (listTeamMembers tid mps maxBound))
    action
  where
    action :: [TeamMember] -> Galley r ()
    action membs = do
      let zothers = map (view userId) membs
      let lhMembers = filter ((== UserLegalHoldEnabled) . view legalHoldStatus) membs
      liftSem . P.debug $
        Log.field "targets" (toByteString . show $ toByteString <$> zothers)
          . Log.field "action" (Log.val "LegalHold.removeSettings'")
      spawnMany (map removeLHForUser lhMembers)
    removeLHForUser :: TeamMember -> Galley r ()
    removeLHForUser member = do
      luid <- liftSem $ qualifyLocal (member ^. Team.userId)
      liftSem $ removeLegalHoldClientFromUser (tUnqualified luid)
      LHService.removeLegalHold tid (tUnqualified luid)
      changeLegalholdStatus tid luid (member ^. legalHoldStatus) UserLegalHoldDisabled -- (support for withdrawing consent is not planned yet.)

-- | Learn whether a user has LH enabled and fetch pre-keys.
-- Note that this is accessible to ANY authenticated user, even ones outside the team
getUserStatusH ::
  Members '[Error InternalError, Error TeamError, LegalHoldStore, TeamStore, P.TinyLog] r =>
  UserId ::: TeamId ::: UserId ::: JSON ->
  Galley r Response
getUserStatusH (_zusr ::: tid ::: uid ::: _) = do
  json <$> getUserStatus tid uid

getUserStatus ::
  forall r.
  Members '[Error InternalError, Error TeamError, LegalHoldStore, TeamStore, P.TinyLog] r =>
  TeamId ->
  UserId ->
  Galley r Public.UserLegalHoldStatusResponse
getUserStatus tid uid = do
  teamMember <- liftSem $ note TeamMemberNotFound =<< getTeamMember tid uid
  let status = view legalHoldStatus teamMember
  (mlk, lcid) <- case status of
    UserLegalHoldNoConsent -> pure (Nothing, Nothing)
    UserLegalHoldDisabled -> pure (Nothing, Nothing)
    UserLegalHoldPending -> makeResponseDetails
    UserLegalHoldEnabled -> makeResponseDetails
  pure $ UserLegalHoldStatusResponse status mlk lcid
  where
    makeResponseDetails :: Galley r (Maybe LastPrekey, Maybe ClientId)
    makeResponseDetails = do
      mLastKey <- liftSem $ fmap snd <$> LegalHoldData.selectPendingPrekeys uid
      lastKey <- case mLastKey of
        Nothing -> liftSem $ do
          P.err . Log.msg $
            "expected to find a prekey for user: "
              <> toByteString' uid
              <> " but none was found"
          throw NoPrekeyForUser
        Just lstKey -> pure lstKey
      let clientId = clientIdFromPrekey . unpackLastPrekey $ lastKey
      pure (Just lastKey, Just clientId)

-- | Change 'UserLegalHoldStatus' from no consent to disabled.  FUTUREWORK:
-- @withdrawExplicitConsentH@ (lots of corner cases we'd have to implement for that to pan
-- out).
grantConsentH ::
  Members
    '[ BotAccess,
       BrigAccess,
       CodeStore,
       ConversationStore,
       Error ActionError,
       Error InvalidInput,
       Error ConversationError,
       Error FederationError,
       Error LegalHoldError,
       Error NotATeamMember,
       Error TeamError,
       ExternalAccess,
       FederatorAccess,
       FireAndForget,
       GundeckAccess,
       Input UTCTime,
       Input (Local ()),
       LegalHoldStore,
       ListItems LegacyPaging ConvId,
       MemberStore,
       TeamStore,
       P.TinyLog
     ]
    r =>
  UserId ::: TeamId ::: JSON ->
  Galley r Response
grantConsentH (zusr ::: tid ::: _) = do
  lusr <- liftSem $ qualifyLocal zusr
  grantConsent lusr tid >>= \case
    GrantConsentSuccess -> pure $ empty & setStatus status201
    GrantConsentAlreadyGranted -> pure $ empty & setStatus status204

data GrantConsentResult
  = GrantConsentSuccess
  | GrantConsentAlreadyGranted

grantConsent ::
  Members
    '[ BotAccess,
       BrigAccess,
       CodeStore,
       ConversationStore,
       Error ActionError,
       Error InvalidInput,
       Error ConversationError,
       Error FederationError,
       Error LegalHoldError,
       Error NotATeamMember,
       Error TeamError,
       ExternalAccess,
       FederatorAccess,
       FireAndForget,
       GundeckAccess,
       Input UTCTime,
       LegalHoldStore,
       ListItems LegacyPaging ConvId,
       MemberStore,
       TeamStore,
       P.TinyLog
     ]
    r =>
  Local UserId ->
  TeamId ->
  Galley r GrantConsentResult
grantConsent lusr tid = do
  userLHStatus <-
    liftSem $
      note TeamMemberNotFound
        =<< fmap (view legalHoldStatus) <$> getTeamMember tid (tUnqualified lusr)
  case userLHStatus of
    lhs@UserLegalHoldNoConsent ->
      changeLegalholdStatus tid lusr lhs UserLegalHoldDisabled $> GrantConsentSuccess
    UserLegalHoldEnabled -> pure GrantConsentAlreadyGranted
    UserLegalHoldPending -> pure GrantConsentAlreadyGranted
    UserLegalHoldDisabled -> pure GrantConsentAlreadyGranted

-- | Request to provision a device on the legal hold service for a user
requestDeviceH ::
  Members
    '[ BotAccess,
       BrigAccess,
       CodeStore,
       ConversationStore,
       Error ActionError,
       Error InvalidInput,
       Error ConversationError,
       Error FederationError,
       Error LegalHoldError,
       Error NotATeamMember,
       Error TeamError,
       ExternalAccess,
       FederatorAccess,
       FireAndForget,
       GundeckAccess,
       Input UTCTime,
       Input (Local ()),
       LegalHoldStore,
       ListItems LegacyPaging ConvId,
       MemberStore,
       TeamFeatureStore,
       TeamStore,
       P.TinyLog
     ]
    r =>
  UserId ::: TeamId ::: UserId ::: JSON ->
  Galley r Response
requestDeviceH (zusr ::: tid ::: uid ::: _) = do
  luid <- liftSem $ qualifyLocal uid
  requestDevice zusr tid luid <&> \case
    RequestDeviceSuccess -> empty & setStatus status201
    RequestDeviceAlreadyPending -> empty & setStatus status204

data RequestDeviceResult
  = RequestDeviceSuccess
  | RequestDeviceAlreadyPending

requestDevice ::
  forall r.
  Members
    '[ BotAccess,
       BrigAccess,
       CodeStore,
       ConversationStore,
       Error ActionError,
       Error InvalidInput,
       Error ConversationError,
       Error FederationError,
       Error LegalHoldError,
       Error NotATeamMember,
       Error TeamError,
       ExternalAccess,
       FederatorAccess,
       FireAndForget,
       GundeckAccess,
       Input UTCTime,
       LegalHoldStore,
       ListItems LegacyPaging ConvId,
       MemberStore,
       TeamFeatureStore,
       TeamStore,
       P.TinyLog
     ]
    r =>
  UserId ->
  TeamId ->
  Local UserId ->
  Galley r RequestDeviceResult
requestDevice zusr tid luid = do
  assertLegalHoldEnabledForTeam tid
  liftSem . P.debug $
    Log.field "targets" (toByteString (tUnqualified luid))
      . Log.field "action" (Log.val "LegalHold.requestDevice")
  zusrMembership <- liftSem $ getTeamMember tid zusr
  void $ permissionCheck ChangeLegalHoldUserSettings zusrMembership
  member <- liftSem $ note TeamMemberNotFound =<< getTeamMember tid (tUnqualified luid)
  case member ^. legalHoldStatus of
    UserLegalHoldEnabled -> liftSem $ throw UserLegalHoldAlreadyEnabled
    lhs@UserLegalHoldPending -> RequestDeviceAlreadyPending <$ provisionLHDevice lhs
    lhs@UserLegalHoldDisabled -> RequestDeviceSuccess <$ provisionLHDevice lhs
    UserLegalHoldNoConsent -> liftSem $ throw NoUserLegalHoldConsent
  where
    -- Wire's LH service that galley is usually calling here is idempotent in device creation,
    -- ie. it returns the existing device on multiple calls to `/init`, like here:
    -- https://github.com/wireapp/legalhold/blob/e0a241162b9dbc841f12fbc57c8a1e1093c7e83a/src/main/java/com/wire/bots/hold/resource/InitiateResource.java#L42
    --
    -- This will still work if the LH service creates two new device on two consecutive calls
    -- to `/init`, but there may be race conditions, eg. when updating and enabling a pending
    -- device at (almost) the same time.
    provisionLHDevice :: UserLegalHoldStatus -> Galley r ()
    provisionLHDevice userLHStatus = do
      (lastPrekey', prekeys) <- requestDeviceFromService
      -- We don't distinguish the last key here; brig will do so when the device is added
      liftSem $ LegalHoldData.insertPendingPrekeys (tUnqualified luid) (unpackLastPrekey lastPrekey' : prekeys)
      changeLegalholdStatus tid luid userLHStatus UserLegalHoldPending
      liftSem $ notifyClientsAboutLegalHoldRequest zusr (tUnqualified luid) lastPrekey'

    requestDeviceFromService :: Galley r (LastPrekey, [Prekey])
    requestDeviceFromService = do
      liftSem $ LegalHoldData.dropPendingPrekeys (tUnqualified luid)
      lhDevice <- LHService.requestNewDevice tid (tUnqualified luid)
      let NewLegalHoldClient prekeys lastKey = lhDevice
      return (lastKey, prekeys)

-- | Approve the adding of a Legal Hold device to the user.
--
-- We don't delete pending prekeys during this flow just in case
-- it gets interupted. There's really no reason to delete them anyways
-- since they are replaced if needed when registering new LH devices.
approveDeviceH ::
  Members
    '[ BotAccess,
       BrigAccess,
       CodeStore,
       ConversationStore,
       Error ActionError,
       Error InvalidInput,
       Error AuthenticationError,
       Error ConversationError,
       Error FederationError,
       Error LegalHoldError,
       Error NotATeamMember,
       Error TeamError,
       ExternalAccess,
       FederatorAccess,
       FireAndForget,
       GundeckAccess,
       Input UTCTime,
       Input (Local ()),
       LegalHoldStore,
       ListItems LegacyPaging ConvId,
       MemberStore,
       TeamFeatureStore,
       TeamStore,
       P.TinyLog,
       WaiRoutes
     ]
    r =>
  UserId ::: TeamId ::: UserId ::: ConnId ::: JsonRequest Public.ApproveLegalHoldForUserRequest ::: JSON ->
  Galley r Response
approveDeviceH (zusr ::: tid ::: uid ::: connId ::: req ::: _) = do
  luid <- liftSem $ qualifyLocal uid
  approve <- liftSem $ fromJsonBody req
  approveDevice zusr tid luid connId approve
  pure empty

approveDevice ::
  Members
    '[ BotAccess,
       BrigAccess,
       CodeStore,
       ConversationStore,
       Error ActionError,
       Error InvalidInput,
       Error AuthenticationError,
       Error ConversationError,
       Error FederationError,
       Error LegalHoldError,
       Error NotATeamMember,
       Error TeamError,
       ExternalAccess,
       FederatorAccess,
       FireAndForget,
       GundeckAccess,
       Input UTCTime,
       LegalHoldStore,
       ListItems LegacyPaging ConvId,
       MemberStore,
       TeamFeatureStore,
       TeamStore,
       P.TinyLog
     ]
    r =>
  UserId ->
  TeamId ->
  Local UserId ->
  ConnId ->
  Public.ApproveLegalHoldForUserRequest ->
  Galley r ()
approveDevice zusr tid luid connId (Public.ApproveLegalHoldForUserRequest mPassword) = do
  assertLegalHoldEnabledForTeam tid
  liftSem . P.debug $
    Log.field "targets" (toByteString (tUnqualified luid))
      . Log.field "action" (Log.val "LegalHold.approveDevice")
  liftSem . unless (zusr == tUnqualified luid) $ throw AccessDenied
  assertOnTeam (tUnqualified luid) tid
  ensureReAuthorised zusr mPassword
  userLHStatus <-
    liftSem $
      maybe defUserLegalHoldStatus (view legalHoldStatus) <$> getTeamMember tid (tUnqualified luid)
  assertUserLHPending userLHStatus
  mPreKeys <- liftSem $ LegalHoldData.selectPendingPrekeys (tUnqualified luid)
  (prekeys, lastPrekey') <- case mPreKeys of
    Nothing -> liftSem $ do
      P.info $ Log.msg @Text "No prekeys found"
      throw NoLegalHoldDeviceAllocated
    Just keys -> pure keys
  clientId <- liftSem $ addLegalHoldClientToUser (tUnqualified luid) connId prekeys lastPrekey'
  -- Note: teamId could be passed in the getLegalHoldAuthToken request instead of lookup up again
  -- Note: both 'getLegalHoldToken' and 'ensureReAuthorized' check the password
  -- Note: both 'getLegalHoldToken' and this function in 'assertOnTeam' above
  --       checks that the user is part of a binding team
  -- FUTUREWORK: reduce double checks
  legalHoldAuthToken <- liftSem $ getLegalHoldAuthToken (tUnqualified luid) mPassword
  LHService.confirmLegalHold clientId tid (tUnqualified luid) legalHoldAuthToken
  -- TODO: send event at this point (see also:
  -- https://github.com/wireapp/wire-server/pull/802#pullrequestreview-262280386)
  changeLegalholdStatus tid luid userLHStatus UserLegalHoldEnabled
  where
    assertUserLHPending :: Member (Error LegalHoldError) r => UserLegalHoldStatus -> Galley r ()
    assertUserLHPending userLHStatus = liftSem $ do
      case userLHStatus of
        UserLegalHoldEnabled -> throw UserLegalHoldAlreadyEnabled
        UserLegalHoldPending -> pure ()
        UserLegalHoldDisabled -> throw UserLegalHoldNotPending
        UserLegalHoldNoConsent -> throw UserLegalHoldNotPending

disableForUserH ::
  Members
    '[ BotAccess,
       BrigAccess,
       CodeStore,
       ConversationStore,
       Error ActionError,
       Error InvalidInput,
       Error AuthenticationError,
       Error ConversationError,
       Error FederationError,
       Error LegalHoldError,
       Error NotATeamMember,
       Error TeamError,
       ExternalAccess,
       FederatorAccess,
       FireAndForget,
       GundeckAccess,
       Input UTCTime,
       Input (Local ()),
       LegalHoldStore,
       ListItems LegacyPaging ConvId,
       MemberStore,
       TeamStore,
       P.TinyLog,
       WaiRoutes
     ]
    r =>
  UserId ::: TeamId ::: UserId ::: JsonRequest Public.DisableLegalHoldForUserRequest ::: JSON ->
  Galley r Response
disableForUserH (zusr ::: tid ::: uid ::: req ::: _) = do
  luid <- liftSem $ qualifyLocal uid
  disable <- liftSem $ fromJsonBody req
  disableForUser zusr tid luid disable <&> \case
    DisableLegalHoldSuccess -> empty
    DisableLegalHoldWasNotEnabled -> noContent

data DisableLegalHoldForUserResponse
  = DisableLegalHoldSuccess
  | DisableLegalHoldWasNotEnabled

disableForUser ::
  forall r.
  Members
    '[ BotAccess,
       BrigAccess,
       CodeStore,
       ConversationStore,
       Error ActionError,
       Error InvalidInput,
       Error AuthenticationError,
       Error ConversationError,
       Error FederationError,
       Error LegalHoldError,
       Error NotATeamMember,
       Error TeamError,
       ExternalAccess,
       FederatorAccess,
       FireAndForget,
       GundeckAccess,
       Input UTCTime,
       LegalHoldStore,
       ListItems LegacyPaging ConvId,
       MemberStore,
       TeamStore,
       P.TinyLog
     ]
    r =>
  UserId ->
  TeamId ->
  Local UserId ->
  Public.DisableLegalHoldForUserRequest ->
  Galley r DisableLegalHoldForUserResponse
disableForUser zusr tid luid (Public.DisableLegalHoldForUserRequest mPassword) = do
  liftSem . P.debug $
    Log.field "targets" (toByteString (tUnqualified luid))
      . Log.field "action" (Log.val "LegalHold.disableForUser")
  zusrMembership <- liftSem $ getTeamMember tid zusr
  void $ permissionCheck ChangeLegalHoldUserSettings zusrMembership

  userLHStatus <-
    liftSem $
      maybe defUserLegalHoldStatus (view legalHoldStatus) <$> getTeamMember tid (tUnqualified luid)
  if not $ userLHEnabled userLHStatus
    then pure DisableLegalHoldWasNotEnabled
    else disableLH userLHStatus $> DisableLegalHoldSuccess
  where
    disableLH :: UserLegalHoldStatus -> Galley r ()
    disableLH userLHStatus = do
      ensureReAuthorised zusr mPassword
      liftSem $ removeLegalHoldClientFromUser (tUnqualified luid)
      LHService.removeLegalHold tid (tUnqualified luid)
      -- TODO: send event at this point (see also: related TODO in this module in
      -- 'approveDevice' and
      -- https://github.com/wireapp/wire-server/pull/802#pullrequestreview-262280386)
      changeLegalholdStatus tid luid userLHStatus UserLegalHoldDisabled

-- | Allow no-consent => consent without further changes.  If LH device is requested, enabled,
-- or disabled, make sure the affected connections are screened for policy conflict (anybody
-- with no-consent), and put those connections in the appropriate blocked state.
changeLegalholdStatus ::
  Members
    '[ BotAccess,
       BrigAccess,
       CodeStore,
       ConversationStore,
       Error ActionError,
       Error InvalidInput,
       Error ConversationError,
       Error FederationError,
       Error LegalHoldError,
       Error NotATeamMember,
       Error TeamError,
       ExternalAccess,
       FederatorAccess,
       FireAndForget,
       GundeckAccess,
       Input UTCTime,
       LegalHoldStore,
       ListItems LegacyPaging ConvId,
       MemberStore,
       TeamStore,
       P.TinyLog
     ]
    r =>
  TeamId ->
  Local UserId ->
  UserLegalHoldStatus ->
  UserLegalHoldStatus ->
  Galley r ()
changeLegalholdStatus tid luid old new = do
  case old of
    UserLegalHoldEnabled -> case new of
      UserLegalHoldEnabled -> noop
      UserLegalHoldPending -> illegal
      UserLegalHoldDisabled -> liftSem update >> removeblocks
      UserLegalHoldNoConsent -> illegal
    --
    UserLegalHoldPending -> case new of
      UserLegalHoldEnabled -> liftSem update
      UserLegalHoldPending -> noop
      UserLegalHoldDisabled -> liftSem update >> removeblocks
      UserLegalHoldNoConsent -> illegal
    --
    UserLegalHoldDisabled -> case new of
      UserLegalHoldEnabled -> illegal
      UserLegalHoldPending -> addblocks >> liftSem update
      UserLegalHoldDisabled -> {- in case the last attempt crashed -} removeblocks
      UserLegalHoldNoConsent -> {- withdrawing consent is not (yet?) implemented -} illegal
    --
    UserLegalHoldNoConsent -> case new of
      UserLegalHoldEnabled -> illegal
      UserLegalHoldPending -> illegal
      UserLegalHoldDisabled -> liftSem update
      UserLegalHoldNoConsent -> noop
  where
    update = LegalHoldData.setUserLegalHoldStatus tid (tUnqualified luid) new
    removeblocks = void . liftSem $ putConnectionInternal (RemoveLHBlocksInvolving (tUnqualified luid))
    addblocks = do
      blockNonConsentingConnections (tUnqualified luid)
      handleGroupConvPolicyConflicts luid new
    noop = pure ()
    illegal = liftSem $ throw UserLegalHoldIllegalOperation

-- FUTUREWORK: make this async?
blockNonConsentingConnections ::
  forall r.
  Members
    '[ BrigAccess,
       Error LegalHoldError,
       Error NotATeamMember,
       LegalHoldStore,
       TeamStore,
       P.TinyLog
     ]
    r =>
  UserId ->
  Galley r ()
blockNonConsentingConnections uid = do
  conns <- liftSem $ getConnectionsUnqualified [uid] Nothing Nothing
  errmsgs <- do
    conflicts <- mconcat <$> findConflicts conns
    blockConflicts uid conflicts
  case mconcat errmsgs of
    [] -> pure ()
    msgs@(_ : _) -> liftSem $ do
      P.warn $ Log.msg @String msgs
      throw LegalHoldCouldNotBlockConnections
  where
    findConflicts :: [ConnectionStatus] -> Galley r [[UserId]]
    findConflicts conns = do
      let (FutureWork @'Public.LegalholdPlusFederationNotImplemented -> _remoteUids, localUids) = (undefined, csTo <$> conns)
      -- FUTUREWORK: Handle remoteUsers here when federation is implemented
      for (chunksOf 32 localUids) $ \others -> do
        teamsOfUsers <- liftSem $ getUsersTeams others
        filterM (fmap (== ConsentNotGiven) . checkConsent teamsOfUsers) others

    blockConflicts :: UserId -> [UserId] -> Galley r [String]
    blockConflicts _ [] = pure []
    blockConflicts userLegalhold othersToBlock@(_ : _) = do
      status <- liftSem $ putConnectionInternal (BlockForMissingLHConsent userLegalhold othersToBlock)
      pure $ ["blocking users failed: " <> show (status, othersToBlock) | status /= status200]

setTeamLegalholdWhitelisted :: Member LegalHoldStore r => TeamId -> Galley r ()
setTeamLegalholdWhitelisted tid =
  liftSem $
    LegalHoldData.setTeamLegalholdWhitelisted tid

setTeamLegalholdWhitelistedH :: Member LegalHoldStore r => TeamId -> Galley r Response
setTeamLegalholdWhitelistedH tid = do
  empty <$ setTeamLegalholdWhitelisted tid

unsetTeamLegalholdWhitelisted :: Member LegalHoldStore r => TeamId -> Galley r ()
unsetTeamLegalholdWhitelisted tid =
  liftSem $
    LegalHoldData.unsetTeamLegalholdWhitelisted tid

unsetTeamLegalholdWhitelistedH :: Member LegalHoldStore r => TeamId -> Galley r Response
unsetTeamLegalholdWhitelistedH tid = do
  () <-
    error
      "FUTUREWORK: if we remove entries from the list, that means removing an unknown \
      \number of LH devices as well, and possibly other things.  think this through \
      \before you enable the end-point."
  setStatus status204 empty <$ unsetTeamLegalholdWhitelisted tid

getTeamLegalholdWhitelistedH :: Member LegalHoldStore r => TeamId -> Galley r Response
getTeamLegalholdWhitelistedH tid = liftSem $ do
  lhEnabled <- LegalHoldData.isTeamLegalholdWhitelisted tid
  pure $
    if lhEnabled
      then setStatus status200 empty
      else setStatus status404 empty

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
  Members
    '[ BotAccess,
       BrigAccess,
       CodeStore,
       ConversationStore,
       Error ActionError,
       Error InvalidInput,
       Error ConversationError,
       Error FederationError,
       Error TeamError,
       ExternalAccess,
       FederatorAccess,
       FireAndForget,
       GundeckAccess,
       Input UTCTime,
       LegalHoldStore,
       ListItems LegacyPaging ConvId,
       MemberStore,
       TeamStore,
       P.TinyLog
     ]
    r =>
  Local UserId ->
  UserLegalHoldStatus ->
  Galley r ()
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
        if any
          ((== ConsentGiven) . consentGiven . snd)
          (filter ((== roleNameWireAdmin) . lmConvRoleName . fst) membersAndLHStatus)
          then do
            for_ (filter ((== ConsentNotGiven) . consentGiven . snd) membersAndLHStatus) $ \(memberNoConsent, _) -> do
              let lusr = qualifyAs luid (lmId memberNoConsent)
              removeMemberFromLocalConv lcnv lusr Nothing (qUntagged lusr)
          else do
            for_ (filter (userLHEnabled . snd) membersAndLHStatus) $ \(legalholder, _) -> do
              let lusr = qualifyAs luid (lmId legalholder)
              removeMemberFromLocalConv lcnv lusr Nothing (qUntagged lusr)
