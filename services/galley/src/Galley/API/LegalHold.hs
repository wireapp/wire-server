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
import Control.Monad.Catch
import Data.ByteString.Conversion (toByteString, toByteString')
import Data.Id
import Data.LegalHold (UserLegalHoldStatus (..), defUserLegalHoldStatus)
import Data.List.Split (chunksOf)
import Data.Misc
import Data.Proxy (Proxy (Proxy))
import Data.Qualified (qUntagged)
import Data.Range (toRange)
import Galley.API.Error
import Galley.API.Query (iterateConversations)
import Galley.API.Update (removeMemberFromLocalConv)
import Galley.API.Util
import Galley.App
import qualified Galley.Data as Data
import Galley.Data.LegalHold (isTeamLegalholdWhitelisted)
import qualified Galley.Data.LegalHold as LegalHoldData
import qualified Galley.Data.TeamFeatures as TeamFeatures
import Galley.Effects
import qualified Galley.External.LegalHoldService as LHService
import qualified Galley.Intra.Client as Client
import Galley.Intra.User (getConnectionsUnqualified, putConnectionInternal)
import qualified Galley.Options as Opts
import Galley.Types (LocalMember, lmConvRoleName, lmId)
import Galley.Types.Teams as Team
import Imports
import Network.HTTP.Types (status200, status404)
import Network.HTTP.Types.Status (status201, status204)
import Network.Wai
import Network.Wai.Predicate hiding (or, result, setStatus, _3)
import Network.Wai.Utilities as Wai
import qualified System.Logger.Class as Log
import Wire.API.Conversation (ConvType (..))
import Wire.API.Conversation.Role (roleNameWireAdmin)
import Wire.API.Routes.Internal.Brig.Connection
import qualified Wire.API.Team.Feature as Public
import Wire.API.Team.LegalHold (LegalholdProtectee (LegalholdPlusFederationNotImplemented))
import qualified Wire.API.Team.LegalHold as Public

assertLegalHoldEnabledForTeam :: TeamId -> Galley r ()
assertLegalHoldEnabledForTeam tid = unlessM (isLegalHoldEnabledForTeam tid) $ throwM legalHoldNotEnabled

isLegalHoldEnabledForTeam :: TeamId -> Galley r Bool
isLegalHoldEnabledForTeam tid = do
  view (options . Opts.optSettings . Opts.setFeatureFlags . flagLegalHold) >>= \case
    FeatureLegalHoldDisabledPermanently -> do
      pure False
    FeatureLegalHoldDisabledByDefault -> do
      statusValue <- Public.tfwoStatus <$$> TeamFeatures.getFeatureStatusNoConfig @'Public.TeamFeatureLegalHold tid
      return $ case statusValue of
        Just Public.TeamFeatureEnabled -> True
        Just Public.TeamFeatureDisabled -> False
        Nothing -> False
    FeatureLegalHoldWhitelistTeamsAndImplicitConsent ->
      isTeamLegalholdWhitelisted tid

createSettingsH :: UserId ::: TeamId ::: JsonRequest Public.NewLegalHoldService ::: JSON -> Galley r Response
createSettingsH (zusr ::: tid ::: req ::: _) = do
  newService <- fromJsonBody req
  setStatus status201 . json <$> createSettings zusr tid newService

createSettings :: UserId -> TeamId -> Public.NewLegalHoldService -> Galley r Public.ViewLegalHoldService
createSettings zusr tid newService = do
  assertLegalHoldEnabledForTeam tid
  zusrMembership <- Data.teamMember tid zusr
  -- let zothers = map (view userId) membs
  -- Log.debug $
  --   Log.field "targets" (toByteString . show $ toByteString <$> zothers)
  --     . Log.field "action" (Log.val "LegalHold.createSettings")
  void $ permissionCheck ChangeLegalHoldTeamSettings zusrMembership
  (key :: ServiceKey, fpr :: Fingerprint Rsa) <-
    LHService.validateServiceKey (newLegalHoldServiceKey newService)
      >>= maybe (throwM legalHoldServiceInvalidKey) pure
  LHService.checkLegalHoldServiceStatus fpr (newLegalHoldServiceUrl newService)
  let service = legalHoldService tid fpr newService key
  LegalHoldData.createSettings service
  pure . viewLegalHoldService $ service

getSettingsH :: UserId ::: TeamId ::: JSON -> Galley r Response
getSettingsH (zusr ::: tid ::: _) = do
  json <$> getSettings zusr tid

getSettings :: UserId -> TeamId -> Galley r Public.ViewLegalHoldService
getSettings zusr tid = do
  zusrMembership <- Data.teamMember tid zusr
  void $ permissionCheck (ViewTeamFeature Public.TeamFeatureLegalHold) zusrMembership
  isenabled <- isLegalHoldEnabledForTeam tid
  mresult <- LegalHoldData.getSettings tid
  pure $ case (isenabled, mresult) of
    (False, _) -> Public.ViewLegalHoldServiceDisabled
    (True, Nothing) -> Public.ViewLegalHoldServiceNotConfigured
    (True, Just result) -> viewLegalHoldService result

removeSettingsH ::
  Members '[BrigAccess, ExternalAccess, FederatorAccess, FireAndForget, GundeckAccess] r =>
  UserId ::: TeamId ::: JsonRequest Public.RemoveLegalHoldSettingsRequest ::: JSON ->
  Galley r Response
removeSettingsH (zusr ::: tid ::: req ::: _) = do
  removeSettingsRequest <- fromJsonBody req
  removeSettings zusr tid removeSettingsRequest
  pure noContent

removeSettings ::
  Members '[BrigAccess, ExternalAccess, FederatorAccess, FireAndForget, GundeckAccess] r =>
  UserId ->
  TeamId ->
  Public.RemoveLegalHoldSettingsRequest ->
  Galley r ()
removeSettings zusr tid (Public.RemoveLegalHoldSettingsRequest mPassword) = do
  assertNotWhitelisting
  assertLegalHoldEnabledForTeam tid
  zusrMembership <- Data.teamMember tid zusr
  -- let zothers = map (view userId) membs
  -- Log.debug $
  --   Log.field "targets" (toByteString . show $ toByteString <$> zothers)
  --     . Log.field "action" (Log.val "LegalHold.removeSettings")
  void $ permissionCheck ChangeLegalHoldTeamSettings zusrMembership
  ensureReAuthorised zusr mPassword
  removeSettings' tid
  where
    assertNotWhitelisting :: Galley r ()
    assertNotWhitelisting = do
      view (options . Opts.optSettings . Opts.setFeatureFlags . flagLegalHold) >>= \case
        FeatureLegalHoldDisabledPermanently -> pure ()
        FeatureLegalHoldDisabledByDefault -> pure ()
        FeatureLegalHoldWhitelistTeamsAndImplicitConsent -> do
          throwM legalHoldDisableUnimplemented

-- | Remove legal hold settings from team; also disabling for all users and removing LH devices
removeSettings' ::
  forall r.
  Members '[BrigAccess, ExternalAccess, FederatorAccess, FireAndForget, GundeckAccess] r =>
  TeamId ->
  Galley r ()
removeSettings' tid = do
  -- Loop through team members and run this action.
  Data.withTeamMembersWithChunks tid action
  LegalHoldData.removeSettings tid
  where
    action :: [TeamMember] -> Galley r ()
    action membs = do
      let zothers = map (view userId) membs
      let lhMembers = filter ((== UserLegalHoldEnabled) . view legalHoldStatus) membs
      Log.debug $
        Log.field "targets" (toByteString . show $ toByteString <$> zothers)
          . Log.field "action" (Log.val "LegalHold.removeSettings'")
      fireAndForgetMany (map removeLHForUser lhMembers)
    removeLHForUser :: TeamMember -> Galley r ()
    removeLHForUser member = do
      let uid = member ^. Team.userId
      Client.removeLegalHoldClientFromUser uid
      LHService.removeLegalHold tid uid
      changeLegalholdStatus tid uid (member ^. legalHoldStatus) UserLegalHoldDisabled -- (support for withdrawing consent is not planned yet.)

-- | Learn whether a user has LH enabled and fetch pre-keys.
-- Note that this is accessible to ANY authenticated user, even ones outside the team
getUserStatusH :: UserId ::: TeamId ::: UserId ::: JSON -> Galley r Response
getUserStatusH (_zusr ::: tid ::: uid ::: _) = do
  json <$> getUserStatus tid uid

getUserStatus :: TeamId -> UserId -> Galley r Public.UserLegalHoldStatusResponse
getUserStatus tid uid = do
  mTeamMember <- Data.teamMember tid uid
  teamMember <- maybe (throwM teamMemberNotFound) pure mTeamMember
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
      mLastKey <- fmap snd <$> LegalHoldData.selectPendingPrekeys uid
      lastKey <- case mLastKey of
        Nothing -> do
          Log.err . Log.msg $
            "expected to find a prekey for user: "
              <> toByteString' uid
              <> " but none was found"
          throwM internalError
        Just lstKey -> pure lstKey
      let clientId = clientIdFromPrekey . unpackLastPrekey $ lastKey
      pure (Just lastKey, Just clientId)

-- | Change 'UserLegalHoldStatus' from no consent to disabled.  FUTUREWORK:
-- @withdrawExplicitConsentH@ (lots of corner cases we'd have to implement for that to pan
-- out).
grantConsentH ::
  Members '[BrigAccess, ExternalAccess, FederatorAccess, FireAndForget, GundeckAccess] r =>
  UserId ::: TeamId ::: JSON ->
  Galley r Response
grantConsentH (zusr ::: tid ::: _) = do
  grantConsent zusr tid >>= \case
    GrantConsentSuccess -> pure $ empty & setStatus status201
    GrantConsentAlreadyGranted -> pure $ empty & setStatus status204

data GrantConsentResult
  = GrantConsentSuccess
  | GrantConsentAlreadyGranted

grantConsent ::
  Members '[BrigAccess, ExternalAccess, FederatorAccess, FireAndForget, GundeckAccess] r =>
  UserId ->
  TeamId ->
  Galley r GrantConsentResult
grantConsent zusr tid = do
  userLHStatus <- fmap (view legalHoldStatus) <$> Data.teamMember tid zusr
  case userLHStatus of
    Nothing ->
      throwM teamMemberNotFound
    Just lhs@UserLegalHoldNoConsent ->
      changeLegalholdStatus tid zusr lhs UserLegalHoldDisabled $> GrantConsentSuccess
    Just UserLegalHoldEnabled -> pure GrantConsentAlreadyGranted
    Just UserLegalHoldPending -> pure GrantConsentAlreadyGranted
    Just UserLegalHoldDisabled -> pure GrantConsentAlreadyGranted

-- | Request to provision a device on the legal hold service for a user
requestDeviceH ::
  Members '[BrigAccess, ExternalAccess, FederatorAccess, FireAndForget, GundeckAccess] r =>
  UserId ::: TeamId ::: UserId ::: JSON ->
  Galley r Response
requestDeviceH (zusr ::: tid ::: uid ::: _) = do
  requestDevice zusr tid uid <&> \case
    RequestDeviceSuccess -> empty & setStatus status201
    RequestDeviceAlreadyPending -> empty & setStatus status204

data RequestDeviceResult
  = RequestDeviceSuccess
  | RequestDeviceAlreadyPending

requestDevice ::
  forall r.
  Members '[BrigAccess, ExternalAccess, FederatorAccess, FireAndForget, GundeckAccess] r =>
  UserId ->
  TeamId ->
  UserId ->
  Galley r RequestDeviceResult
requestDevice zusr tid uid = do
  assertLegalHoldEnabledForTeam tid
  Log.debug $
    Log.field "targets" (toByteString uid)
      . Log.field "action" (Log.val "LegalHold.requestDevice")
  zusrMembership <- Data.teamMember tid zusr
  void $ permissionCheck ChangeLegalHoldUserSettings zusrMembership
  member <- maybe (throwM teamMemberNotFound) pure =<< Data.teamMember tid uid
  case member ^. legalHoldStatus of
    UserLegalHoldEnabled -> throwM userLegalHoldAlreadyEnabled
    lhs@UserLegalHoldPending -> RequestDeviceAlreadyPending <$ provisionLHDevice lhs
    lhs@UserLegalHoldDisabled -> RequestDeviceSuccess <$ provisionLHDevice lhs
    UserLegalHoldNoConsent -> throwM userLegalHoldNoConsent
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
      LegalHoldData.insertPendingPrekeys uid (unpackLastPrekey lastPrekey' : prekeys)
      changeLegalholdStatus tid uid userLHStatus UserLegalHoldPending
      Client.notifyClientsAboutLegalHoldRequest zusr uid lastPrekey'

    requestDeviceFromService :: Galley r (LastPrekey, [Prekey])
    requestDeviceFromService = do
      LegalHoldData.dropPendingPrekeys uid
      lhDevice <- LHService.requestNewDevice tid uid
      let NewLegalHoldClient prekeys lastKey = lhDevice
      return (lastKey, prekeys)

-- | Approve the adding of a Legal Hold device to the user.
--
-- We don't delete pending prekeys during this flow just in case
-- it gets interupted. There's really no reason to delete them anyways
-- since they are replaced if needed when registering new LH devices.
approveDeviceH ::
  Members '[BrigAccess, ExternalAccess, FederatorAccess, FireAndForget, GundeckAccess] r =>
  UserId ::: TeamId ::: UserId ::: ConnId ::: JsonRequest Public.ApproveLegalHoldForUserRequest ::: JSON ->
  Galley r Response
approveDeviceH (zusr ::: tid ::: uid ::: connId ::: req ::: _) = do
  approve <- fromJsonBody req
  approveDevice zusr tid uid connId approve
  pure empty

approveDevice ::
  Members '[BrigAccess, ExternalAccess, FederatorAccess, FireAndForget, GundeckAccess] r =>
  UserId ->
  TeamId ->
  UserId ->
  ConnId ->
  Public.ApproveLegalHoldForUserRequest ->
  Galley r ()
approveDevice zusr tid uid connId (Public.ApproveLegalHoldForUserRequest mPassword) = do
  assertLegalHoldEnabledForTeam tid
  Log.debug $
    Log.field "targets" (toByteString uid)
      . Log.field "action" (Log.val "LegalHold.approveDevice")
  unless (zusr == uid) (throwM accessDenied)
  assertOnTeam uid tid
  ensureReAuthorised zusr mPassword
  userLHStatus <- maybe defUserLegalHoldStatus (view legalHoldStatus) <$> Data.teamMember tid uid
  assertUserLHPending userLHStatus
  mPreKeys <- LegalHoldData.selectPendingPrekeys uid
  (prekeys, lastPrekey') <- case mPreKeys of
    Nothing -> do
      Log.info $ Log.msg @Text "No prekeys found"
      throwM noLegalHoldDeviceAllocated
    Just keys -> pure keys
  clientId <- Client.addLegalHoldClientToUser uid connId prekeys lastPrekey'
  -- Note: teamId could be passed in the getLegalHoldAuthToken request instead of lookup up again
  -- Note: both 'Client.getLegalHoldToken' and 'ensureReAuthorized' check the password
  -- Note: both 'Client.getLegalHoldToken' and this function in 'assertOnTeam' above
  --       checks that the user is part of a binding team
  -- FUTUREWORK: reduce double checks
  legalHoldAuthToken <- Client.getLegalHoldAuthToken uid mPassword
  LHService.confirmLegalHold clientId tid uid legalHoldAuthToken
  -- TODO: send event at this point (see also:
  -- https://github.com/wireapp/wire-server/pull/802#pullrequestreview-262280386)
  changeLegalholdStatus tid uid userLHStatus UserLegalHoldEnabled
  where
    assertUserLHPending :: UserLegalHoldStatus -> Galley r ()
    assertUserLHPending userLHStatus = do
      case userLHStatus of
        UserLegalHoldEnabled -> throwM userLegalHoldAlreadyEnabled
        UserLegalHoldPending -> pure ()
        UserLegalHoldDisabled -> throwM userLegalHoldNotPending
        UserLegalHoldNoConsent -> throwM userLegalHoldNotPending

disableForUserH ::
  Members '[BrigAccess, ExternalAccess, FederatorAccess, FireAndForget, GundeckAccess] r =>
  UserId ::: TeamId ::: UserId ::: JsonRequest Public.DisableLegalHoldForUserRequest ::: JSON ->
  Galley r Response
disableForUserH (zusr ::: tid ::: uid ::: req ::: _) = do
  disable <- fromJsonBody req
  disableForUser zusr tid uid disable <&> \case
    DisableLegalHoldSuccess -> empty
    DisableLegalHoldWasNotEnabled -> noContent

data DisableLegalHoldForUserResponse
  = DisableLegalHoldSuccess
  | DisableLegalHoldWasNotEnabled

disableForUser ::
  forall r.
  Members '[BrigAccess, ExternalAccess, FederatorAccess, FireAndForget, GundeckAccess] r =>
  UserId ->
  TeamId ->
  UserId ->
  Public.DisableLegalHoldForUserRequest ->
  Galley r DisableLegalHoldForUserResponse
disableForUser zusr tid uid (Public.DisableLegalHoldForUserRequest mPassword) = do
  Log.debug $
    Log.field "targets" (toByteString uid)
      . Log.field "action" (Log.val "LegalHold.disableForUser")
  zusrMembership <- Data.teamMember tid zusr
  void $ permissionCheck ChangeLegalHoldUserSettings zusrMembership

  userLHStatus <- maybe defUserLegalHoldStatus (view legalHoldStatus) <$> Data.teamMember tid uid
  if not $ userLHEnabled userLHStatus
    then pure DisableLegalHoldWasNotEnabled
    else disableLH userLHStatus $> DisableLegalHoldSuccess
  where
    disableLH :: UserLegalHoldStatus -> Galley r ()
    disableLH userLHStatus = do
      ensureReAuthorised zusr mPassword
      Client.removeLegalHoldClientFromUser uid
      LHService.removeLegalHold tid uid
      -- TODO: send event at this point (see also: related TODO in this module in
      -- 'approveDevice' and
      -- https://github.com/wireapp/wire-server/pull/802#pullrequestreview-262280386)
      changeLegalholdStatus tid uid userLHStatus UserLegalHoldDisabled

-- | Allow no-consent => consent without further changes.  If LH device is requested, enabled,
-- or disabled, make sure the affected connections are screened for policy conflict (anybody
-- with no-consent), and put those connections in the appropriate blocked state.
changeLegalholdStatus ::
  Members '[BrigAccess, ExternalAccess, FederatorAccess, FireAndForget, GundeckAccess] r =>
  TeamId ->
  UserId ->
  UserLegalHoldStatus ->
  UserLegalHoldStatus ->
  Galley r ()
changeLegalholdStatus tid uid old new = do
  case old of
    UserLegalHoldEnabled -> case new of
      UserLegalHoldEnabled -> noop
      UserLegalHoldPending -> illegal
      UserLegalHoldDisabled -> update >> removeblocks
      UserLegalHoldNoConsent -> illegal
    --
    UserLegalHoldPending -> case new of
      UserLegalHoldEnabled -> update
      UserLegalHoldPending -> noop
      UserLegalHoldDisabled -> update >> removeblocks
      UserLegalHoldNoConsent -> illegal
    --
    UserLegalHoldDisabled -> case new of
      UserLegalHoldEnabled -> illegal
      UserLegalHoldPending -> addblocks >> update
      UserLegalHoldDisabled -> {- in case the last attempt crashed -} removeblocks
      UserLegalHoldNoConsent -> {- withdrawing consent is not (yet?) implemented -} illegal
    --
    UserLegalHoldNoConsent -> case new of
      UserLegalHoldEnabled -> illegal
      UserLegalHoldPending -> illegal
      UserLegalHoldDisabled -> update
      UserLegalHoldNoConsent -> noop
  where
    update = LegalHoldData.setUserLegalHoldStatus tid uid new
    removeblocks = void $ putConnectionInternal (RemoveLHBlocksInvolving uid)
    addblocks = do
      blockNonConsentingConnections uid
      handleGroupConvPolicyConflicts uid new
    noop = pure ()
    illegal = throwM userLegalHoldIllegalOperation

-- FUTUREWORK: make this async?
blockNonConsentingConnections :: UserId -> Galley r ()
blockNonConsentingConnections uid = do
  conns <- getConnectionsUnqualified [uid] Nothing Nothing
  errmsgs <- do
    conflicts <- mconcat <$> findConflicts conns
    blockConflicts uid conflicts
  case mconcat errmsgs of
    [] -> pure ()
    msgs@(_ : _) -> do
      Log.warn $ Log.msg @String msgs
      throwM legalHoldCouldNotBlockConnections
  where
    findConflicts :: [ConnectionStatus] -> Galley r [[UserId]]
    findConflicts conns = do
      let (FutureWork @'Public.LegalholdPlusFederationNotImplemented -> _remoteUids, localUids) = (undefined, csTo <$> conns)
      -- FUTUREWORK: Handle remoteUsers here when federation is implemented
      for (chunksOf 32 localUids) $ \others -> do
        teamsOfUsers <- Data.usersTeams others
        filterM (fmap (== ConsentNotGiven) . checkConsent teamsOfUsers) others

    blockConflicts :: UserId -> [UserId] -> Galley r [String]
    blockConflicts _ [] = pure []
    blockConflicts userLegalhold othersToBlock@(_ : _) = do
      status <- putConnectionInternal (BlockForMissingLHConsent userLegalhold othersToBlock)
      pure $ ["blocking users failed: " <> show (status, othersToBlock) | status /= status200]

setTeamLegalholdWhitelisted :: TeamId -> Galley r ()
setTeamLegalholdWhitelisted tid = do
  LegalHoldData.setTeamLegalholdWhitelisted tid

setTeamLegalholdWhitelistedH :: TeamId -> Galley r Response
setTeamLegalholdWhitelistedH tid = do
  empty <$ setTeamLegalholdWhitelisted tid

unsetTeamLegalholdWhitelisted :: TeamId -> Galley r ()
unsetTeamLegalholdWhitelisted tid = do
  LegalHoldData.unsetTeamLegalholdWhitelisted tid

unsetTeamLegalholdWhitelistedH :: TeamId -> Galley r Response
unsetTeamLegalholdWhitelistedH tid = do
  () <-
    error
      "FUTUREWORK: if we remove entries from the list, that means removing an unknown \
      \number of LH devices as well, and possibly other things.  think this through \
      \before you enable the end-point."
  setStatus status204 empty <$ unsetTeamLegalholdWhitelisted tid

getTeamLegalholdWhitelistedH :: TeamId -> Galley r Response
getTeamLegalholdWhitelistedH tid = do
  lhEnabled <- isTeamLegalholdWhitelisted tid
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
  Members '[BrigAccess, ExternalAccess, FederatorAccess, FireAndForget, GundeckAccess] r =>
  UserId ->
  UserLegalHoldStatus ->
  Galley r ()
handleGroupConvPolicyConflicts uid hypotheticalLHStatus =
  void $
    iterateConversations uid (toRange (Proxy @500)) $ \convs -> do
      for_ (filter ((== RegularConv) . Data.convType) convs) $ \conv -> do
        let FutureWork _convRemoteMembers' = FutureWork @'LegalholdPlusFederationNotImplemented Data.convRemoteMembers

        membersAndLHStatus :: [(LocalMember, UserLegalHoldStatus)] <- do
          let mems = Data.convLocalMembers conv
          uidsLHStatus <- getLHStatusForUsers (lmId <$> mems)
          pure $
            zipWith
              ( \mem (mid, status) ->
                  assert (lmId mem == mid) $
                    if lmId mem == uid
                      then (mem, hypotheticalLHStatus)
                      else (mem, status)
              )
              mems
              uidsLHStatus

        lcnv <- qualifyLocal (Data.convId conv)
        if any
          ((== ConsentGiven) . consentGiven . snd)
          (filter ((== roleNameWireAdmin) . lmConvRoleName . fst) membersAndLHStatus)
          then do
            for_ (filter ((== ConsentNotGiven) . consentGiven . snd) membersAndLHStatus) $ \(memberNoConsent, _) -> do
              lusr <- qualifyLocal (lmId memberNoConsent)
              removeMemberFromLocalConv lcnv lusr Nothing (qUntagged lusr)
          else do
            for_ (filter (userLHEnabled . snd) membersAndLHStatus) $ \(legalholder, _) -> do
              lusr <- qualifyLocal (lmId legalholder)
              removeMemberFromLocalConv lcnv lusr Nothing (qUntagged lusr)
