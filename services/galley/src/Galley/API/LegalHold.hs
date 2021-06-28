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
    guardQualifiedLegalholdPolicyConflicts,
    guardLegalholdPolicyConflicts,
  )
where

import Brig.Types.Client.Prekey
import Brig.Types.Connection (UpdateConnectionsInternal (..))
import Brig.Types.Intra (ConnectionStatus (..), accountUser)
import Brig.Types.Provider
import Brig.Types.Team.LegalHold hiding (userId)
import Control.Lens (view, (^.))
import Control.Monad.Catch
import Data.ByteString.Conversion (toByteString, toByteString')
import Data.Id
import Data.LegalHold (UserLegalHoldStatus (..), defUserLegalHoldStatus)
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as Map
import Data.Misc
import qualified Data.Set as Set
import Galley.API.Error
import Galley.API.Util
import Galley.App
import qualified Galley.Data as Data
import Galley.Data.LegalHold (isTeamLegalholdWhitelisted)
import qualified Galley.Data.LegalHold as LegalHoldData
import qualified Galley.Data.TeamFeatures as TeamFeatures
import qualified Galley.External.LegalHoldService as LHService
import qualified Galley.Intra.Client as Client
import qualified Galley.Intra.Client as Intra
import Galley.Intra.User (getConnections, getUser, putConnectionInternal)
import Galley.Options
import qualified Galley.Types.Teams as Team
import Imports
import Network.HTTP.Types (status200, status404)
import Network.HTTP.Types.Status (status201, status204)
import Network.Wai
import Network.Wai.Predicate hiding (or, result, setStatus, _3)
import Network.Wai.Utilities as Wai
import qualified System.Logger.Class as Log
import UnliftIO.Async (pooledMapConcurrentlyN_)
import Wire.API.Team.Feature
import Wire.API.Team.LegalHold
import Wire.API.User
import Wire.API.User.Client

assertLegalHoldEnabledForTeam :: TeamId -> Galley ()
assertLegalHoldEnabledForTeam tid = unlessM (isLegalHoldEnabledForTeam tid) $ throwM legalHoldNotEnabled

isLegalHoldEnabledForTeam :: TeamId -> Galley Bool
isLegalHoldEnabledForTeam tid = do
  view (options . optSettings . setFeatureFlags . Team.flagLegalHold) >>= \case
    Team.FeatureLegalHoldDisabledPermanently -> do
      pure False
    Team.FeatureLegalHoldDisabledByDefault -> do
      statusValue <- tfwoStatus <$$> TeamFeatures.getFeatureStatusNoConfig @'TeamFeatureLegalHold tid
      return $ case statusValue of
        Just TeamFeatureEnabled -> True
        Just TeamFeatureDisabled -> False
        Nothing -> False
    Team.FeatureLegalHoldWhitelistTeamsAndImplicitConsent ->
      isTeamLegalholdWhitelisted tid

createSettingsH :: UserId ::: TeamId ::: JsonRequest NewLegalHoldService ::: JSON -> Galley Response
createSettingsH (zusr ::: tid ::: req ::: _) = do
  newService <- fromJsonBody req
  setStatus status201 . json <$> createSettings zusr tid newService

createSettings :: UserId -> TeamId -> NewLegalHoldService -> Galley ViewLegalHoldService
createSettings zusr tid newService = do
  assertLegalHoldEnabledForTeam tid
  zusrMembership <- Data.teamMember tid zusr
  -- let zothers = map (view userId) membs
  -- Log.debug $
  --   Log.field "targets" (toByteString . show $ toByteString <$> zothers)
  --     . Log.field "action" (Log.val "LegalHold.createSettings")
  void $ permissionCheck Team.ChangeLegalHoldTeamSettings zusrMembership
  (key :: ServiceKey, fpr :: Fingerprint Rsa) <-
    LHService.validateServiceKey (newLegalHoldServiceKey newService)
      >>= maybe (throwM legalHoldServiceInvalidKey) pure
  LHService.checkLegalHoldServiceStatus fpr (newLegalHoldServiceUrl newService)
  let service = legalHoldService tid fpr newService key
  LegalHoldData.createSettings service
  pure . viewLegalHoldService $ service

getSettingsH :: UserId ::: TeamId ::: JSON -> Galley Response
getSettingsH (zusr ::: tid ::: _) = do
  json <$> getSettings zusr tid

getSettings :: UserId -> TeamId -> Galley ViewLegalHoldService
getSettings zusr tid = do
  zusrMembership <- Data.teamMember tid zusr
  void $ permissionCheck (Team.ViewTeamFeature TeamFeatureLegalHold) zusrMembership
  isenabled <- isLegalHoldEnabledForTeam tid
  mresult <- LegalHoldData.getSettings tid
  pure $ case (isenabled, mresult) of
    (False, _) -> ViewLegalHoldServiceDisabled
    (True, Nothing) -> ViewLegalHoldServiceNotConfigured
    (True, Just result) -> viewLegalHoldService result

removeSettingsH :: UserId ::: TeamId ::: JsonRequest RemoveLegalHoldSettingsRequest ::: JSON -> Galley Response
removeSettingsH (zusr ::: tid ::: req ::: _) = do
  removeSettingsRequest <- fromJsonBody req
  removeSettings zusr tid removeSettingsRequest
  pure noContent

removeSettings :: UserId -> TeamId -> RemoveLegalHoldSettingsRequest -> Galley ()
removeSettings zusr tid (RemoveLegalHoldSettingsRequest mPassword) = do
  assertNotWhitelisting
  assertLegalHoldEnabledForTeam tid
  zusrMembership <- Data.teamMember tid zusr
  -- let zothers = map (view userId) membs
  -- Log.debug $
  --   Log.field "targets" (toByteString . show $ toByteString <$> zothers)
  --     . Log.field "action" (Log.val "LegalHold.removeSettings")
  void $ permissionCheck Team.ChangeLegalHoldTeamSettings zusrMembership
  ensureReAuthorised zusr mPassword
  removeSettings' tid
  where
    assertNotWhitelisting :: Galley ()
    assertNotWhitelisting = do
      view (options . optSettings . setFeatureFlags . Team.flagLegalHold) >>= \case
        Team.FeatureLegalHoldDisabledPermanently -> pure ()
        Team.FeatureLegalHoldDisabledByDefault -> pure ()
        Team.FeatureLegalHoldWhitelistTeamsAndImplicitConsent -> do
          throwM legalHoldDisableUnimplemented

-- | Remove legal hold settings from team; also disabling for all users and removing LH devices
removeSettings' ::
  TeamId ->
  Galley ()
removeSettings' tid = do
  -- Loop through team members and run this action.
  Data.withTeamMembersWithChunks tid action
  LegalHoldData.removeSettings tid
  where
    action :: [Team.TeamMember] -> Galley ()
    action membs = do
      let zothers = map (view Team.userId) membs
      let lhMembers = filter ((== UserLegalHoldEnabled) . view Team.legalHoldStatus) membs
      Log.debug $
        Log.field "targets" (toByteString . show $ toByteString <$> zothers)
          . Log.field "action" (Log.val "LegalHold.removeSettings'")
      -- I picked this number by fair dice roll, feel free to change it :P
      pooledMapConcurrentlyN_ 8 removeLHForUser lhMembers
    removeLHForUser :: Team.TeamMember -> Galley ()
    removeLHForUser member = do
      let uid = member ^. Team.userId
      Client.removeLegalHoldClientFromUser uid
      LHService.removeLegalHold tid uid
      changeLegalholdStatus tid uid (member ^. Team.legalHoldStatus) UserLegalHoldDisabled -- (support for withdrawing consent is not planned yet.)

-- | Learn whether a user has LH enabled and fetch pre-keys.
-- Note that this is accessible to ANY authenticated user, even ones outside the team
getUserStatusH :: UserId ::: TeamId ::: UserId ::: JSON -> Galley Response
getUserStatusH (_zusr ::: tid ::: uid ::: _) = do
  json <$> getUserStatus tid uid

getUserStatus :: TeamId -> UserId -> Galley UserLegalHoldStatusResponse
getUserStatus tid uid = do
  mTeamMember <- Data.teamMember tid uid
  teamMember <- maybe (throwM teamMemberNotFound) pure mTeamMember
  let status = view Team.legalHoldStatus teamMember
  (mlk, lcid) <- case status of
    UserLegalHoldNoConsent -> pure (Nothing, Nothing)
    UserLegalHoldDisabled -> pure (Nothing, Nothing)
    UserLegalHoldPending -> makeResponseDetails
    UserLegalHoldEnabled -> makeResponseDetails
  pure $ UserLegalHoldStatusResponse status mlk lcid
  where
    makeResponseDetails :: Galley (Maybe LastPrekey, Maybe ClientId)
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
grantConsentH :: UserId ::: TeamId ::: JSON -> Galley Response
grantConsentH (zusr ::: tid ::: _) = do
  grantConsent zusr tid >>= \case
    GrantConsentSuccess -> pure $ empty & setStatus status201
    GrantConsentAlreadyGranted -> pure $ empty & setStatus status204

data GrantConsentResult
  = GrantConsentSuccess
  | GrantConsentAlreadyGranted

grantConsent :: UserId -> TeamId -> Galley GrantConsentResult
grantConsent zusr tid = do
  userLHStatus <- fmap (view Team.legalHoldStatus) <$> Data.teamMember tid zusr
  case userLHStatus of
    Nothing ->
      throwM teamMemberNotFound
    Just lhs@UserLegalHoldNoConsent ->
      changeLegalholdStatus tid zusr lhs UserLegalHoldDisabled $> GrantConsentSuccess
    Just UserLegalHoldEnabled -> pure GrantConsentAlreadyGranted
    Just UserLegalHoldPending -> pure GrantConsentAlreadyGranted
    Just UserLegalHoldDisabled -> pure GrantConsentAlreadyGranted

-- | Request to provision a device on the legal hold service for a user
requestDeviceH :: UserId ::: TeamId ::: UserId ::: JSON -> Galley Response
requestDeviceH (zusr ::: tid ::: uid ::: _) = do
  requestDevice zusr tid uid <&> \case
    RequestDeviceSuccess -> empty & setStatus status201
    RequestDeviceAlreadyPending -> empty & setStatus status204

data RequestDeviceResult
  = RequestDeviceSuccess
  | RequestDeviceAlreadyPending

requestDevice :: UserId -> TeamId -> UserId -> Galley RequestDeviceResult
requestDevice zusr tid uid = do
  assertLegalHoldEnabledForTeam tid
  Log.debug $
    Log.field "targets" (toByteString uid)
      . Log.field "action" (Log.val "LegalHold.requestDevice")
  zusrMembership <- Data.teamMember tid zusr
  void $ permissionCheck Team.ChangeLegalHoldUserSettings zusrMembership
  member <- maybe (throwM teamMemberNotFound) pure =<< Data.teamMember tid uid
  case member ^. Team.legalHoldStatus of
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
    provisionLHDevice :: UserLegalHoldStatus -> Galley ()
    provisionLHDevice userLHStatus = do
      (lastPrekey', prekeys) <- requestDeviceFromService
      -- We don't distinguish the last key here; brig will do so when the device is added
      LegalHoldData.insertPendingPrekeys uid (unpackLastPrekey lastPrekey' : prekeys)
      changeLegalholdStatus tid uid userLHStatus UserLegalHoldPending
      Client.notifyClientsAboutLegalHoldRequest zusr uid lastPrekey'

    requestDeviceFromService :: Galley (LastPrekey, [Prekey])
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
  UserId ::: TeamId ::: UserId ::: ConnId ::: JsonRequest ApproveLegalHoldForUserRequest ::: JSON ->
  Galley Response
approveDeviceH (zusr ::: tid ::: uid ::: connId ::: req ::: _) = do
  approve <- fromJsonBody req
  approveDevice zusr tid uid connId approve
  pure empty

approveDevice :: UserId -> TeamId -> UserId -> ConnId -> ApproveLegalHoldForUserRequest -> Galley ()
approveDevice zusr tid uid connId (ApproveLegalHoldForUserRequest mPassword) = do
  assertLegalHoldEnabledForTeam tid
  Log.debug $
    Log.field "targets" (toByteString uid)
      . Log.field "action" (Log.val "LegalHold.approveDevice")
  unless (zusr == uid) (throwM accessDenied)
  assertOnTeam uid tid
  ensureReAuthorised zusr mPassword
  userLHStatus <- maybe defUserLegalHoldStatus (view Team.legalHoldStatus) <$> Data.teamMember tid uid
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
    assertUserLHPending :: UserLegalHoldStatus -> Galley ()
    assertUserLHPending userLHStatus = do
      case userLHStatus of
        UserLegalHoldEnabled -> throwM userLegalHoldAlreadyEnabled
        UserLegalHoldPending -> pure ()
        UserLegalHoldDisabled -> throwM userLegalHoldNotPending
        UserLegalHoldNoConsent -> throwM userLegalHoldNotPending

disableForUserH ::
  UserId ::: TeamId ::: UserId ::: JsonRequest DisableLegalHoldForUserRequest ::: JSON ->
  Galley Response
disableForUserH (zusr ::: tid ::: uid ::: req ::: _) = do
  disable <- fromJsonBody req
  disableForUser zusr tid uid disable <&> \case
    DisableLegalHoldSuccess -> empty
    DisableLegalHoldWasNotEnabled -> noContent

data DisableLegalHoldForUserResponse
  = DisableLegalHoldSuccess
  | DisableLegalHoldWasNotEnabled

disableForUser :: UserId -> TeamId -> UserId -> DisableLegalHoldForUserRequest -> Galley DisableLegalHoldForUserResponse
disableForUser zusr tid uid (DisableLegalHoldForUserRequest mPassword) = do
  Log.debug $
    Log.field "targets" (toByteString uid)
      . Log.field "action" (Log.val "LegalHold.disableForUser")
  zusrMembership <- Data.teamMember tid zusr
  void $ permissionCheck Team.ChangeLegalHoldUserSettings zusrMembership

  userLHStatus <- maybe defUserLegalHoldStatus (view Team.legalHoldStatus) <$> Data.teamMember tid uid
  if not $ userLHEnabled userLHStatus
    then pure DisableLegalHoldWasNotEnabled
    else disableLH userLHStatus $> DisableLegalHoldSuccess
  where
    disableLH :: UserLegalHoldStatus -> Galley ()
    disableLH userLHStatus = do
      ensureReAuthorised zusr mPassword
      Client.removeLegalHoldClientFromUser uid
      LHService.removeLegalHold tid uid
      -- TODO: send event at this point (see also: related TODO in this module in
      -- 'approveDevice' and
      -- https://github.com/wireapp/wire-server/pull/802#pullrequestreview-262280386)
      changeLegalholdStatus tid uid userLHStatus UserLegalHoldDisabled

-- | If not enabled nor pending, then it's disabled
userLHEnabled :: UserLegalHoldStatus -> Bool
userLHEnabled = \case
  UserLegalHoldEnabled -> True
  UserLegalHoldPending -> True
  UserLegalHoldDisabled -> False
  UserLegalHoldNoConsent -> False

-- | Allow no-consent => consent without further changes.  If LH device is requested, enabled,
-- or disabled, make sure the affected connections are screened for policy conflict (anybody
-- with no-consent), and put those connections in the appropriate blocked state.
changeLegalholdStatus :: TeamId -> UserId -> UserLegalHoldStatus -> UserLegalHoldStatus -> Galley ()
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
    addblocks = blockConnectionsFrom1on1s uid
    noop = pure ()
    illegal = throwM userLegalHoldIllegalOperation

-- FUTUREWORK: make this async?
blockConnectionsFrom1on1s :: UserId -> Galley ()
blockConnectionsFrom1on1s uid = do
  conns <- getConnections [uid] Nothing Nothing
  errmsgs <- do
    conflicts <- mconcat <$> findConflicts conns
    blockConflicts uid conflicts
  case mconcat errmsgs of
    [] -> pure ()
    msgs@(_ : _) -> do
      Log.warn $ Log.msg @String msgs
      throwM legalHoldCouldNotBlockConnections
  where
    findConflicts :: [ConnectionStatus] -> Galley [[UserId]]
    findConflicts conns = do
      let (FutureWork @'LegalholdPlusFederationNotImplemented -> _remoteUids, localUids) = (undefined, csTo <$> conns)
      -- FUTUREWORK: Handle remoteUsers here when federation is implemented
      for (chunksOf 32 localUids) $ \others -> do
        teamsOfUsers <- Data.usersTeams others
        filterM (shouldBlock teamsOfUsers) others

    blockConflicts :: UserId -> [UserId] -> Galley [String]
    blockConflicts _ [] = pure []
    blockConflicts userLegalhold othersToBlock@(_ : _) = do
      status <- putConnectionInternal (BlockForMissingLHConsent userLegalhold othersToBlock)
      pure $ ["blocking users failed: " <> show (status, othersToBlock) | status /= status200]

    shouldBlock :: Map UserId TeamId -> UserId -> Galley Bool
    shouldBlock teamsOfUsers other =
      (== UserLegalHoldNoConsent)
        <$> case Map.lookup other teamsOfUsers of
          Nothing -> pure defUserLegalHoldStatus
          Just team -> do
            mMember <- Data.teamMember team other
            pure $ maybe defUserLegalHoldStatus (view Team.legalHoldStatus) mMember

setTeamLegalholdWhitelisted :: TeamId -> Galley ()
setTeamLegalholdWhitelisted tid = do
  LegalHoldData.setTeamLegalholdWhitelisted tid

setTeamLegalholdWhitelistedH :: TeamId -> Galley Response
setTeamLegalholdWhitelistedH tid = do
  empty <$ setTeamLegalholdWhitelisted tid

unsetTeamLegalholdWhitelisted :: TeamId -> Galley ()
unsetTeamLegalholdWhitelisted tid = do
  LegalHoldData.unsetTeamLegalholdWhitelisted tid

unsetTeamLegalholdWhitelistedH :: TeamId -> Galley Response
unsetTeamLegalholdWhitelistedH tid = do
  () <-
    error
      "FUTUREWORK: if we remove entries from the list, that means removing an unknown \
      \number of LH devices as well, and possibly other things.  think this through \
      \before you enable the end-point."
  setStatus status204 empty <$ unsetTeamLegalholdWhitelisted tid

getTeamLegalholdWhitelistedH :: TeamId -> Galley Response
getTeamLegalholdWhitelistedH tid = do
  lhEnabled <- isTeamLegalholdWhitelisted tid
  pure $
    if lhEnabled
      then setStatus status200 empty
      else setStatus status404 empty

guardQualifiedLegalholdPolicyConflicts :: LegalholdProtectee -> QualifiedUserClients -> Galley ()
guardQualifiedLegalholdPolicyConflicts protectee qclients = do
  localDomain <- viewFederationDomain
  guardLegalholdPolicyConflicts protectee
    . UserClients
    . Map.findWithDefault mempty localDomain
    . qualifiedUserClients
    $ qclients

-- | If user has legalhold status `no_consent` or has client devices that have no legalhold
-- capability, and some of the clients she is about to get connected are LH devices, respond
-- with 412 and do not process notification.
--
-- This is a fallback safeguard that shouldn't get triggered if backend and clients work as
-- intended.
guardLegalholdPolicyConflicts :: LegalholdProtectee -> UserClients -> Galley ()
guardLegalholdPolicyConflicts LegalholdPlusFederationNotImplemented _otherClients = pure ()
guardLegalholdPolicyConflicts UnprotectedBot _otherClients = pure ()
guardLegalholdPolicyConflicts (ProtectedUser self) otherClients = do
  view (options . optSettings . setFeatureFlags . Team.flagLegalHold) >>= \case
    Team.FeatureLegalHoldDisabledPermanently -> case FutureWork @'LegalholdPlusFederationNotImplemented () of
      FutureWork () -> pure () -- FUTUREWORK: if federation is enabled, we still need to run the guard!
    Team.FeatureLegalHoldDisabledByDefault -> guardLegalholdPolicyConflictsUid self otherClients
    Team.FeatureLegalHoldWhitelistTeamsAndImplicitConsent -> guardLegalholdPolicyConflictsUid self otherClients

guardLegalholdPolicyConflictsUid :: UserId -> UserClients -> Galley ()
guardLegalholdPolicyConflictsUid self otherClients = do
  let otherCids :: [ClientId]
      otherCids = Set.toList . Set.unions . Map.elems . userClients $ otherClients

      otherUids :: [UserId]
      otherUids = nub $ Map.keys . userClients $ otherClients

  when (nub otherUids /= [self {- if all other clients belong to us, there can be no conflict -}]) $ do
    allClients :: UserClientsFull <- Intra.lookupClientsFull (nub $ self : otherUids)

    let selfClients :: [Client] =
          allClients
            & userClientsFull
            & Map.lookup self
            & fromMaybe Set.empty
            & Set.toList

        otherClientHasLH :: Bool
        otherClientHasLH =
          let clients =
                allClients
                  & userClientsFull
                  & Map.delete self
                  & Map.elems
                  & Set.unions
                  & Set.toList
                  & filter ((`elem` otherCids) . clientId)
           in LegalHoldClientType `elem` (clientType <$> clients)

        checkSelfHasLHClients :: Bool
        checkSelfHasLHClients =
          any ((== LegalHoldClientType) . clientType) selfClients

        checkSelfHasOldClients :: Bool
        checkSelfHasOldClients =
          any isOld selfClients
          where
            isOld :: Client -> Bool
            isOld =
              (ClientSupportsLegalholdImplicitConsent `Set.notMember`)
                . fromClientCapabilityList
                . clientCapabilities

        checkConsentMissing :: Galley Bool
        checkConsentMissing = do
          -- (we could also get the profile from brig.  would make the code slightly more
          -- concise, but not really help with the rpc back-and-forth, so, like, why?)
          mbUser <- accountUser <$$> getUser self
          mbTeamMember <- join <$> for (mbUser >>= userTeam) (`Data.teamMember` self)
          let lhStatus = maybe defUserLegalHoldStatus (view Team.legalHoldStatus) mbTeamMember
          pure (lhStatus == UserLegalHoldNoConsent)

    Log.debug $
      Log.field "self" (toByteString' self)
        Log.~~ Log.field "otherClients" (toByteString' $ show otherClients)
        Log.~~ Log.field "otherClientHasLH" (toByteString' otherClientHasLH)
        Log.~~ Log.field "checkSelfHasOldClients" (toByteString' checkSelfHasOldClients)
        Log.~~ Log.field "checkSelfHasLHClients" (toByteString' checkSelfHasLHClients)
        Log.~~ Log.msg ("guardLegalholdPolicyConflicts[1]" :: Text)

    -- (I've tried to order the following checks for minimum IO; did it work?  ~~fisx)
    when otherClientHasLH $ do
      when checkSelfHasOldClients $ do
        Log.debug $ Log.msg ("guardLegalholdPolicyConflicts[2]: old clients" :: Text)
        throwM missingLegalholdConsent

      unless checkSelfHasLHClients {- carrying a LH device implies having granted LH consent -} $ do
        whenM checkConsentMissing $ do
          -- We assume this is impossible, since conversations are automatically
          -- blocked if LH consent is missing of any participant.
          -- We add this check here as an extra failsafe.
          Log.debug $ Log.msg ("guardLegalholdPolicyConflicts[3]: consent missing" :: Text)
          throwM missingLegalholdConsent
