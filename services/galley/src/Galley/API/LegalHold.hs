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
  )
where

import Brig.Types.Client.Prekey
import Brig.Types.Connection (UpdateConnectionInternal (..))
import Brig.Types.Provider
import Brig.Types.Team.LegalHold hiding (userId)
import Control.Lens (view, (^.), _3)
import Control.Monad.Catch
import Data.ByteString.Conversion (toByteString, toByteString')
import Data.Id
import Data.LegalHold (UserLegalHoldStatus (..))
import qualified Data.Map.Strict as Map
import Data.Misc
import Data.Proxy
import Data.Qualified (Qualified, partitionRemoteOrLocalIds)
import Data.Range (toRange)
import Data.String.Conversions (cs)
import Galley.API.Error
import Galley.API.Query (iterateConversations)
import Galley.API.Util
import Galley.App
import qualified Galley.Data as Data
import qualified Galley.Data.LegalHold as LegalHoldData
import qualified Galley.Data.TeamFeatures as TeamFeatures
import qualified Galley.External.LegalHoldService as LHService
import qualified Galley.Intra.Client as Client
import Galley.Intra.User (putConnectionInternal)
import qualified Galley.Options as Opts
import Galley.Types.Teams as Team
import Imports
import Network.HTTP.Types (Status, status200)
import Network.HTTP.Types.Status (status201, status204)
import Network.Wai
import Network.Wai.Predicate hiding (or, result, setStatus, _3)
import Network.Wai.Utilities as Wai
import qualified System.Logger.Class as Log
import UnliftIO.Async (pooledMapConcurrentlyN_)
import Wire.API.Conversation (ConvMembers (..), ConvType (..), Conversation (..), OtherMember (..), cnvType)
import qualified Wire.API.Team.Feature as Public
import qualified Wire.API.Team.LegalHold as Public

assertLegalHoldEnabledForTeam :: TeamId -> Galley ()
assertLegalHoldEnabledForTeam tid = unlessM (isLegalHoldEnabledForTeam tid) $ throwM legalHoldNotEnabled

isLegalHoldEnabledForTeam :: TeamId -> Galley Bool
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
    FeatureLegalHoldWhitelistTeamsAndImplicitConsent -> do
      view (options . Opts.optSettings . Opts.setLegalHoldTeamsWhitelist)
        <&> maybe
          (False {- reasonable default, even though this is impossible due to "Galley.Options.validateOpts" -})
          (tid `elem`)

createSettingsH :: UserId ::: TeamId ::: JsonRequest Public.NewLegalHoldService ::: JSON -> Galley Response
createSettingsH (zusr ::: tid ::: req ::: _) = do
  newService <- fromJsonBody req
  setStatus status201 . json <$> createSettings zusr tid newService

createSettings :: UserId -> TeamId -> Public.NewLegalHoldService -> Galley Public.ViewLegalHoldService
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

getSettingsH :: UserId ::: TeamId ::: JSON -> Galley Response
getSettingsH (zusr ::: tid ::: _) = do
  json <$> getSettings zusr tid

getSettings :: UserId -> TeamId -> Galley Public.ViewLegalHoldService
getSettings zusr tid = do
  zusrMembership <- Data.teamMember tid zusr
  void $ permissionCheck (ViewTeamFeature Public.TeamFeatureLegalHold) zusrMembership
  isenabled <- isLegalHoldEnabledForTeam tid
  mresult <- LegalHoldData.getSettings tid
  pure $ case (isenabled, mresult) of
    (False, _) -> Public.ViewLegalHoldServiceDisabled
    (True, Nothing) -> Public.ViewLegalHoldServiceNotConfigured
    (True, Just result) -> viewLegalHoldService result

removeSettingsH :: UserId ::: TeamId ::: JsonRequest Public.RemoveLegalHoldSettingsRequest ::: JSON -> Galley Response
removeSettingsH (zusr ::: tid ::: req ::: _) = do
  removeSettingsRequest <- fromJsonBody req
  removeSettings zusr tid removeSettingsRequest
  pure noContent

removeSettings :: UserId -> TeamId -> Public.RemoveLegalHoldSettingsRequest -> Galley ()
removeSettings zusr tid (Public.RemoveLegalHoldSettingsRequest mPassword) = do
  assertLegalHoldEnabledForTeam tid
  zusrMembership <- Data.teamMember tid zusr
  -- let zothers = map (view userId) membs
  -- Log.debug $
  --   Log.field "targets" (toByteString . show $ toByteString <$> zothers)
  --     . Log.field "action" (Log.val "LegalHold.removeSettings")
  void $ permissionCheck ChangeLegalHoldTeamSettings zusrMembership
  ensureReAuthorised zusr mPassword
  removeSettings' tid

-- | Remove legal hold settings from team; also disabling for all users and removing LH devices
removeSettings' ::
  TeamId ->
  Galley ()
removeSettings' tid = do
  -- Loop through team members and run this action.
  Data.withTeamMembersWithChunks tid action
  LegalHoldData.removeSettings tid
  where
    action :: [TeamMember] -> Galley ()
    action membs = do
      let zothers = map (view userId) membs
      let lhMembers = filter ((== UserLegalHoldEnabled) . view legalHoldStatus) membs
      Log.debug $
        Log.field "targets" (toByteString . show $ toByteString <$> zothers)
          . Log.field "action" (Log.val "LegalHold.removeSettings'")
      -- I picked this number by fair dice roll, feel free to change it :P
      pooledMapConcurrentlyN_ 8 removeLHForUser lhMembers
    removeLHForUser :: TeamMember -> Galley ()
    removeLHForUser member = do
      let uid = member ^. Team.userId
      Client.removeLegalHoldClientFromUser uid
      LHService.removeLegalHold tid uid
      changeLegalholdStatus tid uid UserLegalHoldDisabled -- (support for withdrawing consent is not planned yet.)

-- | Learn whether a user has LH enabled and fetch pre-keys.
-- Note that this is accessible to ANY authenticated user, even ones outside the team
getUserStatusH :: UserId ::: TeamId ::: UserId ::: JSON -> Galley Response
getUserStatusH (_zusr ::: tid ::: uid ::: _) = do
  json <$> getUserStatus tid uid

getUserStatus :: TeamId -> UserId -> Galley Public.UserLegalHoldStatusResponse
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
  userLHStatus <- fmap (view legalHoldStatus) <$> Data.teamMember tid zusr
  case userLHStatus of
    Nothing ->
      throwM teamMemberNotFound
    Just UserLegalHoldNoConsent ->
      changeLegalholdStatus tid zusr UserLegalHoldDisabled $> GrantConsentSuccess
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
  void $ permissionCheck ChangeLegalHoldUserSettings zusrMembership
  userLHStatus <- fmap (view legalHoldStatus) <$> Data.teamMember tid uid
  case userLHStatus of
    Just UserLegalHoldEnabled -> throwM userLegalHoldAlreadyEnabled
    Just UserLegalHoldPending -> RequestDeviceAlreadyPending <$ provisionLHDevice
    Just UserLegalHoldDisabled -> RequestDeviceSuccess <$ provisionLHDevice
    Just UserLegalHoldNoConsent -> throwM userLegalHoldNoConsent
    Nothing -> throwM teamMemberNotFound
  where
    -- Wire's LH service that galley is usually calling here is idempotent in device creation,
    -- ie. it returns the existing device on multiple calls to `/init`, like here:
    -- https://github.com/wireapp/legalhold/blob/e0a241162b9dbc841f12fbc57c8a1e1093c7e83a/src/main/java/com/wire/bots/hold/resource/InitiateResource.java#L42
    --
    -- This will still work if the LH service creates two new device on two consecutive calls
    -- to `/init`, but there may be race conditions, eg. when updating and enabling a pending
    -- device at (almost) the same time.
    provisionLHDevice :: Galley ()
    provisionLHDevice = do
      (lastPrekey', prekeys) <- requestDeviceFromService
      -- We don't distinguish the last key here; brig will do so when the device is added
      LegalHoldData.insertPendingPrekeys uid (unpackLastPrekey lastPrekey' : prekeys)
      changeLegalholdStatus tid uid UserLegalHoldPending
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
  UserId ::: TeamId ::: UserId ::: ConnId ::: JsonRequest Public.ApproveLegalHoldForUserRequest ::: JSON ->
  Galley Response
approveDeviceH (zusr ::: tid ::: uid ::: connId ::: req ::: _) = do
  approve <- fromJsonBody req
  approveDevice zusr tid uid connId approve
  pure empty

approveDevice :: UserId -> TeamId -> UserId -> ConnId -> Public.ApproveLegalHoldForUserRequest -> Galley ()
approveDevice zusr tid uid connId (Public.ApproveLegalHoldForUserRequest mPassword) = do
  assertLegalHoldEnabledForTeam tid
  Log.debug $
    Log.field "targets" (toByteString uid)
      . Log.field "action" (Log.val "LegalHold.approveDevice")
  unless (zusr == uid) (throwM accessDenied)
  assertOnTeam uid tid
  ensureReAuthorised zusr mPassword
  assertUserLHPending
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
  changeLegalholdStatus tid uid UserLegalHoldEnabled
  where
    assertUserLHPending :: Galley ()
    assertUserLHPending = do
      userLHStatus <- fmap (view legalHoldStatus) <$> Data.teamMember tid uid
      case userLHStatus of
        Just UserLegalHoldEnabled -> throwM userLegalHoldAlreadyEnabled
        Just UserLegalHoldPending -> pure ()
        _ -> throwM userLegalHoldNotPending

disableForUserH ::
  UserId ::: TeamId ::: UserId ::: JsonRequest Public.DisableLegalHoldForUserRequest ::: JSON ->
  Galley Response
disableForUserH (zusr ::: tid ::: uid ::: req ::: _) = do
  disable <- fromJsonBody req
  disableForUser zusr tid uid disable <&> \case
    DisableLegalHoldSuccess -> empty
    DisableLegalHoldWasNotEnabled -> noContent

data DisableLegalHoldForUserResponse
  = DisableLegalHoldSuccess
  | DisableLegalHoldWasNotEnabled

disableForUser :: UserId -> TeamId -> UserId -> Public.DisableLegalHoldForUserRequest -> Galley DisableLegalHoldForUserResponse
disableForUser zusr tid uid (Public.DisableLegalHoldForUserRequest mPassword) = do
  Log.debug $
    Log.field "targets" (toByteString uid)
      . Log.field "action" (Log.val "LegalHold.disableForUser")
  zusrMembership <- Data.teamMember tid zusr
  void $ permissionCheck ChangeLegalHoldUserSettings zusrMembership
  uidMembership <- Data.teamMember tid uid
  if not $ userLHEnabled uidMembership
    then pure DisableLegalHoldWasNotEnabled
    else disableLH $> DisableLegalHoldSuccess
  where
    -- If not enabled nor pending, then it's disabled
    userLHEnabled :: Maybe TeamMember -> Bool
    userLHEnabled target = do
      case fmap (view legalHoldStatus) target of
        Just UserLegalHoldEnabled -> True
        Just UserLegalHoldPending -> True
        Just UserLegalHoldDisabled -> False
        Just UserLegalHoldNoConsent -> False
        Nothing -> {- user not found -} False

    disableLH :: Galley ()
    disableLH = do
      ensureReAuthorised zusr mPassword
      Client.removeLegalHoldClientFromUser uid
      LHService.removeLegalHold tid uid
      -- TODO: send event at this point (see also: related TODO in this module in
      -- 'approveDevice' and
      -- https://github.com/wireapp/wire-server/pull/802#pullrequestreview-262280386)
      changeLegalholdStatus tid uid UserLegalHoldDisabled

changeLegalholdStatus :: TeamId -> UserId -> UserLegalHoldStatus -> Galley ()
changeLegalholdStatus tid uid lhStatus = do
  LegalHoldData.setUserLegalHoldStatus tid uid lhStatus
  when (hasLH lhStatus) $
    iterateConversations uid (toRange (Proxy @500)) $ \convs ->
      block1on1s uid (filter ((== One2OneConv) . cnvType) convs)
  where
    hasLH :: UserLegalHoldStatus -> Bool
    hasLH = \case
      UserLegalHoldDisabled -> False
      UserLegalHoldPending -> True
      UserLegalHoldEnabled -> True
      UserLegalHoldNoConsent -> False

    hasGrantedConsent :: UserLegalHoldStatus -> Bool
    hasGrantedConsent = \case
      UserLegalHoldDisabled -> True
      UserLegalHoldPending -> True
      UserLegalHoldEnabled -> True
      UserLegalHoldNoConsent -> False

    -- TODO: make this async
    block1on1s :: UserId -> [Conversation] -> Galley ()
    block1on1s userLegalhold convs = do
      Log.info $ Log.msg @Text "-------------------------------------------------------------- block1on1s" -- TODO: remove this line
      let otherUids :: [Qualified UserId] =
            concatMap (fmap omQualifiedId . cmOthers . cnvMembers) convs
      ownDomain <- viewFederationDomain
      let (_remoteUsers, localUids) = partitionRemoteOrLocalIds ownDomain otherUids
      -- FUTUREWORK: Handle remoteUsers here when federation is implemented
      results <- for (chunksOf 32 localUids) $ \uids -> do
        teamsOfUsers <- Data.usersTeams uids
        results :: [Maybe (UserId, UserId, Status)] <- for uids $ \user ->
          case Map.lookup user teamsOfUsers of
            Nothing ->
              Just <$> blockConnection user userLegalhold
            Just team -> do
              mMember <- Data.teamMember team user
              if maybe False (hasGrantedConsent . view legalHoldStatus) mMember
                then pure Nothing
                else Just <$> blockConnection user userLegalhold
        pure results

      case filter ((/= status200) . view _3) . catMaybes . concat $ results of
        problems@(_ : _) ->
          throwM $ internalErrorWithDescription (cs $ "block1on1s " <> show problems)
        [] -> pure ()

    chunksOf :: Int -> [any] -> [[any]]
    chunksOf _maxSize [] = []
    chunksOf maxSize uids = case splitAt maxSize uids of (h, t) -> h : chunksOf maxSize t

    blockConnection :: UserId -> UserId -> Galley (UserId, UserId, Status)
    blockConnection userA userB = do
      (userA,userB,) <$> putConnectionInternal userA userB BlockForMissingLegalholdConsent
