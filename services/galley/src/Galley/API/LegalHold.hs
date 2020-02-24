module Galley.API.LegalHold
  ( createSettingsH,
    getSettingsH,
    removeSettingsH,
    removeSettings',
    getUserStatusH,
    requestDeviceH,
    approveDeviceH,
    disableForUserH,
  )
where

import Brig.Types.Client.Prekey
import Brig.Types.Provider
import Brig.Types.Team.LegalHold hiding (userId)
import Control.Lens ((^.), view)
import Control.Monad.Catch
import Data.ByteString.Conversion (toByteString, toByteString')
import Data.Id
import Data.LegalHold (UserLegalHoldStatus (..))
import Data.Misc
import Galley.API.Error
import Galley.API.Util
import Galley.App
import qualified Galley.Data as Data
import qualified Galley.Data.LegalHold as LegalHoldData
import qualified Galley.External.LegalHoldService as LHService
import qualified Galley.Intra.Client as Client
import Galley.Types.Teams as Team
import Imports
import Network.HTTP.Types.Status (status201, status204)
import Network.Wai
import Network.Wai.Predicate hiding (or, result, setStatus)
import Network.Wai.Utilities as Wai
import qualified System.Logger.Class as Log
import UnliftIO.Async (pooledMapConcurrentlyN_)

assertLegalHoldEnabled :: TeamId -> Galley ()
assertLegalHoldEnabled tid = unlessM (isLegalHoldEnabled tid) $ throwM legalHoldNotEnabled

isLegalHoldEnabled :: TeamId -> Galley Bool
isLegalHoldEnabled tid = do
  lhConfig <- LegalHoldData.getLegalHoldTeamConfig tid
  return $ case legalHoldTeamConfigStatus <$> lhConfig of
    Just LegalHoldEnabled -> True
    Just LegalHoldDisabled -> False
    Nothing -> False

createSettingsH :: UserId ::: TeamId ::: JsonRequest NewLegalHoldService ::: JSON -> Galley Response
createSettingsH (zusr ::: tid ::: req ::: _) = do
  newService :: NewLegalHoldService <- fromJsonBody req
  setStatus status201 . json <$> createSettings zusr tid newService

createSettings :: UserId -> TeamId -> NewLegalHoldService -> Galley ViewLegalHoldService
createSettings zusr tid newService = do
  assertLegalHoldEnabled tid
  membs <- Data.teamMembers tid
  let zothers = map (view userId) membs
  Log.debug $
    Log.field "targets" (toByteString . show $ toByteString <$> zothers)
      . Log.field "action" (Log.val "LegalHold.createSettings")
  void $ permissionCheck zusr ChangeLegalHoldTeamSettings membs
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
  membs <- Data.teamMembers tid
  void $ permissionCheck zusr ViewLegalHoldTeamSettings membs
  isenabled <- isLegalHoldEnabled tid
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
  assertLegalHoldEnabled tid
  membs <- Data.teamMembers tid
  let zothers = map (view userId) membs
  Log.debug $
    Log.field "targets" (toByteString . show $ toByteString <$> zothers)
      . Log.field "action" (Log.val "LegalHold.removeSettings")
  void $ permissionCheck zusr ChangeLegalHoldTeamSettings membs
  ensureReAuthorised zusr mPassword
  removeSettings' tid (Just membs)

-- | Remove legal hold settings from team; also disabling for all users and removing LH devices
removeSettings' ::
  TeamId ->
  -- | If you've already got the team members you can pass them in otherwise they'll be looked up.
  Maybe [TeamMember] ->
  Galley ()
removeSettings' tid mMembers = do
  membs <- maybe (Data.teamMembers tid) pure mMembers
  let zothers = map (view userId) membs
  Log.debug $
    Log.field "targets" (toByteString . show $ toByteString <$> zothers)
      . Log.field "action" (Log.val "LegalHold.removeSettings'")
  let lhMembers = filter ((== UserLegalHoldEnabled) . view legalHoldStatus) membs
  -- I picked this number by fair dice roll, feel free to change it :P
  pooledMapConcurrentlyN_ 6 removeLHForUser lhMembers
  LegalHoldData.removeSettings tid
  where
    removeLHForUser :: TeamMember -> Galley ()
    removeLHForUser member = do
      let uid = member ^. Team.userId
      Client.removeLegalHoldClientFromUser uid
      LHService.removeLegalHold tid uid
      LegalHoldData.setUserLegalHoldStatus tid uid UserLegalHoldDisabled

-- | Learn whether a user has LH enabled and fetch pre-keys.
-- Note that this is accessible to ANY authenticated user, even ones outside the team
getUserStatusH :: UserId ::: TeamId ::: UserId ::: JSON -> Galley Response
getUserStatusH (_zusr ::: tid ::: uid ::: _) = do
  json <$> getUserStatus tid uid

getUserStatus :: TeamId -> UserId -> Galley UserLegalHoldStatusResponse
getUserStatus tid uid = do
  mTeamMember <- Data.teamMember tid uid
  teamMember <- maybe (throwM teamMemberNotFound) pure mTeamMember
  statusResponse <- case view legalHoldStatus teamMember of
    UserLegalHoldDisabled ->
      pure $ UserLegalHoldStatusResponse UserLegalHoldDisabled Nothing Nothing
    status@UserLegalHoldPending -> makeResponse status
    status@UserLegalHoldEnabled -> makeResponse status
  pure $ statusResponse
  where
    makeResponse status = do
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
      pure $ UserLegalHoldStatusResponse status (Just lastKey) (Just clientId)

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
  assertLegalHoldEnabled tid
  Log.debug $
    Log.field "targets" (toByteString uid)
      . Log.field "action" (Log.val "LegalHold.requestDevice")
  membs <- Data.teamMembers tid
  void $ permissionCheck zusr ChangeLegalHoldUserSettings membs
  userLHStatus <- fmap (view legalHoldStatus) <$> Data.teamMember tid uid
  case userLHStatus of
    Just UserLegalHoldEnabled -> throwM userLegalHoldAlreadyEnabled
    Just UserLegalHoldPending -> RequestDeviceAlreadyPending <$ provisionLHDevice
    Just UserLegalHoldDisabled -> RequestDeviceSuccess <$ provisionLHDevice
    Nothing -> throwM teamMemberNotFound
  where
    provisionLHDevice :: Galley ()
    provisionLHDevice = do
      (lastPrekey', prekeys) <- requestDeviceFromService
      -- We don't distinguish the last key here; brig will do so when the device is added
      LegalHoldData.insertPendingPrekeys uid (unpackLastPrekey lastPrekey' : prekeys)
      LegalHoldData.setUserLegalHoldStatus tid uid UserLegalHoldPending
      Client.notifyClientsAboutLegalHoldRequest zusr uid lastPrekey'
    requestDeviceFromService :: Galley (LastPrekey, [Prekey])
    requestDeviceFromService = do
      LegalHoldData.dropPendingPrekeys uid
      lhDevice <- LHService.requestNewDevice tid uid
      let NewLegalHoldClient prekeys lastKey = lhDevice
      return (lastKey, prekeys)

-- | Approve the adding of a Legal Hold device to the user
-- we don't delete pending prekeys during this flow just in case
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
  assertLegalHoldEnabled tid
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
  LegalHoldData.setUserLegalHoldStatus tid uid UserLegalHoldEnabled
  where
    assertUserLHPending :: Galley ()
    assertUserLHPending = do
      userLHStatus <- fmap (view legalHoldStatus) <$> Data.teamMember tid uid
      case userLHStatus of
        Just UserLegalHoldEnabled -> throwM userLegalHoldAlreadyEnabled
        Just UserLegalHoldPending -> pure ()
        _ -> throwM userLegalHoldNotPending

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
  membs <- Data.teamMembers tid
  void $ permissionCheck zusr ChangeLegalHoldUserSettings membs
  if userLHNotDisabled membs
    then disableLH >> pure DisableLegalHoldSuccess
    else pure DisableLegalHoldWasNotEnabled
  where
    -- If not enabled nor pending, then it's disabled
    userLHNotDisabled mems = do
      let target = findTeamMember uid mems
      case fmap (view legalHoldStatus) target of
        Just UserLegalHoldEnabled -> True
        Just UserLegalHoldPending -> True
        Just UserLegalHoldDisabled -> False
        Nothing -> False -- Never been set
    disableLH = do
      ensureReAuthorised zusr mPassword
      Client.removeLegalHoldClientFromUser uid
      LHService.removeLegalHold tid uid
      -- TODO: send event at this point (see also: related TODO in this module in
      -- 'approveDevice' and
      -- https://github.com/wireapp/wire-server/pull/802#pullrequestreview-262280386)
      LegalHoldData.setUserLegalHoldStatus tid uid UserLegalHoldDisabled
