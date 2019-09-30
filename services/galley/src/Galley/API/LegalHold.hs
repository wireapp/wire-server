module Galley.API.LegalHold where

import Imports
import Galley.API.Error
import Brig.Types.Provider
import Brig.Types.Team.LegalHold hiding (userId)
import Brig.Types.Client.Prekey
import Control.Monad.Catch
import Control.Lens (view, (^.))
import Data.Id
import Data.ByteString.Conversion (toByteString', toByteString)
import Data.Misc
import Data.LegalHold (UserLegalHoldStatus(..))
import Galley.API.Util
import Galley.App
import Galley.Types.Teams as Team
import Network.HTTP.Types.Status (status201, status204)
import Network.Wai
import Network.Wai.Predicate hiding (setStatus, result, or)
import Network.Wai.Utilities as Wai
import UnliftIO.Async (pooledMapConcurrentlyN_)

import qualified Galley.Data                  as Data
import qualified Galley.Data.LegalHold        as LegalHoldData
import qualified Galley.Intra.Client as Client
import qualified Galley.External.LegalHoldService as LHService
import qualified System.Logger.Class as Log

assertLegalHoldEnabled :: TeamId -> Galley ()
assertLegalHoldEnabled tid = unlessM (isLegalHoldEnabled tid) $ throwM legalHoldNotEnabled

isLegalHoldEnabled :: TeamId -> Galley Bool
isLegalHoldEnabled tid = do
    lhConfig <- LegalHoldData.getLegalHoldTeamConfig tid
    return $ case legalHoldTeamConfigStatus <$> lhConfig of
        Just LegalHoldEnabled  -> True
        Just LegalHoldDisabled -> False
        Nothing                -> False

createSettings :: UserId ::: TeamId ::: JsonRequest NewLegalHoldService ::: JSON -> Galley Response
createSettings (zusr ::: tid ::: req ::: _) = do
    assertLegalHoldEnabled tid

    membs <- Data.teamMembers tid
    let zothers = map (view userId) membs
    Log.debug $ Log.field "targets" (toByteString . show $ toByteString <$> zothers)
              . Log.field "action" (Log.val "LegalHold.createSettings")
    
    void $ permissionCheck zusr ChangeLegalHoldTeamSettings membs

    newService :: NewLegalHoldService
        <- fromJsonBody req

    (key :: ServiceKey, fpr :: Fingerprint Rsa)
        <- LHService.validateServiceKey (newLegalHoldServiceKey newService)
               >>= maybe (throwM legalHoldServiceInvalidKey) pure
    LHService.checkLegalHoldServiceStatus fpr (newLegalHoldServiceUrl newService)

    let service = legalHoldService tid fpr newService key
    LegalHoldData.createSettings service
    pure . setStatus status201 . json . viewLegalHoldService $ service

getSettings :: UserId ::: TeamId ::: JSON -> Galley Response
getSettings (zusr ::: tid ::: _) = do
    membs <- Data.teamMembers tid
    void $ permissionCheck zusr ViewLegalHoldTeamSettings membs
    isenabled <- isLegalHoldEnabled tid

    mresult <- LegalHoldData.getSettings tid
    pure . json $ case (isenabled, mresult) of
        (False, _)          -> ViewLegalHoldServiceDisabled
        (True, Nothing)     -> ViewLegalHoldServiceNotConfigured
        (True, Just result) -> viewLegalHoldService result

removeSettings :: UserId ::: TeamId ::: JsonRequest RemoveLegalHoldSettingsRequest ::: JSON -> Galley Response
removeSettings (zusr ::: tid ::: req ::: _) = do
    assertLegalHoldEnabled tid
    membs <- Data.teamMembers tid
    let zothers = map (view userId) membs
    Log.debug $ Log.field "targets" (toByteString . show $ toByteString <$> zothers)
              . Log.field "action" (Log.val "LegalHold.removeSettings")

    void $ permissionCheck zusr ChangeLegalHoldTeamSettings membs
    RemoveLegalHoldSettingsRequest mPassword <- fromJsonBody req
    ensureReAuthorised zusr mPassword
    removeSettings' tid (Just membs)
    pure noContent

-- | Remove legal hold settings from team; also disabling for all users and removing LH devices
removeSettings'
    :: TeamId
    -> Maybe [TeamMember]
    -- ^ If you've already got the team members you can pass them in otherwise they'll be looked up.
    -> Galley ()
removeSettings' tid mMembers = do
    membs <- maybe (Data.teamMembers tid) pure mMembers
    let zothers = map (view userId) membs
    Log.debug $ Log.field "targets" (toByteString . show $ toByteString <$> zothers)
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
getUserStatus :: UserId ::: TeamId ::: UserId ::: JSON -> Galley Response
getUserStatus (_zusr ::: tid ::: uid ::: _) = do
    mTeamMember <- Data.teamMember tid uid
    teamMember <- maybe (throwM teamMemberNotFound) pure mTeamMember
    statusResponse <- case view legalHoldStatus teamMember of
        UserLegalHoldDisabled ->
            pure $ UserLegalHoldStatusResponse UserLegalHoldDisabled Nothing Nothing
        status@UserLegalHoldPending -> makeResponse status
        status@UserLegalHoldEnabled -> makeResponse status
    pure . json $ statusResponse
  where
    makeResponse status = do
        mLastKey <- fmap snd <$> LegalHoldData.selectPendingPrekeys uid
        lastKey <- case mLastKey of
            Nothing -> do
                Log.err . Log.msg $ "expected to find a prekey for user: "
                                 <> toByteString' uid <> " but none was found"
                throwM internalError
            Just lstKey -> pure lstKey
        let clientId = clientIdFromPrekey . unpackLastPrekey $ lastKey
        pure $ UserLegalHoldStatusResponse status (Just lastKey) (Just clientId)


-- | Request to provision a device on the legal hold service for a user
requestDevice :: UserId ::: TeamId ::: UserId ::: JSON -> Galley Response
requestDevice (zusr ::: tid ::: uid ::: _) = do
    assertLegalHoldEnabled tid

    Log.debug $ Log.field "targets" (toByteString uid)
              . Log.field "action" (Log.val "LegalHold.requestDevice")
    membs <- Data.teamMembers tid
    void $ permissionCheck zusr ChangeLegalHoldUserSettings membs

    userLHStatus <- fmap (view legalHoldStatus) <$> Data.teamMember tid uid
    case userLHStatus of
        Just UserLegalHoldEnabled -> throwM userLegalHoldAlreadyEnabled
        Just UserLegalHoldPending ->  provisionLHDevice <&> setStatus status204
        Just UserLegalHoldDisabled -> provisionLHDevice <&> setStatus status201
        Nothing -> throwM teamMemberNotFound
  where
    provisionLHDevice :: Galley Response
    provisionLHDevice = do
        (lastPrekey', prekeys) <- requestDeviceFromService
        -- We don't distinguish the last key here; brig will do so when the device is added
        LegalHoldData.insertPendingPrekeys uid (unpackLastPrekey lastPrekey' : prekeys)
        LegalHoldData.setUserLegalHoldStatus tid uid UserLegalHoldPending
        Client.notifyClientsAboutLegalHoldRequest zusr uid lastPrekey'
        pure empty

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
approveDevice
    :: UserId ::: TeamId ::: UserId ::: ConnId ::: JsonRequest ApproveLegalHoldForUserRequest ::: JSON
    -> Galley Response
approveDevice (zusr ::: tid ::: uid ::: connId ::: req ::: _) = do
    assertLegalHoldEnabled tid
    Log.debug $ Log.field "targets" (toByteString uid)
              . Log.field "action" (Log.val "LegalHold.approveDevice")

    unless (zusr == uid) (throwM accessDenied)
    assertOnTeam uid tid
    ApproveLegalHoldForUserRequest mPassword <- fromJsonBody req
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
    LegalHoldData.setUserLegalHoldStatus tid uid UserLegalHoldEnabled
    -- TODO: send event at this point (see also:
    -- https://github.com/wireapp/wire-server/pull/802#pullrequestreview-262280386)
    pure empty
  where
    assertUserLHPending :: Galley ()
    assertUserLHPending = do
        userLHStatus <- fmap (view legalHoldStatus) <$> Data.teamMember tid uid
        case userLHStatus of
            Just UserLegalHoldEnabled -> throwM userLegalHoldAlreadyEnabled
            Just UserLegalHoldPending -> pure ()
            _ -> throwM userLegalHoldNotPending

disableForUser
  :: UserId ::: TeamId ::: UserId ::: JsonRequest DisableLegalHoldForUserRequest ::: JSON
  -> Galley Response
disableForUser (zusr ::: tid ::: uid ::: req ::: _) = do
    Log.debug $ Log.field "targets" (toByteString uid)
              . Log.field "action" (Log.val "LegalHold.disableForUser")
    membs <- Data.teamMembers tid
    void $ permissionCheck zusr ChangeLegalHoldUserSettings membs
    if userLHNotDisabled membs
        then disableLH >> pure empty
        else pure noContent
  where
    -- If not enabled nor pending, then it's disabled
    userLHNotDisabled mems = do
        let target = findTeamMember uid mems
        case fmap (view legalHoldStatus) target of
            Just UserLegalHoldEnabled  -> True
            Just UserLegalHoldPending  -> True
            Just UserLegalHoldDisabled -> False
            Nothing                    -> False -- Never been set

    disableLH = do
        DisableLegalHoldForUserRequest mPassword <- fromJsonBody req
        ensureReAuthorised zusr mPassword
        Client.removeLegalHoldClientFromUser uid
        LHService.removeLegalHold tid uid
        LegalHoldData.setUserLegalHoldStatus tid uid UserLegalHoldDisabled
        -- TODO: send event at this point (see also: related TODO in this module in
        -- 'approveDevice' and
        -- https://github.com/wireapp/wire-server/pull/802#pullrequestreview-262280386)
