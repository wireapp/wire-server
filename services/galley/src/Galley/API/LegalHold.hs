module Galley.API.LegalHold where

import Imports
import Data.Aeson (encode)
import Galley.API.Error
import Brig.Types.Provider
import Brig.Types.Team.LegalHold
import Brig.Types.Client.Prekey
import Control.Monad.Catch
import Control.Lens (view, (^.))
import Data.Id
import Data.ByteString.Conversion (toByteString')
import Data.Misc
import Data.LegalHold (UserLegalHoldStatus(..))
import Galley.API.Util
import Galley.App
import Galley.Types.Teams as Team
import Network.HTTP.Types
import Network.HTTP.Types.Status (status201)
import Network.Wai
import Network.Wai.Predicate hiding (setStatus, result, or)
import Network.Wai.Utilities as Wai
import UnliftIO.Async (pooledMapConcurrentlyN_)

import qualified Galley.Data                  as Data
import qualified Galley.Data.LegalHold        as LegalHoldData
import qualified Galley.Intra.Client as Client
import qualified Galley.External.LegalHoldService as LHService
import qualified System.Logger as Logger

assertLegalHoldEnabled :: TeamId -> Galley ()
assertLegalHoldEnabled tid = unlessM (isLegalHoldEnabled tid) $ throwM legalHoldNotEnabled

isLegalHoldEnabled :: TeamId -> Galley Bool
isLegalHoldEnabled tid = do
    lhConfig <- LegalHoldData.getLegalHoldTeamConfig tid
    case lhConfig of
        Just LegalHoldTeamConfig{legalHoldTeamConfigStatus=LegalHoldEnabled}
          -> return True
        _ -> return True  -- TODO: must be false. this behaviour is only here for testing.

-- | Enable or disable legal hold for a team.
getEnabled :: TeamId ::: JSON -> Galley Response
getEnabled (tid ::: _) = do
    legalHoldTeamConfig <- LegalHoldData.getLegalHoldTeamConfig tid
    pure . json . fromMaybe defConfig $ legalHoldTeamConfig
      where
        defConfig = LegalHoldTeamConfig LegalHoldDisabled

-- | Enable or disable legal hold for a team.
setEnabled :: TeamId ::: JsonRequest LegalHoldTeamConfig ::: JSON -> Galley Response
setEnabled (tid ::: req ::: _) = do
    legalHoldTeamConfig <- fromJsonBody req
    case legalHoldTeamConfigStatus legalHoldTeamConfig of
        LegalHoldDisabled -> removeSettings' tid Nothing
        LegalHoldEnabled -> pure ()
    LegalHoldData.setLegalHoldTeamConfig tid legalHoldTeamConfig
    pure $ responseLBS status204 [] mempty

createSettings :: UserId ::: TeamId ::: JsonRequest NewLegalHoldService ::: JSON -> Galley Response
createSettings (zusr ::: tid ::: req ::: _) = do
    membs <- Data.teamMembers tid
    void $ permissionCheck zusr ChangeLegalHoldTeamSettings membs
    assertLegalHoldEnabled tid

    newService :: NewLegalHoldService
        <- fromJsonBody req

    (_key :: ServiceKey, fpr :: Fingerprint Rsa)
        <- LHService.validateServiceKey (newLegalHoldServiceKey newService)
               >>= maybe (throwM legalHoldServiceInvalidKey) pure
    LHService.checkLegalHoldServiceStatus fpr (newLegalHoldServiceUrl newService)

    let service = legalHoldService tid fpr newService
    LegalHoldData.createSettings service
    pure $ responseLBS status201 [] (encode . viewLegalHoldService $ service)

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

removeSettings :: UserId ::: TeamId ::: JSON -> Galley Response
removeSettings (zusr ::: tid ::: _) = do
    membs <- Data.teamMembers tid
    void $ permissionCheck zusr ChangeLegalHoldTeamSettings membs
    assertLegalHoldEnabled tid
    removeSettings' tid (Just membs)
    pure $ responseLBS status204 [] mempty

-- | Remove legal hold settings from team; also disabling for all users and removing LH devices
removeSettings'
    :: TeamId
    -> Maybe [TeamMember]
    -- ^ If you've already got the team members you can pass them in otherwise they'll be looked up.
    -> Galley ()
removeSettings' tid mMembers = do
    membs <- maybe (Data.teamMembers tid) pure mMembers
    let lhMembers = filter ((== UserLegalHoldEnabled) . view legalHoldStatus) membs
    -- I picked this number by fair dice roll, feel free to change it :P
    pooledMapConcurrentlyN_ 6 removeLHForUser lhMembers
    LegalHoldData.removeSettings tid
  where
    removeLHForUser :: TeamMember -> Galley ()
    removeLHForUser member = do
        let uid = member ^. Team.userId
        Client.removeLegalHoldClientFromUser uid
        LegalHoldData.setUserLegalHoldStatus tid uid UserLegalHoldDisabled

-- | Request to provision a device on the legal hold service for a user
-- Note that this is accessible to ANY authenticated user, even ones outside the team
getUserStatus :: UserId ::: TeamId ::: UserId ::: JSON -> Galley Response
getUserStatus (_zusr ::: tid ::: uid ::: _) = do
    assertLegalHoldEnabled tid
    mTeamMember <- Data.teamMember tid uid
    teamMember <- maybe (throwM teamMemberNotFound) pure mTeamMember
    lg <- view applog
    statusResponse <- case (view legalHoldStatus teamMember) of
        UserLegalHoldDisabled ->
            pure $ UserLegalHoldStatusResponse UserLegalHoldDisabled Nothing Nothing
        status -> do
            mLastKey <- fmap snd <$> LegalHoldData.selectPendingPrekeys uid
            lastKey <- case mLastKey of
                Nothing -> do
                    Logger.err lg . Logger.msg
                        $ "expected to find a prekey for user: "
                        <> toByteString' uid
                        <> " but none was found"
                    throwM internalError
                Just lstKey -> pure lstKey
            let clientId = clientIdFromPrekey . unpackLastPrekey $ lastKey
            pure $ UserLegalHoldStatusResponse status (Just lastKey) (Just clientId)
    pure . json $ statusResponse

-- | Request to provision a device on the legal hold service for a user
requestDevice :: UserId ::: TeamId ::: UserId ::: JSON -> Galley Response
requestDevice (zusr ::: tid ::: uid ::: _) = do
    membs <- Data.teamMembers tid
    void $ permissionCheck zusr ChangeLegalHoldUserSettings membs
    assertLegalHoldEnabled tid

    userLHStatus <- fmap (view legalHoldStatus) <$> Data.teamMember tid uid
    case userLHStatus of
        Just UserLegalHoldEnabled -> throwM userLegalHoldAlreadyEnabled
        Just UserLegalHoldPending -> provisionLHDevice
        Just UserLegalHoldDisabled -> provisionLHDevice
        Nothing -> throwM teamMemberNotFound
  where
    provisionLHDevice :: Galley Response
    provisionLHDevice = do
        (lastPrekey', prekeys) <- requestDeviceFromService
        -- We don't distinguish the last key here; brig will do so when the device is added
        LegalHoldData.insertPendingPrekeys uid (unpackLastPrekey lastPrekey' : prekeys)
        LegalHoldData.setUserLegalHoldStatus tid uid UserLegalHoldPending
        Client.notifyClientsAboutLegalHoldRequest zusr uid lastPrekey'
        pure $ responseLBS status204 [] mempty

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
approveDevice :: UserId ::: TeamId ::: UserId ::: ConnId ::: JSON -> Galley Response
approveDevice (zusr ::: tid ::: uid ::: connId ::: _) = do
    unless (zusr == uid) (throwM accessDenied)
    assertOnTeam uid tid
    assertLegalHoldEnabled tid
    assertUserLHPending

    mPreKeys <- LegalHoldData.selectPendingPrekeys uid
    lg <- view applog
    (prekeys, lastPrekey') <- case mPreKeys of
        Nothing -> do
            Logger.info lg $ Logger.msg @Text "No prekeys found"
            throwM noLegalHoldDeviceAllocated
        Just keys -> pure keys

    clientId <- Client.addLegalHoldClientToUser uid connId prekeys lastPrekey'
    -- TODO: this may be redundant
    Data.updateClient True uid clientId

    legalHoldAuthToken <- Client.getLegalHoldAuthToken uid
    LHService.confirmLegalHold clientId tid uid legalHoldAuthToken
    LegalHoldData.setUserLegalHoldStatus tid uid UserLegalHoldEnabled

    pure $ responseLBS status200 [] mempty
  where
    assertUserLHPending :: Galley ()
    assertUserLHPending = do
        userLHStatus <- fmap (view legalHoldStatus) <$> Data.teamMember tid uid
        case userLHStatus of
            Just UserLegalHoldEnabled -> throwM userLegalHoldAlreadyEnabled
            Just UserLegalHoldPending -> pure ()
            _ -> throwM userLegalHoldNotPending

disableForUser :: UserId ::: TeamId ::: UserId ::: JSON -> Galley Response
disableForUser (zusr ::: tid ::: uid ::: _) = do
    membs <- Data.teamMembers tid
    void $ permissionCheck zusr ChangeLegalHoldUserSettings membs

    Client.removeLegalHoldClientFromUser uid
    LHService.removeLegalHold tid uid
    LegalHoldData.setUserLegalHoldStatus tid uid UserLegalHoldDisabled
    pure $ responseLBS status200 [] mempty
