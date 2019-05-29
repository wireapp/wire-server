module Galley.API.LegalHold where

import Imports
import Data.Aeson (encode)
import Galley.API.Error
import Brig.Types.Provider
import Brig.Types.Team.LegalHold
import Brig.Types.Client.Prekey
import Control.Monad.Catch
import Control.Lens (unsnoc)
import Data.Id
import Data.Misc
import Galley.API.Util
import Galley.App
import Galley.Types.Teams
import Galley.Intra.Client
  (notifyClientsAboutLegalHoldRequest, addLegalHoldClientToUser, getLegalHoldAuthToken)
import qualified Galley.External.LegalHoldService as LHService
import Network.HTTP.Types
import Network.HTTP.Types.Status (status201)
import Network.Wai
import Network.Wai.Predicate hiding (setStatus, result, or)
import Network.Wai.Utilities as Wai

import qualified Galley.Data                  as Data
import qualified Galley.Data.LegalHold        as LegalHoldData

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
    LegalHoldData.setLegalHoldTeamConfig tid legalHoldTeamConfig
    -- TODO: How do we remove all devices from all users?
    -- Do we also delete the settings from the table?
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

    LegalHoldData.removeSettings tid
    pure $ responseLBS status204 [] mempty

-- | Request to provision a device on the legal hold service for a user
getUserStatus :: UserId ::: TeamId ::: UserId ::: JSON -> Galley Response
getUserStatus (_zusr ::: tid ::: uid ::: _) = do
    assertLegalHoldEnabled tid

    lhStatus <- LegalHoldData.getUserLegalHoldStatus uid
    pure $ json lhStatus

-- | Request to provision a device on the legal hold service for a user
requestDevice :: UserId ::: TeamId ::: UserId ::: JSON -> Galley Response
requestDevice (zusr ::: tid ::: uid ::: _) = do
    membs <- Data.teamMembers tid
    void $ permissionCheck zusr ChangeLegalHoldUserSettings membs
    assertLegalHoldEnabled tid

    userLHStatus <- LegalHoldData.getUserLegalHoldStatus uid
    case userLHStatus of
        UserLegalHoldEnabled -> throwM userLegalHoldAlreadyEnabled
        UserLegalHoldPending -> provisionLHDevice
        UserLegalHoldDisabled -> provisionLHDevice
  where
    provisionLHDevice :: Galley Response
    provisionLHDevice = do
        (lastPrekey', prekeys) <- requestDeviceFromService
        -- We don't distinguish the last key here; brig will do so when the device is added
        LegalHoldData.insertPendingPrekeys uid (unpackLastPrekey lastPrekey' : prekeys)
        LegalHoldData.setUserLegalHoldStatus uid UserLegalHoldPending
        notifyClientsAboutLegalHoldRequest zusr uid lastPrekey' prekeys
        pure $ responseLBS status204 [] mempty

    requestDeviceFromService :: Galley (LastPrekey, [Prekey])
    requestDeviceFromService = do
        LegalHoldData.dropPendingPrekeys uid
        lhDevice <- LHService.requestNewDevice tid uid
        let NewLegalHoldClient prekeys lastKey = lhDevice
        return (lastKey, prekeys)

-- | Approve the adding of a Legal Hold device to the user
approveDevice :: UserId ::: TeamId ::: UserId ::: JSON -> Galley Response
approveDevice (zusr ::: tid ::: uid ::: _) = do
    unless (zusr == uid) (throwM accessDenied)
    assertOnTeam uid tid
    assertLegalHoldEnabled tid

    legalHoldAuthToken <- getLegalHoldAuthToken uid
    allPrekeys <- LegalHoldData.selectPendingPrekeys uid
    (prekeys, lastPrekey') <- case unsnoc allPrekeys of
        Nothing -> throwM internalError
        Just (keys, lst) -> pure (keys, lastPrekey . prekeyKey $ lst)
    addLegalHoldClientToUser uid prekeys lastPrekey'

    let clientId = undefined
    LHService.confirmLegalHold clientId tid uid legalHoldAuthToken
    return undefined

