module Galley.API.LegalHold where

import Imports
import Data.Aeson (encode)
import Galley.API.Error
import Brig.Types.Provider
import Brig.Types.Team.LegalHold
import Brig.Types.Client.Prekey
import Control.Monad.Catch
import Data.Id
import Data.Misc
import Galley.API.Util
import Galley.App
import Galley.Types.Teams
import qualified Galley.External.LegalHoldService as LHService
import Network.HTTP.Types
import Network.HTTP.Types.Status (status201)
import Network.Wai
import Network.Wai.Predicate hiding (setStatus, result, or)
import Network.Wai.Utilities as Wai

import qualified Galley.Data                  as Data
import qualified Galley.Data.LegalHold        as LegalHoldData

assertLegalHoldEnabled :: TeamId -> Galley ()
assertLegalHoldEnabled tid = do
    lhConfig <- LegalHoldData.getLegalHoldTeamConfig tid
    case lhConfig of
        Just LegalHoldTeamConfig{legalHoldTeamConfigStatus=LegalHoldEnabled}
          -> return ()
        -- TODO: throw 'legalHoldNotEnabled' here before releasing. This behaviour is only
        -- here for testing.
        Nothing -> return ()
        _ -> throwM legalHoldNotEnabled

-- | Enable or disable legal hold for a team.
getEnabled :: TeamId ::: JSON -> Galley Response
getEnabled (tid ::: _) = do
    legalHoldTeamConfig <- LegalHoldData.getLegalHoldTeamConfig tid
    pure . json . fromMaybe defConfig $ legalHoldTeamConfig
      where
        defConfig = LegalHoldTeamConfig LegalHoldEnabled

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
    assertLegalHoldEnabled tid

    mresult <- LegalHoldData.getSettings tid
    case mresult of
        Nothing -> throwM legalHoldNotRegistered
        Just result -> pure $ json result

removeSettings :: UserId ::: TeamId ::: JSON -> Galley Response
removeSettings (zusr ::: tid ::: _) = do
    membs <- Data.teamMembers tid
    void $ permissionCheck zusr ChangeLegalHoldTeamSettings membs

    LegalHoldData.removeSettings tid
    pure $ responseLBS status204 [] mempty

-- | Request to provision a device on the legal hold service for a user
getUserStatus :: UserId ::: TeamId ::: UserId ::: JSON -> Galley Response
getUserStatus (zusr ::: tid ::: uid ::: _) = do
    membs <- Data.teamMembers tid
    void $ permissionCheck zusr ChangeLegalHoldUserSettings membs
    assertLegalHoldEnabled tid

    lhStatus <- LegalHoldData.getUserLegalHoldStatus uid
    pure $ json lhStatus

-- | Request to provision a device on the legal hold service for a user
requestDevice :: UserId ::: TeamId ::: UserId ::: JSON -> Galley Response
requestDevice (zusr ::: tid ::: uid ::: _) = do
    membs <- Data.teamMembers tid
    void $ permissionCheck zusr ChangeLegalHoldUserSettings membs
    assertLegalHoldEnabled tid

    lhDevice <- LHService.requestNewDevice tid uid
    let NewLegalHoldClient
          { newLegalHoldClientPrekeys = prekeys
          , newLegalHoldClientLastKey = lastKey
          } = lhDevice

    -- TODO: Do we distinguish the last key somehow?
    LegalHoldData.insertPendingPrekeys uid (unpackLastPrekey lastKey : prekeys)

    -- informClientsAboutPendingLH
    pure $ responseLBS status200 [] mempty
