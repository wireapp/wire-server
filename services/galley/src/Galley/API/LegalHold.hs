module Galley.API.LegalHold where

import Imports
import Data.Aeson (encode)
import Galley.API.Error
import Bilge.Retry
import Brig.Types.Provider
import Brig.Types.Team.LegalHold
import Control.Exception.Enclosed (handleAny)
import Control.Lens
import Control.Monad.Catch
import Control.Retry
import Data.ByteString.Conversion.To
import Data.Id
import Data.Misc
import Galley.API.Util
import Galley.App
import Galley.Types.Teams
import Network.HTTP.Types
import Network.HTTP.Types.Status (status201)
import Network.Wai
import Network.Wai.Predicate hiding (setStatus, result, or)
import Network.Wai.Utilities as Wai
import Ssl.Util

import qualified Bilge
import qualified Data.ByteString.Lazy.Char8   as LC8
import qualified Galley.Data                  as Data
import qualified Galley.Data.LegalHold        as LegalHoldData
import qualified Network.HTTP.Client          as Http
import qualified OpenSSL.EVP.Digest           as SSL
import qualified OpenSSL.EVP.PKey             as SSL
import qualified OpenSSL.PEM                  as SSL
import qualified OpenSSL.RSA                  as SSL
import qualified Ssl.Util                     as SSL

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
        <- validateServiceKey (newLegalHoldServiceKey newService)
               >>= maybe (throwM legalHoldServiceInvalidKey) pure
    checkLegalHoldServiceStatus fpr (newLegalHoldServiceUrl newService)

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
requestDevice :: UserId ::: TeamId ::: UserId ::: JSON -> Galley Response
requestDevice (zusr ::: tid ::: uid ::: _) = do
    membs <- Data.teamMembers tid
    void $ permissionCheck zusr ChangeLegalHoldUserSettings membs
    assertLegalHoldEnabled tid

    mresult <- LegalHoldData.getSettings tid
    legalHoldService <- case mresult of
        Nothing -> throwM legalHoldNotRegistered
        Just lhs -> pure lhs
    undefined


