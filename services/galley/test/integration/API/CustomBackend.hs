module API.CustomBackend (tests) where

import Imports
import Bilge hiding (timeout)
import Bilge.Assert
import Control.Lens (view)
import Data.Aeson hiding (json)
import Data.Aeson.QQ (aesonQQ)
import Data.String.Conversions (cs)
import Test.Tasty
import TestHelpers
import TestSetup


tests :: IO TestSetup -> TestTree
tests s = testGroup "Custom Backends"
    [ test s "GET by-domain (404)" getByDomainNotFound
    , test s "GET by-domain (400)" getByDomainInvalidDomain
    , test s "PUT, GET by-domain (200)" getByDomainFound
    , test s "PUT, DELETE, GET by-domain (404)" getByDomainDeleted
    ]

getByDomainNotFound :: TestM ()
getByDomainNotFound = do
    galley <- view tsGalley
    get (galley . path "/custom-backend/by-domain/domain.no1") !!! do
        const 404 === statusCode
        const ("domain.no1" :: ByteString) =~= (cs . fold . responseBody)

getByDomainInvalidDomain :: TestM ()
getByDomainInvalidDomain = do
    galley <- view tsGalley
    -- contains invalid character '+'
    get (galley . path "/custom-backend/by-domain/invalid%2Bdomain") !!! do
        const 400 === statusCode
        const ("Failed parsing" :: ByteString) =~= (cs . fold . responseBody)

getByDomainFound :: TestM ()
getByDomainFound = do
    galley <- view tsGalley

    let jsonBody :: Value
        jsonBody = [aesonQQ|{
          "config_json_url": "https://wire-rest.https.no2.com/config.json",
          "webapp_welcome_url": "https://app.wire.no2.com/",
          "block_cloud_users": false
          }|]

    put (galley . path "/i/custom-backend/by-domain/domain.no2" . json jsonBody) !!!
        const 201 === statusCode

    get (galley . path "/custom-backend/by-domain/domain.no2") !!! do
        const 200 === statusCode
        const jsonBody === responseJsonUnsafe

getByDomainDeleted :: TestM ()
getByDomainDeleted = do
    galley <- view tsGalley

    let jsonBody :: Value
        jsonBody = [aesonQQ|{
          "config_json_url": "https://wire-rest.https.no3.com/config.json",
          "webapp_welcome_url": "https://app.wire.no3.com/",
          "block_cloud_users": false
          }|]

    put (galley . path "/i/custom-backend/by-domain/domain.no3" . json jsonBody) !!!
        const 201 === statusCode

    delete (galley . path "/i/custom-backend/by-domain/domain.no3") !!!
        const 200 === statusCode

    get (galley . path "/custom-backend/by-domain/domain.no3") !!! do
        const 404 === statusCode
        const ("domain.no3" :: ByteString) =~= (cs . fold . responseBody)
