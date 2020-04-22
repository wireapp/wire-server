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

module API.CustomBackend
  ( tests,
  )
where

import Bilge hiding (timeout)
import Bilge.Assert
import Control.Lens (view)
import Data.Aeson hiding (json)
import Data.Aeson.QQ (aesonQQ)
import Data.String.Conversions (cs)
import Imports
import Test.Tasty
import TestHelpers
import TestSetup

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "Custom Backends"
    [ test s "GET by-domain (404)" getByDomainNotFound,
      test s "GET by-domain (400)" getByDomainInvalidDomain,
      test s "PUT, GET by-domain (200)" getByDomainFound,
      test s "PUT, DELETE, GET by-domain (404)" getByDomainDeleted,
      test s "domain is case-insensitive" getByDomainIsCaseInsensitive
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
    const ("client-error" :: ByteString) =~= (cs . fold . responseBody)

getByDomainFound :: TestM ()
getByDomainFound = do
  galley <- view tsGalley
  let jsonBody :: Value
      jsonBody =
        [aesonQQ|{
          "config_json_url": "https://wire-rest.https.no2.com/config.json",
          "webapp_welcome_url": "https://app.wire.no2.com/"
          }|]
  put (galley . path "/i/custom-backend/by-domain/domain.no2" . json jsonBody)
    !!! const 201 === statusCode
  get (galley . path "/custom-backend/by-domain/domain.no2") !!! do
    const 200 === statusCode
    const jsonBody === responseJsonUnsafe

getByDomainDeleted :: TestM ()
getByDomainDeleted = do
  galley <- view tsGalley
  let jsonBody :: Value
      jsonBody =
        [aesonQQ|{
          "config_json_url": "https://wire-rest.https.no3.com/config.json",
          "webapp_welcome_url": "https://app.wire.no3.com/"
          }|]
  put (galley . path "/i/custom-backend/by-domain/domain.no3" . json jsonBody)
    !!! const 201 === statusCode
  delete (galley . path "/i/custom-backend/by-domain/domain.no3")
    !!! const 200 === statusCode
  get (galley . path "/custom-backend/by-domain/domain.no3") !!! do
    const 404 === statusCode
    const ("domain.no3" :: ByteString) =~= (cs . fold . responseBody)

getByDomainIsCaseInsensitive :: TestM ()
getByDomainIsCaseInsensitive = do
  galley <- view tsGalley
  let jsonBody :: Value
      jsonBody =
        [aesonQQ|{
          "config_json_url": "https://wire-rest.https.no2.com/config.json",
          "webapp_welcome_url": "https://app.wire.no2.com/"
          }|]
  put (galley . path "/i/custom-backend/by-domain/Ab.Ab" . json jsonBody)
    !!! const 201 === statusCode
  get (galley . path "/custom-backend/by-domain/aB.aB") !!! do
    const 200 === statusCode
    const jsonBody === responseJsonUnsafe
  delete (galley . path "/i/custom-backend/by-domain/AB.ab")
    !!! const 200 === statusCode
  get (galley . path "/custom-backend/by-domain/Ab.Ab") !!! do
    const 404 === statusCode
