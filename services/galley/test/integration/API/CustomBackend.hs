-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

import API.Util
import Bilge hiding (timeout)
import Bilge.Assert
import Data.Aeson hiding (json)
import Data.Aeson.QQ (aesonQQ)
import Imports
import Test.Tasty
import TestHelpers
import TestSetup

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "Custom Backends"
    [ test s "GET by-domain - domain does not exist (404)" getByDomainNotFound,
      test s "GET by-domain - domain invalid (404)" getByDomainInvalidDomain,
      test s "PUT, GET by-domain (200)" getByDomainFound,
      test s "PUT, DELETE, GET by-domain (404)" getByDomainDeleted,
      test s "domain is case-insensitive" getByDomainIsCaseInsensitive
    ]

getByDomainNotFound :: TestM ()
getByDomainNotFound = do
  galley <- viewGalley
  get (galley . path "/custom-backend/by-domain/domain.no1") !!! do
    const 404 === statusCode

getByDomainInvalidDomain :: TestM ()
getByDomainInvalidDomain = do
  galley <- viewGalley
  -- contains invalid character '+'
  -- this used to respond with '400 bad request'
  -- but after servantification it returns '404 not found'
  -- because the domain parameter is invalid and therefore the endpoint is invalid, too
  get (galley . path "/custom-backend/by-domain/invalid%2Bdomain") !!! do
    const 404 === statusCode

getByDomainFound :: TestM ()
getByDomainFound = do
  galley <- viewGalley
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
  galley <- viewGalley
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

getByDomainIsCaseInsensitive :: TestM ()
getByDomainIsCaseInsensitive = do
  galley <- viewGalley
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
