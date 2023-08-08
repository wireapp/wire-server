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

module Galley.External.LegalHoldService
  ( -- * api
    checkLegalHoldServiceStatus,
    requestNewDevice,
    confirmLegalHold,
    removeLegalHold,

    -- * helpers
    validateServiceKey,
  )
where

import Bilge qualified
import Bilge.Response
import Brig.Types.Team.LegalHold
import Data.Aeson
import Data.ByteString.Conversion.To
import Data.ByteString.Lazy.Char8 qualified as LC8
import Data.Id
import Data.Misc
import Galley.Effects.LegalHoldStore as LegalHoldData
import Galley.External.LegalHoldService.Types
import Imports
import Network.HTTP.Client qualified as Http
import Network.HTTP.Types
import Polysemy
import Polysemy.TinyLog qualified as P
import System.Logger.Class qualified as Log
import Wire.API.Error (ErrorS, throwS)
import Wire.API.Error.Galley
import Wire.API.Team.LegalHold.External

----------------------------------------------------------------------
-- api

-- | Get /status from legal hold service; throw 'Wai.Error' if things go wrong.
checkLegalHoldServiceStatus ::
  ( Member (ErrorS 'LegalHoldServiceBadResponse) r,
    Member LegalHoldStore r,
    Member P.TinyLog r
  ) =>
  Fingerprint Rsa ->
  HttpsUrl ->
  Sem r ()
checkLegalHoldServiceStatus fpr url = do
  resp <- makeVerifiedRequestFreshManager fpr url reqBuilder
  if Bilge.statusCode resp < 400
    then pure ()
    else do
      P.info . Log.msg $ showResponse resp
      throwS @'LegalHoldServiceBadResponse
  where
    reqBuilder :: Http.Request -> Http.Request
    reqBuilder =
      Bilge.paths ["status"]
        . Bilge.method GET
        . Bilge.expect2xx

-- | @POST /initiate@.
requestNewDevice ::
  ( Member (ErrorS 'LegalHoldServiceBadResponse) r,
    Member (ErrorS 'LegalHoldServiceNotRegistered) r,
    Member LegalHoldStore r,
    Member P.TinyLog r
  ) =>
  TeamId ->
  UserId ->
  Sem r NewLegalHoldClient
requestNewDevice tid uid = do
  resp <- makeLegalHoldServiceRequest tid reqParams
  case eitherDecode (responseBody resp) of
    Left e -> do
      P.info . Log.msg $ "Error decoding NewLegalHoldClient: " <> e
      throwS @'LegalHoldServiceBadResponse
    Right client -> pure client
  where
    reqParams =
      Bilge.paths ["initiate"]
        . Bilge.json (RequestNewLegalHoldClient uid tid)
        . Bilge.method POST
        . Bilge.acceptJson
        . Bilge.expect2xx

-- | @POST /confirm@
-- Confirm that a device has been linked to a user and provide an authorization token
confirmLegalHold ::
  ( Member (ErrorS 'LegalHoldServiceNotRegistered) r,
    Member LegalHoldStore r
  ) =>
  ClientId ->
  TeamId ->
  UserId ->
  -- | TODO: Replace with 'LegalHold' token type
  OpaqueAuthToken ->
  Sem r ()
confirmLegalHold clientId tid uid legalHoldAuthToken = do
  void $ makeLegalHoldServiceRequest tid reqParams
  where
    reqParams =
      Bilge.paths ["confirm"]
        . Bilge.json (LegalHoldServiceConfirm clientId uid tid (opaqueAuthTokenToText legalHoldAuthToken))
        . Bilge.method POST
        . Bilge.acceptJson
        . Bilge.expect2xx

-- | @POST /remove@
-- Inform the LegalHold Service that a user's legalhold has been disabled.
removeLegalHold ::
  ( Member (ErrorS 'LegalHoldServiceNotRegistered) r,
    Member LegalHoldStore r
  ) =>
  TeamId ->
  UserId ->
  Sem r ()
removeLegalHold tid uid = do
  void $ makeLegalHoldServiceRequest tid reqParams
  where
    reqParams =
      Bilge.paths ["remove"]
        . Bilge.json (LegalHoldServiceRemove uid tid)
        . Bilge.method POST
        . Bilge.acceptJson
        . Bilge.expect2xx

----------------------------------------------------------------------
-- helpers

-- | Lookup legal hold service settings for a team and make a request to the service.  Pins
-- the TSL fingerprint via 'makeVerifiedRequest' and passes the token so the service can
-- authenticate the request.
makeLegalHoldServiceRequest ::
  ( Member (ErrorS 'LegalHoldServiceNotRegistered) r,
    Member LegalHoldStore r
  ) =>
  TeamId ->
  (Http.Request -> Http.Request) ->
  Sem r (Http.Response LC8.ByteString)
makeLegalHoldServiceRequest tid reqBuilder = do
  maybeLHSettings <- LegalHoldData.getSettings tid
  lhSettings <- case maybeLHSettings of
    Nothing -> throwS @'LegalHoldServiceNotRegistered
    Just lhSettings -> pure lhSettings
  let LegalHoldService
        { legalHoldServiceUrl = baseUrl,
          legalHoldServiceFingerprint = fpr,
          legalHoldServiceToken = serviceToken
        } = lhSettings
  makeVerifiedRequest fpr baseUrl $ mkReqBuilder serviceToken
  where
    mkReqBuilder token =
      reqBuilder
        . Bilge.header "Authorization" ("Bearer " <> toByteString' token)
