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
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Conversion.To
import Data.ByteString.Lazy.Char8 qualified as LC8
import Data.Id
import Data.Misc
import Data.Qualified (Local, QualifiedWithTag (tUntagged), tUnqualified)
import Data.Set qualified as Set
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

data LhApiVersion = V0 | V1
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

versionToInt :: LhApiVersion -> Int
versionToInt V0 = 0
versionToInt V1 = 1

versionToBS :: LhApiVersion -> ByteString
versionToBS = ("v" <>) . BS8.pack . show . versionToInt

versionedPaths :: LhApiVersion -> [ByteString] -> Http.Request -> Http.Request
versionedPaths V0 paths = Bilge.paths paths
versionedPaths v paths = Bilge.paths (versionToBS v : paths)

supportedClientVersions :: Set LhApiVersion
supportedClientVersions = Set.fromList [minBound .. maxBound]

getLegalHoldApiVersion ::
  ( Member (ErrorS 'LegalHoldServiceNotRegistered) r,
    Member LegalHoldStore r
  ) =>
  TeamId ->
  Sem r (Maybe (Set LhApiVersion))
getLegalHoldApiVersion tid =
  fmap toLhApiVersion . decode . (.responseBody) <$> makeLegalHoldServiceRequest tid params
  where
    params =
      Bilge.paths ["api-version"]
        . Bilge.method GET
        . Bilge.acceptJson

    toLhApiVersion :: SupportedVersions -> Set LhApiVersion
    toLhApiVersion (SupportedVersions supported) = Set.fromList $ mapMaybe toVersion supported
      where
        toVersion 0 = Just V0
        toVersion 1 = Just V1
        toVersion _ = Nothing

negotiateVersion ::
  ( Member (ErrorS 'LegalHoldServiceNotRegistered) r,
    Member (ErrorS 'LegalHoldServiceBadResponse) r,
    Member LegalHoldStore r
  ) =>
  TeamId ->
  Set LhApiVersion ->
  Sem r LhApiVersion
negotiateVersion tid clientVersions = do
  mSupportedServerVersions <- getLegalHoldApiVersion tid
  case mSupportedServerVersions of
    Nothing -> pure V0
    Just serverVersions -> do
      let commonVersions = Set.intersection clientVersions serverVersions
      case Set.lookupMax commonVersions of
        Nothing -> throwS @'LegalHoldServiceBadResponse
        Just version -> pure version

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
  Local UserId ->
  Sem r NewLegalHoldClient
requestNewDevice tid luid = do
  apiVersion <- negotiateVersion tid supportedClientVersions
  resp <- makeLegalHoldServiceRequest tid (reqParams apiVersion)
  case eitherDecode (responseBody resp) of
    Left e -> do
      P.info . Log.msg $ "Error decoding NewLegalHoldClient: " <> e
      throwS @'LegalHoldServiceBadResponse
    Right client -> pure client
  where
    reqParams v =
      versionedPaths v ["initiate"]
        . mkBody v
        . Bilge.method POST
        . Bilge.acceptJson
        . Bilge.expect2xx

    mkBody :: LhApiVersion -> Bilge.Request -> Bilge.Request
    mkBody V0 = Bilge.json (RequestNewLegalHoldClientV0 (tUnqualified luid) tid)
    mkBody V1 = Bilge.json (RequestNewLegalHoldClient (tUntagged luid) tid)

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
