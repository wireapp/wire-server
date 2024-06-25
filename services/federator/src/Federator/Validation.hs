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

module Federator.Validation
  ( ensureCanFederateWith,
    parseDomain,
    decodeCertificate,
    validateDomain,
    validateDomainName,
    ValidationError (..),
  )
where

import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Conversion
import Data.Domain
import Data.List.NonEmpty (NonEmpty)
import Data.PEM qualified as X509
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding.Error qualified as Text
import Data.Text.Lazy qualified as LText
import Data.X509 qualified as X509
import Data.X509.Validation qualified as X509
import Federator.Discovery
import Federator.Error
import Imports
import Network.HTTP.Types qualified as HTTP
import Network.Wai.Utilities.Error qualified as Wai
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Wire.API.Routes.FederationDomainConfig
import Wire.Network.DNS.SRV (SrvTarget (..))

data ValidationError
  = NoClientCertificate
  | CertificateParseError Text
  | DomainParseError Text
  | AuthenticationFailure (NonEmpty [X509.FailedReason])
  | FederationDenied Domain
  deriving (Eq, Show, Typeable)

instance Exception ValidationError

instance AsWai ValidationError where
  toWai err =
    Wai.mkError (validationErrorStatus err) (validationErrorLabel err)
      . LText.fromStrict
      $ validationErrorDescription err

validationErrorDescription :: ValidationError -> Text
validationErrorDescription NoClientCertificate = "no client certificate provided"
validationErrorDescription (CertificateParseError reason) =
  "certificate parse failure: " <> reason
validationErrorDescription (DomainParseError domain) =
  "domain parse failure for [" <> domain <> "]"
validationErrorDescription (AuthenticationFailure errs) =
  "none of the domain names match the certificate, errors: "
    <> Text.pack (show (toList errs))
validationErrorDescription (FederationDenied domain) =
  "origin domain [" <> domainText domain <> "] not in the federation allow list"

validationErrorLabel :: ValidationError -> LText
validationErrorLabel NoClientCertificate = "no-client-certificate"
validationErrorLabel (CertificateParseError _) = "certificate-parse-error"
validationErrorLabel (DomainParseError _) = "domain-parse-error"
validationErrorLabel (AuthenticationFailure _) = "authentication-failure"
validationErrorLabel (FederationDenied _) = "federation-denied"

validationErrorStatus :: ValidationError -> HTTP.Status
-- the FederationDenied case is handled differently, because it may be caused
-- by wrong input in the original request, so we let this error propagate to the
-- client
validationErrorStatus (FederationDenied _) = HTTP.status400
validationErrorStatus _ = HTTP.status403

-- | Validates an already-parsed domain against the allow list (stored in
-- `brig.federation_remotes`, cached in `Env`).
ensureCanFederateWith ::
  ( Member (Input FederationDomainConfigs) r,
    Member (Error ValidationError) r
  ) =>
  Domain ->
  Sem r ()
ensureCanFederateWith targetDomain = do
  FederationDomainConfigs strategy domains _ <- input
  case strategy of
    AllowNone -> throw (FederationDenied targetDomain)
    AllowAll -> pure ()
    AllowDynamic -> do
      unless (targetDomain `elem` fmap domain domains) $
        throw (FederationDenied targetDomain)

decodeCertificate ::
  ByteString ->
  Either String X509.Certificate
decodeCertificate =
  (pure . X509.getCertificate)
    <=< X509.decodeSignedCertificate
    <=< (pure . X509.pemContent)
    <=< expectOne "certificate"
    <=< X509.pemParseBS
  where
    expectOne :: String -> [a] -> Either String a
    expectOne label [] = Left $ "no " <> label <> " found"
    expectOne _ [x] = pure x
    expectOne label _ = Left $ "found multiple " <> label <> "s"

parseDomain :: (Member (Error ValidationError) r) => ByteString -> Sem r Domain
parseDomain domain =
  note (DomainParseError (Text.decodeUtf8With Text.lenientDecode domain)) $
    fromByteString domain

-- | Validates an unknown domain string against the allow list using the
-- federator startup configuration and checks that it matches the names reported
-- by the client certificate
validateDomain ::
  ( Member (Input FederationDomainConfigs) r,
    Member (Error ValidationError) r,
    Member (Error DiscoveryFailure) r,
    Member DiscoverFederator r
  ) =>
  X509.Certificate ->
  Domain ->
  Sem r Domain
validateDomain certificate targetDomain = do
  ensureCanFederateWith targetDomain

  -- run discovery to find the hostname of the client federator
  hostnames <- srvTargetDomain <$$> discoverAllFederatorsWithError targetDomain
  let validationErrors = (\h -> validateDomainName (B8.unpack h) certificate) <$> hostnames
  unless (any null validationErrors) $
    throw $
      AuthenticationFailure validationErrors

  pure targetDomain

-- | Match a hostname against the domain names of a certificate.
--
-- We strip the trailing dot from the domain, as the certificate is not
-- expected to have the trailing dot.
validateDomainName :: String -> X509.Certificate -> [X509.FailedReason]
validateDomainName hostname =
  X509.hookValidateName X509.defaultHooks (stripDot hostname)
  where
    stripDot h
      | "." `isSuffixOf` h = take (length h - 1) h
      | otherwise = h
