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

module Federator.Validation
  ( ensureCanFederateWith,
    parseDomain,
    parseDomainText,
    validateDomain,
    validateDomainName,
    ValidationError (..),
  )
where

import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Conversion
import Data.Domain
import Data.List.NonEmpty (NonEmpty)
import qualified Data.PEM as X509
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Data.Text.Lazy as LText
import qualified Data.X509 as X509
import qualified Data.X509.Validation as X509
import Federator.Discovery
import Federator.Error
import Federator.Options
import Imports
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai.Utilities.Error as Wai
import Polysemy
import Polysemy.Error
import Polysemy.Input
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
    Wai.mkError HTTP.status403 (validationErrorLabel err)
      . LText.fromStrict
      $ waiErrorDescription err

  waiErrorDescription :: ValidationError -> Text
  waiErrorDescription NoClientCertificate = "no client certificate provided"
  waiErrorDescription (CertificateParseError reason) =
    "certificate parse failure: " <> reason
  waiErrorDescription (DomainParseError domain) =
    "domain parse failure for [" <> domain <> "]"
  waiErrorDescription (AuthenticationFailure errs) =
    "none of the domain names match the certificate, errors: "
      <> Text.pack (show (toList errs))
  waiErrorDescription (FederationDenied domain) =
    "origin domain [" <> domainText domain <> "] not in the federation allow list"

validationErrorLabel :: ValidationError -> LText
validationErrorLabel NoClientCertificate = "no-client-certificate"
validationErrorLabel (CertificateParseError _) = "certificate-parse-error"
validationErrorLabel (DomainParseError _) = "domain-parse-error"
validationErrorLabel (AuthenticationFailure _) = "authentication-failure"
validationErrorLabel (FederationDenied _) = "federation-denied"

-- | Validates an already-parsed domain against the allowList using the federator
-- startup configuration.
ensureCanFederateWith ::
  Members '[Input RunSettings, Error ValidationError] r =>
  Domain ->
  Sem r ()
ensureCanFederateWith targetDomain = do
  strategy <- inputs federationStrategy
  case strategy of
    AllowAll -> pure ()
    AllowList (AllowedDomains domains) ->
      unless (targetDomain `elem` domains) $
        throw (FederationDenied targetDomain)

decodeCertificate ::
  Member (Error String) r =>
  ByteString ->
  Sem r X509.Certificate
decodeCertificate =
  fromEither
    . ( (pure . X509.getCertificate)
          <=< X509.decodeSignedCertificate
          <=< (pure . X509.pemContent)
          <=< expectOne "certificate"
          <=< X509.pemParseBS
      )
  where
    expectOne :: String -> [a] -> Either String a
    expectOne label [] = Left $ "no " <> label <> " found"
    expectOne _ [x] = pure x
    expectOne label _ = Left $ "found multiple " <> label <> "s"

parseDomain :: Member (Error ValidationError) r => ByteString -> Sem r Domain
parseDomain domain =
  note (DomainParseError (Text.decodeUtf8With Text.lenientDecode domain)) $
    fromByteString domain

parseDomainText :: Member (Error ValidationError) r => Text -> Sem r Domain
parseDomainText domain =
  mapError @String (const (DomainParseError domain))
    . fromEither
    . mkDomain
    $ domain

-- | Validates an unknown domain string against the allowList using the
-- federator startup configuration and checks that it matches the names reported
-- by the client certificate
validateDomain ::
  Members
    '[ Input RunSettings,
       Error ValidationError,
       Error DiscoveryFailure,
       DiscoverFederator
     ]
    r =>
  Maybe ByteString ->
  ByteString ->
  Sem r Domain
validateDomain Nothing _ = throw NoClientCertificate
validateDomain (Just encodedCertificate) unparsedDomain = do
  targetDomain <- parseDomain unparsedDomain

  -- run discovery to find the hostname of the client federator
  certificate <-
    mapError (CertificateParseError . Text.pack) $
      decodeCertificate encodedCertificate
  hostnames <- srvTargetDomain <$$> discoverAllFederatorsWithError targetDomain
  let validationErrors = (\h -> validateDomainName (B8.unpack h) certificate) <$> hostnames
  unless (any null validationErrors) $
    throw $ AuthenticationFailure validationErrors

  ensureCanFederateWith targetDomain $> targetDomain

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
