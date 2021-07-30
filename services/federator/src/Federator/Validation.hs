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
  ( federateWith,
    validateDomain,
    throwInward,
    sanitizePath,
  )
where

import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.Domain (Domain, domainText, mkDomain)
import qualified Data.PEM as X509
import Data.String.Conversions (cs)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.X509 as X509
import qualified Data.X509.Validation as X509
import Federator.Discovery
import Federator.Options
import Imports
import Polysemy (Member, Members, Sem)
import qualified Polysemy.Error as Polysemy
import qualified Polysemy.Reader as Polysemy
import URI.ByteString
import Wire.API.Federation.GRPC.Types
import Wire.Network.DNS.SRV (SrvTarget (..))

-- | Validates an already-parsed domain against the allowList using the federator
-- startup configuration.
federateWith :: Members '[Polysemy.Reader RunSettings] r => Domain -> Sem r Bool
federateWith targetDomain = do
  strategy <- Polysemy.asks federationStrategy
  pure $ case strategy of
    AllowAll -> True
    AllowList (AllowedDomains domains) -> targetDomain `elem` domains

decodeCertificate ::
  Member (Polysemy.Error InwardError) r =>
  ByteString ->
  Sem r X509.Certificate
decodeCertificate =
  Polysemy.fromEither
    . first (InwardError IInvalidDomain . Text.pack)
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

-- | Validates an unknown domain string against the allowList using the
-- federator startup configuration and checks that it matches the names reported
-- by the client certificate
validateDomain ::
  Members
    '[ Polysemy.Reader RunSettings,
       Polysemy.Error InwardError,
       DiscoverFederator
     ]
    r =>
  Maybe ByteString ->
  Text ->
  Sem r Domain
validateDomain Nothing _ = throwInward IInvalidDomain "no client certificate provided"
validateDomain (Just encodedCertificate) unparsedDomain = do
  targetDomain <- case mkDomain unparsedDomain of
    Left parseErr -> throwInward IInvalidDomain (errDomainParsing parseErr)
    Right d -> pure d

  -- TODO: extract this to a separate module
  -- validate the hostname without a trailing dot as the certificate is not
  -- expected to have the trailing dot.
  let stripDot hostname
        | "." `isSuffixOf` hostname = take (length hostname - 1) hostname
        | otherwise = hostname
  let validateName hostname cert =
        X509.hookValidateName X509.defaultHooks (stripDot hostname) cert

  -- run discovery to find the hostname of the client federator
  certificate <- decodeCertificate encodedCertificate
  SrvTarget hostname _ <-
    Polysemy.mapError (InwardError IDiscoveryFailed . errDiscovery) $
      discoverFederatorWithError targetDomain
  let validationErrors = validateName (B8.unpack hostname) certificate
  unless (null validationErrors) $
    throwInward IInvalidDomain ("domain name does not match certificate: " <> Text.pack (show validationErrors))

  passAllowList <- federateWith targetDomain
  if passAllowList
    then pure targetDomain
    else throwInward IFederationDeniedByRemote (errAllowList targetDomain)
  where
    errDomainParsing :: String -> Text
    errDomainParsing err = "Domain parse failure for [" <> unparsedDomain <> "]: " <> cs err

    errAllowList :: Domain -> Text
    errAllowList domain = "Origin domain [" <> domainText domain <> "] not in the federation allow list"

    errDiscovery :: LookupError -> Text
    errDiscovery (LookupErrorSrvNotAvailable msg) = "srv record not found: " <> Text.decodeUtf8 msg
    errDiscovery (LookupErrorDNSError msg) = "DNS error: " <> Text.decodeUtf8 msg

throwInward :: Members '[Polysemy.Error InwardError] r => InwardErrorType -> Text -> Sem r a
throwInward errType errMsg = Polysemy.throw $ InwardError errType errMsg

-- | Normalize the path, and ensure the path begins with "federation/" after normalization
sanitizePath :: Members '[Polysemy.Error InwardError] r => ByteString -> Sem r ByteString
sanitizePath originalPath = do
  when (BS.length originalPath > 200) $
    throwInward IForbiddenEndpoint "path too long"
  -- we parse the path using the URI.ByteString module to make use of its normalization functions
  uriRef <- case parseRelativeRef strictURIParserOptions originalPath of
    Left err -> throwInward IForbiddenEndpoint (cs $ show err <> cs originalPath)
    Right ref -> pure ref

  -- we don't expect any query parameters or other URL parts other than a plain path
  when (queryPairs (rrQuery uriRef) /= []) $
    throwInward IForbiddenEndpoint "query parameters not allowed"
  when (isJust (rrFragment uriRef)) $
    throwInward IForbiddenEndpoint "fragments not allowed"
  when (isJust (rrAuthority uriRef)) $
    throwInward IForbiddenEndpoint "authority not allowed"

  -- Perform these normalizations:
  -- - hTtP -> http
  -- - eXaMpLe.org -> example.org
  -- - If the scheme is known and the port is the default (e.g. 80 for http) it is removed.
  -- - If the path is empty, set it to /
  -- - Rewrite path from /foo//bar///baz to /foo/bar/baz
  -- - Sorts parameters by parameter name
  -- - Remove dot segments as per RFC3986 Section 5.2.4
  let normalizedURI = normalizeURIRef' aggressiveNormalization uriRef
  -- sometimes normalization above results in a path with a leading slash.
  -- For consistency, remove a leading slash, if any
  let withoutLeadingSlash = BS.stripPrefix "/" normalizedURI
  let normalized = fromMaybe normalizedURI withoutLeadingSlash

  -- to guard against double percent encoding, we disallow the '%' character
  -- here, since we expect to only use POST requests on ASCII paths without any
  -- query parameters
  let (_, pTailEncoding) = BS.breakSubstring "%" normalized
  unless (BS.null pTailEncoding) $
    throwInward IForbiddenEndpoint "percent encoding not allowed"

  -- Most importantly, only allow paths with the federation/ prefix.
  unless ("federation/" `BS.isPrefixOf` normalized) $
    throwInward IForbiddenEndpoint ("disallowed path: " <> cs originalPath)

  pure normalized
