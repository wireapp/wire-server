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

import qualified Data.ByteString as BS
import Data.Domain (Domain, domainText, mkDomain)
import Data.String.Conversions (cs)
import Federator.Options
import Imports
import Polysemy (Members, Sem)
import qualified Polysemy.Error as Polysemy
import qualified Polysemy.Reader as Polysemy
import URI.ByteString
import Wire.API.Federation.GRPC.Types

-- | Validates an already-parsed domain against the allowList using the federator
-- startup configuration.
federateWith :: Members '[Polysemy.Reader RunSettings] r => Domain -> Sem r Bool
federateWith targetDomain = do
  strategy <- Polysemy.asks federationStrategy
  pure $ case strategy of
    AllowAll -> True
    AllowList (AllowedDomains domains) -> targetDomain `elem` domains

-- | Validates an unknown domain string against the allowList using the federator startup configuration
validateDomain :: Members '[Polysemy.Reader RunSettings, Polysemy.Error InwardError] r => Text -> Sem r Domain
validateDomain unparsedDomain = do
  targetDomain <- case mkDomain unparsedDomain of
    Left parseErr -> throwInward IInvalidDomain (errDomainParsing parseErr)
    Right d -> pure d
  passAllowList <- federateWith targetDomain
  if passAllowList
    then pure targetDomain
    else throwInward IFederationDeniedByRemote (errAllowList targetDomain)
  where
    errDomainParsing :: String -> Text
    errDomainParsing err = "Domain parse failure for [" <> unparsedDomain <> "]: " <> cs err

    errAllowList :: Domain -> Text
    errAllowList domain = "Origin domain [" <> domainText domain <> "] not in the federation allow list"

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
