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

import Control.Lens (view)
import Data.Domain (Domain, domainText, mkDomain)
import Data.String.Conversions (cs)
import Federator.Options
import Imports
import Network.URI
import Polysemy (Members, Sem)
import qualified Polysemy.Error as Polysemy
import qualified Polysemy.Reader as Polysemy
import qualified URI.ByteString as URIB
import Wire.API.Federation.GRPC.Types

-- | Validates an already-parsed domain against the allowList using the federator
-- startup configuration.
federateWith :: Members '[Polysemy.Reader RunSettings] r => Domain -> Sem r Bool
federateWith targetDomain = do
  strategy <- view federationStrategy <$> Polysemy.ask
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
  -- we parse the path using the URI.ByteString so we can make use of that library's normalization functions
  uriRef <- case URIB.parseRelativeRef URIB.strictURIParserOptions originalPath of
    Left err -> throwInward IInvalidEndpoint (cs $ show err)
    Right ref -> pure ref
  -- Perform these normalizations:
  -- - hTtP -> http
  -- - eXaMpLe.org -> example.org
  -- - If the scheme is known and the port is the default (e.g. 80 for http) it is removed.
  -- - If the path is empty, set it to /
  -- - Rewrite path from /foo//bar///baz to /foo/bar/baz
  -- - Sorts parameters by parameter name
  -- - Remove dot segments as per RFC3986 Section 5.2.4
  let normalized = URIB.normalizeURIRef' URIB.aggressiveNormalization uriRef
  -- next, we use the Network.URI library to get path segments after normalization,
  -- and ensure the first segment in the path is "federation"
  uri <- case parseURIReference (cs normalized) of
    -- The Nothing case is unlikely to happen here as it's already parsed by URI.bytestring at this point
    Nothing -> throwInward IInvalidEndpoint (cs $ "cannot parse " <> normalized)
    Just u -> pure u
  case pathSegments uri of
    ["federation"] -> throwInward IForbiddenEndpoint $ cs ("disallowed path: " <> uriPath uri)
    "federation" : _ -> pure (cs (uriPath uri))
    _ -> throwInward IForbiddenEndpoint $ cs ("disallowed path: " <> uriPath uri)
