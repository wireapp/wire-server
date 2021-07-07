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
  )
where

import Control.Lens (view)
import Data.Domain (Domain, domainText, mkDomain)
import Data.String.Conversions (cs)
import Federator.Options
import Imports
import Polysemy (Members, Sem)
import qualified Polysemy.Error as Polysemy
import qualified Polysemy.Reader as Polysemy
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
