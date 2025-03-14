{-# LANGUAGE TemplateHaskell #-}

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

module Spar.Sem.SamlProtocolSettings
  ( SamlProtocolSettings (..),
    spIssuer,
    responseURI,
    contactPersons,
  )
where

import Data.Domain
import Data.Id (TeamId)
import Imports
import Polysemy
import qualified SAML2.WebSSO.Types as SAML
import qualified URI.ByteString as URI

data SamlProtocolSettings m a where
  SpIssuer :: Maybe TeamId -> Maybe Domain -> SamlProtocolSettings m (Maybe SAML.Issuer)
  ResponseURI :: Maybe TeamId -> Maybe Domain -> SamlProtocolSettings m (Maybe URI.URI)
  ContactPersons :: Maybe Domain -> SamlProtocolSettings m ([SAML.ContactPerson])

makeSem ''SamlProtocolSettings
