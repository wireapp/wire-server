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

module Spar.Sem.SAML2
  ( SAML2 (..),
    authReq,
    authResp,
    meta,
    toggleCookie,
  )
where

import Data.Id (TeamId)
import Data.List.NonEmpty (NonEmpty)
import Data.Time (NominalDiffTime)
import GHC.TypeLits (KnownSymbol)
import Imports (ByteString, Maybe, Text)
import Polysemy
import SAML2.WebSSO hiding (meta, toggleCookie)
import URI.ByteString (URI)
import Wire.API.User.IdentityProvider (IdP)

data SAML2 m a where
  AuthReq ::
    NominalDiffTime ->
    m Issuer ->
    IdPId ->
    SAML2 m (FormRedirect AuthnRequest)
  AuthResp ::
    Maybe TeamId ->
    m Issuer ->
    m URI ->
    (NonEmpty Assertion -> IdP -> AccessVerdict -> m resp) ->
    AuthnResponseBody ->
    SAML2 m resp
  Meta :: Text -> m Issuer -> m URI -> m [ContactPerson] -> SAML2 m SPMetadata
  ToggleCookie ::
    (KnownSymbol name) =>
    ByteString ->
    Maybe (Text, NominalDiffTime) ->
    SAML2 m (SimpleSetCookie name)

-- TODO(sandy): Inline this definition --- no TH
makeSem ''SAML2
