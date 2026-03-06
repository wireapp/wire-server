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

module Wire.IdPConfigStore
  ( IdPConfigStore (..),
    Replacing (..),
    Replaced (..),
    insertConfig,
    getConfig,
    getIdPByIssuerV1,
    getIdPByIssuerV1Maybe,
    getIdPByIssuerV2,
    getIdPByIssuerV2Maybe,
    getConfigsByTeam,
    deleteConfig,
    setReplacedBy,
    clearReplacedBy,
    deleteIssuer,
    newHandle,
    IdpDbError (..),
  )
where

import Data.Id
import Imports
import Polysemy
import SAML2.WebSSO qualified as SAML
import Wire.API.User.IdentityProvider qualified as IP

newtype Replaced = Replaced SAML.IdPId
  deriving (Eq, Ord, Show)

newtype Replacing = Replacing SAML.IdPId
  deriving (Eq, Ord, Show)

data IdPConfigStore m a where
  InsertConfig :: IP.IdP -> IdPConfigStore m ()
  NewHandle :: TeamId -> IdPConfigStore m IP.IdPHandle
  GetConfig :: SAML.IdPId -> IdPConfigStore m IP.IdP
  GetIdPByIssuerV1Maybe :: SAML.Issuer -> IdPConfigStore m (Maybe IP.IdP)
  GetIdPByIssuerV1 :: SAML.Issuer -> IdPConfigStore m IP.IdP
  GetIdPByIssuerV2Maybe :: SAML.Issuer -> TeamId -> IdPConfigStore m (Maybe IP.IdP)
  GetIdPByIssuerV2 :: SAML.Issuer -> TeamId -> IdPConfigStore m IP.IdP
  GetConfigsByTeam :: TeamId -> IdPConfigStore m [IP.IdP]
  DeleteConfig :: IP.IdP -> IdPConfigStore m ()
  -- affects _wiReplacedBy in GetConfig
  SetReplacedBy :: Replaced -> Replacing -> IdPConfigStore m ()
  ClearReplacedBy :: Replaced -> IdPConfigStore m ()
  DeleteIssuer :: SAML.Issuer -> Maybe TeamId -> IdPConfigStore m ()

deriving stock instance Show (IdPConfigStore m a)

makeSem ''IdPConfigStore

data IdpDbError
  = InsertIdPConfigCannotMixApiVersions
  | AttemptToGetV1IssuerViaV2API
  | AttemptToGetV2IssuerViaV1API
  | IdpNonUnique
  | IdpWrongTeam
  | IdpNotFound -- like 'SparIdPNotFound', but a database consistency error.  (should we consolidate something anyway?)
  deriving (Eq, Show)
