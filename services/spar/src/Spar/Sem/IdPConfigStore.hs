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

module Spar.Sem.IdPConfigStore
  ( IdPConfigStore (..),
    Replacing (..),
    Replaced (..),
    GetIdPResult (..),
    storeConfig,
    getConfig,
    getIdByIssuerWithoutTeam,
    getIdByIssuerWithTeam,
    getConfigsByTeam,
    deleteConfig,
    setReplacedBy,
    clearReplacedBy,
  )
where

import Data.Id
import Imports
import Polysemy
import Polysemy.Check (deriveGenericK)
import qualified SAML2.WebSSO as SAML
import qualified Wire.API.User.IdentityProvider as IP

data GetIdPResult a
  = GetIdPFound a
  | GetIdPNotFound
  | -- | IdPId has been found, but no IdPConfig matching that Id.  (Database
    --   inconsistency or race condition.)
    GetIdPDanglingId SAML.IdPId
  | -- | You were looking for an idp by just providing issuer, not teamid, and `issuer_idp_v2`
    --   has more than one entry (for different teams).
    GetIdPNonUnique [SAML.IdPId]
  | -- | An IdP was found, but it lives in another team than the one you were looking for.
    --   This should be handled similarly to NotFound in most cases.
    GetIdPWrongTeam SAML.IdPId
  deriving (Eq, Show, Generic)

newtype Replaced = Replaced SAML.IdPId
  deriving (Eq, Ord, Show)

newtype Replacing = Replacing SAML.IdPId
  deriving (Eq, Ord, Show)

data IdPConfigStore m a where
  StoreConfig :: IP.IdP -> IdPConfigStore m ()
  GetConfig :: SAML.IdPId -> IdPConfigStore m (Maybe IP.IdP)
  GetIdByIssuerWithoutTeam :: SAML.Issuer -> IdPConfigStore m (GetIdPResult SAML.IdPId)
  GetIdByIssuerWithTeam :: SAML.Issuer -> TeamId -> IdPConfigStore m (Maybe SAML.IdPId)
  GetConfigsByTeam :: TeamId -> IdPConfigStore m [IP.IdP]
  DeleteConfig :: IP.IdP -> IdPConfigStore m ()
  -- affects _wiReplacedBy in GetConfig
  SetReplacedBy :: Replaced -> Replacing -> IdPConfigStore m ()
  ClearReplacedBy :: Replaced -> IdPConfigStore m ()

deriving stock instance Show (IdPConfigStore m a)

-- TODO(sandy): Inline this definition --- no TH
makeSem ''IdPConfigStore
deriveGenericK ''IdPConfigStore
