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

module Spar.Sem.IdP where

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

data IdConfigStore m a where
  StoreConfig :: IP.IdP -> IdConfigStore m ()
  GetConfig :: SAML.IdPId -> IdConfigStore m (Maybe IP.IdP)
  GetIdByIssuerWithoutTeam :: SAML.Issuer -> IdConfigStore m (GetIdPResult SAML.IdPId)
  GetIdByIssuerWithTeam :: SAML.Issuer -> TeamId -> IdConfigStore m (Maybe SAML.IdPId)
  GetConfigsByTeam :: TeamId -> IdConfigStore m [IP.IdP]
  DeleteConfig :: IP.IdP -> IdConfigStore m ()
  -- affects _wiReplacedBy in GetConfig
  SetReplacedBy :: Replaced -> Replacing -> IdConfigStore m ()
  ClearReplacedBy :: Replaced -> IdConfigStore m ()

deriving stock instance Show (IdConfigStore m a)

-- TODO(sandy): Inline this definition --- no TH
makeSem ''IdConfigStore
deriveGenericK ''IdConfigStore
