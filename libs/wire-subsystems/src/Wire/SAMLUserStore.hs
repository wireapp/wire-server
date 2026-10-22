{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.SAMLUserStore
  ( SAMLUserStore (..),
    insert,
    get,
    deleteByIssuer,
    delete,
    getAllByIssuer,
  )
where

import Data.Id
import Imports
import Polysemy
import SAML2.WebSSO qualified as SAML

data SAMLUserStore m a where
  Insert :: SAML.UserRef -> UserId -> SAMLUserStore m ()
  Get :: SAML.UserRef -> SAMLUserStore m (Maybe UserId)
  DeleteByIssuer :: SAML.Issuer -> SAMLUserStore m ()
  Delete :: UserId -> SAML.UserRef -> SAMLUserStore m ()
  -- | All users bound to the given issuer. This replaces the former
  -- @Cas.Page@-leaking @GetAllByIssuerPaginated@\/@NextPage@ pair: the only
  -- consumer walks every page anyway, so the interpreters drain the pages
  -- internally and the effect stays first-order (implementable by Hasql).
  GetAllByIssuer :: SAML.Issuer -> SAMLUserStore m [(SAML.UserRef, UserId)]

-- TODO(sandy): Inline this definition --- no TH
makeSem ''SAMLUserStore
