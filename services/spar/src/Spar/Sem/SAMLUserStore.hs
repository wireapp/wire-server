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

module Spar.Sem.SAMLUserStore (
  SAMLUserStore(..),
  insert,
  get,
  getAnyByIssuer,
  getSomeByIssuer,
  deleteByIssuer,
  delete ) where

import Data.Id
import Imports
import Polysemy
import qualified SAML2.WebSSO as SAML

data SAMLUserStore m a where
  Insert :: SAML.UserRef -> UserId -> SAMLUserStore m ()
  Get :: SAML.UserRef -> SAMLUserStore m (Maybe UserId)
  GetAnyByIssuer :: SAML.Issuer -> SAMLUserStore m (Maybe UserId)
  GetSomeByIssuer :: SAML.Issuer -> SAMLUserStore m [(SAML.UserRef, UserId)]
  DeleteByIssuer :: SAML.Issuer -> SAMLUserStore m ()
  Delete :: UserId -> SAML.UserRef -> SAMLUserStore m ()

-- TODO(sandy): Inline this definition --- no TH
makeSem ''SAMLUserStore
