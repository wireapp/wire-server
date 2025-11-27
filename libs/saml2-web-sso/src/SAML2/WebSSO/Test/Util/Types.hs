-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module SAML2.WebSSO.Test.Util.Types where

import Control.Concurrent.MVar
import Control.Lens
import Data.X509 as X509
import SAML2.WebSSO
import SAML2.WebSSO.API.Example
import Text.XML.DSig as SAML

type CtxV = MVar Ctx

data Ctx = Ctx
  { _ctxNow :: Time,
    _ctxConfig :: Config,
    _ctxIdPs :: [(IdPConfig_, SampleIdP)],
    _ctxAssertionStore :: AssertionStore,
    _ctxRequestStore :: RequestStore
  }
  deriving (Eq, Show)

data SampleIdP = SampleIdP
  { sampleIdPMetadata :: IdPMetadata,
    sampleIdPSignPrivCreds :: SignPrivCreds,
    sampleIdPSignCreds :: SignCreds,
    sampleIdPSignedCertificate :: SignedCertificate
  }
  deriving (Eq, Show)

makeLenses ''Ctx
