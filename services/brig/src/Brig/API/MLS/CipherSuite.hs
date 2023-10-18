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

module Brig.API.MLS.CipherSuite (getCipherSuite, getCipherSuites) where

import Brig.API.Handler
import Brig.API.MLS.KeyPackages.Validation
import Imports
import Wire.API.MLS.CipherSuite

getOneCipherSuite :: CipherSuite -> Handler r CipherSuiteTag
getOneCipherSuite s =
  maybe
    (mlsProtocolError "Unknown ciphersuite")
    pure
    (cipherSuiteTag s)

getCipherSuite :: Maybe CipherSuite -> Handler r CipherSuiteTag
getCipherSuite = maybe (pure defCipherSuite) getOneCipherSuite

getCipherSuites :: Maybe [CipherSuite] -> Handler r [CipherSuiteTag]
getCipherSuites = maybe (pure [defCipherSuite]) (traverse getOneCipherSuite)
