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

module Brig.API.MLS.CipherSuite (getCipherSuite) where

import Brig.API.Handler
import Brig.API.MLS.KeyPackages.Validation
import Imports
import Wire.API.MLS.CipherSuite

getCipherSuite :: Maybe CipherSuite -> Handler r CipherSuiteTag
getCipherSuite mSuite = case mSuite of
  Nothing -> pure defCipherSuite
  Just x ->
    maybe (mlsProtocolError "Unknown ciphersuite") pure (cipherSuiteTag x)
