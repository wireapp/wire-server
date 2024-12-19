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

module Brig.API.MLS.CipherSuite (getCipherSuite, validateCipherSuites) where

import Brig.API.Handler
import Brig.API.MLS.KeyPackages.Validation
import Data.Set qualified as Set
import Imports
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Serialisation

getOneCipherSuite :: CipherSuite -> Handler r CipherSuiteTag
getOneCipherSuite s =
  maybe
    (mlsProtocolError "Unknown ciphersuite")
    pure
    (cipherSuiteTag s)

getCipherSuite :: Maybe CipherSuite -> Handler r CipherSuiteTag
getCipherSuite = maybe (pure legacyCipherSuite) getOneCipherSuite

validateCipherSuites ::
  Maybe [CipherSuite] ->
  KeyPackageUpload ->
  Handler r (Set CipherSuiteTag)
validateCipherSuites suites upload = do
  suitesQuery <- Set.fromList <$> maybe (pure [legacyCipherSuite]) (traverse getOneCipherSuite) suites
  when (any isNothing suitesKPM) . void $ mlsProtocolError "uploaded key packages contains unsupported cipher suite"
  unless (suitesQuery == suitesKP) . void $ mlsProtocolError "uploaded key packages for unannounced cipher suites"
  pure suitesQuery
  where
    suitesKPM = map (cipherSuiteTag . (.cipherSuite) . value) upload.keyPackages
    suitesKP = Set.fromList $ catMaybes suitesKPM
