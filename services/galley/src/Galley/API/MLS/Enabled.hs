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

module Galley.API.MLS.Enabled
  ( isMLSEnabled,
    assertMLSEnabled,
  )
where

import Control.Lens (view)
import Data.Monoid
import Galley.Env
import Imports hiding (getFirst)
import Polysemy
import Polysemy.Input
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.MLS.Credential
import Wire.API.MLS.Keys

isMLSEnabled :: Member (Input Env) r => Sem r Bool
isMLSEnabled = do
  keys <- inputs (view mlsKeys) <*> pure RemovalPurpose
  pure $
    isJust (getFirst (mlsKeyPair_ed25519 keys))
      && isJust (getFirst (mlsKeyPair_ecdsa_secp256r1_sha256 keys))
      && isJust (getFirst (mlsKeyPair_ecdsa_secp384r1_sha384 keys))
      && isJust (getFirst (mlsKeyPair_ecdsa_secp521r1_sha512 keys))

-- | Fail if MLS is not enabled. Only use this function at the beginning of an
-- MLS endpoint, NOT in utility functions.
assertMLSEnabled ::
  ( Member (Input Env) r,
    Member (ErrorS 'MLSNotEnabled) r
  ) =>
  Sem r ()
assertMLSEnabled =
  unlessM isMLSEnabled $
    throwS @'MLSNotEnabled
