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

import Galley.API.MLS.Keys
import Galley.Env
import Imports
import Polysemy
import Polysemy.Input
import Wire.API.Error
import Wire.API.Error.Galley

isMLSEnabled :: Member (Input Env) r => Sem r Bool
isMLSEnabled = isJust <$> getMLSRemovalKey

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
