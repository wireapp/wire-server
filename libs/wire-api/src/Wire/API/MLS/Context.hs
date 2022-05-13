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

module Wire.API.MLS.Context where

import Imports

-- Warning: the "context" string here is different from the one mandated by
-- the spec, but it is the one that happens to be used by openmls. Until
-- openmls is patched and we switch to a fixed version, we will have to use
-- the "wrong" string here as well.
--
-- This is used when invoking 'csHash'.
context :: ByteString
context = "MLS 1.0 ref"

proposalContext, keyPackageContext :: ByteString
proposalContext = context -- TODO: see if this is how it is defined in openmls
keyPackageContext = context
