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

module Wire.API.RawJson where

import Imports
import Servant

-- | Wrap json content as plain 'LByteString'
-- This type is intented to be used to receive json content as 'LByteString'.
-- Warning: There is no validation of the json content. It may be any string.
newtype RawJson = RawJson {rawJsonBytes :: LByteString}

instance {-# OVERLAPPING #-} MimeUnrender JSON RawJson where
  mimeUnrender _ = pure . RawJson
