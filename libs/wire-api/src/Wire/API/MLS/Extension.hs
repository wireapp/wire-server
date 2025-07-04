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

module Wire.API.MLS.Extension where

import Data.Binary
import Imports
import Wire.API.MLS.Serialisation
import Wire.Arbitrary

-- | https://messaginglayersecurity.rocks/mls-protocol/draft-ietf-mls-protocol-20/draft-ietf-mls-protocol.html#section-7.2-2
data Extension = Extension
  { extType :: Word16,
    extData :: ByteString
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via GenericUniform Extension

instance ParseMLS Extension where
  parseMLS = Extension <$> parseMLS <*> parseMLSBytes @VarInt

instance SerialiseMLS Extension where
  serialiseMLS (Extension ty d) = do
    serialiseMLS ty
    serialiseMLSBytes @VarInt d

class IsExtension e where
  extensionType :: Word16

findExtension ::
  forall e.
  (ParseMLS e, IsExtension e) =>
  [Extension] ->
  Either Text [e]
findExtension =
  traverse (decodeMLS' . (.extData))
    . filter (\e -> e.extType == extensionType @e)
