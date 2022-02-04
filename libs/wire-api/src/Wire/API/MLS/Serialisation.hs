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

module Wire.API.MLS.Serialisation where

import Data.Binary
import Imports

-- | Parse a value encoded using the "TLS presentation" format.
class ParseMLS a where
  parseMLS :: Get a

instance ParseMLS ByteString where
  parseMLS = get

-- | A wrapper to generate a 'ParseMLS' instance given a 'Binary' instance.
newtype BinaryMLS a = BinaryMLS a

instance Binary a => ParseMLS (BinaryMLS a) where
  parseMLS = BinaryMLS <$> get

-- | A wrapper to generate a 'Binary' instance for an enumerated type.
newtype EnumBinary w a = EnumBinary {unEnumBinary :: a}

instance (Binary w, Integral w, Enum a) => Binary (EnumBinary w a) where
  get = EnumBinary . toEnum . fromIntegral <$> get @w
  put = put @w . fromIntegral . fromEnum . unEnumBinary
