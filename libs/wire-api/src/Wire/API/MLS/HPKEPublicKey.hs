{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module Wire.API.MLS.HPKEPublicKey where

import Imports
import Test.QuickCheck
import Wire.API.MLS.Serialisation

-- | https://messaginglayersecurity.rocks/mls-protocol/draft-ietf-mls-protocol-20/draft-ietf-mls-protocol.html#section-5.1.1-2
newtype HPKEPublicKey = HPKEPublicKey {unHPKEPublicKey :: ByteString}
  deriving (Show, Eq, Arbitrary)

instance ParseMLS HPKEPublicKey where
  parseMLS = HPKEPublicKey <$> parseMLSBytes @VarInt

instance SerialiseMLS HPKEPublicKey where
  serialiseMLS = serialiseMLSBytes @VarInt . unHPKEPublicKey
