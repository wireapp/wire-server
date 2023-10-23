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

module Wire.API.MLS.Capabilities where

import Imports
import Test.QuickCheck
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Serialisation
import Wire.Arbitrary

-- | https://messaginglayersecurity.rocks/mls-protocol/draft-ietf-mls-protocol-20/draft-ietf-mls-protocol.html#section-7.2-2
data Capabilities = Capabilities
  { versions :: [Word16],
    ciphersuites :: [CipherSuite],
    extensions :: [Word16],
    proposals :: [Word16],
    credentials :: [Word16]
  }
  deriving (Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform Capabilities)

instance ParseMLS Capabilities where
  parseMLS =
    Capabilities
      <$> parseMLSVector @VarInt parseMLS
      <*> parseMLSVector @VarInt parseMLS
      <*> parseMLSVector @VarInt parseMLS
      <*> parseMLSVector @VarInt parseMLS
      <*> parseMLSVector @VarInt parseMLS

instance SerialiseMLS Capabilities where
  serialiseMLS caps = do
    serialiseMLSVector @VarInt serialiseMLS caps.versions
    serialiseMLSVector @VarInt serialiseMLS caps.ciphersuites
    serialiseMLSVector @VarInt serialiseMLS caps.extensions
    serialiseMLSVector @VarInt serialiseMLS caps.proposals
    serialiseMLSVector @VarInt serialiseMLS caps.credentials
