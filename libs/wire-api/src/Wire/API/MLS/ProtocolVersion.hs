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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.MLS.ProtocolVersion
  ( ProtocolVersion (..),
    ProtocolVersionTag (..),
    pvTag,
    protocolVersionFromTag,
    defaultProtocolVersion,
  )
where

import Data.Binary
import Imports
import Wire.API.MLS.Serialisation
import Wire.Arbitrary

-- | https://messaginglayersecurity.rocks/mls-protocol/draft-ietf-mls-protocol-20/draft-ietf-mls-protocol.html#section-6-4
newtype ProtocolVersion = ProtocolVersion {pvNumber :: Word16}
  deriving newtype (Eq, Ord, Show, Binary, Arbitrary, ParseMLS, SerialiseMLS)

data ProtocolVersionTag = ProtocolMLS10 | ProtocolMLSDraft11
  deriving stock (Bounded, Enum, Eq, Show, Generic)
  deriving (Arbitrary) via GenericUniform ProtocolVersionTag

pvTag :: ProtocolVersion -> Maybe ProtocolVersionTag
pvTag (ProtocolVersion v) = case v of
  1 -> pure ProtocolMLS10
  -- used by openmls
  200 -> pure ProtocolMLSDraft11
  _ -> Nothing

protocolVersionFromTag :: ProtocolVersionTag -> ProtocolVersion
protocolVersionFromTag ProtocolMLS10 = ProtocolVersion 1
protocolVersionFromTag ProtocolMLSDraft11 = ProtocolVersion 200

defaultProtocolVersion :: ProtocolVersion
defaultProtocolVersion = protocolVersionFromTag ProtocolMLS10
