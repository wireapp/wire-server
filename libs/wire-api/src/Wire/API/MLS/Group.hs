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

module Wire.API.MLS.Group where

import Data.Schema
import qualified Data.Swagger as S
import Imports
import Wire.API.MLS.Serialisation

newtype GroupId = GroupId {unGroupId :: ByteString}
  deriving (Eq, Show)
  deriving (S.ToSchema) via Schema GroupId

-- TODO(md): This instance is defined in PR #2150 that is yet to be merged as of
-- March 15, 2022.
instance ToSchema GroupId where
  schema = undefined

instance IsString GroupId where
  fromString = GroupId . fromString

instance ParseMLS GroupId where
  parseMLS = GroupId <$> parseMLSBytes @Word8
