-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Team.Export (TeamExportUser (..)) where

import Data.ByteString.Conversion (toByteString')
import Data.Csv (DefaultOrdered (..), ToNamedRecord (..), namedRecord)
import Data.Handle (Handle)
import Data.Vector (fromList)
import Imports
import Wire.API.Team.Role (Role)
import Wire.API.User (Name)
import Wire.API.User.Identity (Email)

data TeamExportUser = TeamExportUser
  { tExportDisplayName :: Name,
    tExportUserName :: Maybe Handle,
    tExportEmail :: Email,
    tExportRole :: Role
  }
  deriving (Show, Eq, Generic)

instance ToNamedRecord TeamExportUser where
  toNamedRecord row =
    namedRecord
      [ ("name", toByteString' (tExportDisplayName row)),
        ("username", maybe "" toByteString' (tExportUserName row)),
        ("email", toByteString' (tExportEmail row)),
        ("role", toByteString' (tExportRole row))
      ]

instance DefaultOrdered TeamExportUser where
  headerOrder = const $ fromList ["name", "username", "email", "role"]
