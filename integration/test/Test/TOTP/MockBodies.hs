-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Test.TOTP.MockBodies where

import API.Common (defPassword)
import Testlib.Prelude

mockEnrolBody :: Value
mockEnrolBody =
  object
    [ "name" .= ("work phone" :: String),
      "client_time" .= ("2026-02-03T11:17:05.021Z" :: String),
      "password" .= defPassword
    ]

mockActivateBody :: Value
mockActivateBody =
  object
    [ "code" .= ("014921" :: String),
      "client_time" .= ("2026-02-03T11:18:44.903Z" :: String)
    ]

mockRenameBody :: Value
mockRenameBody = object ["name" .= ("private phone" :: String)]

mockDeleteBody :: Value
mockDeleteBody = object ["password" .= defPassword]

mockLoginTotpCode :: String
mockLoginTotpCode = "totp:014921"
