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

module Galley.Intra.Spar
  ( deleteTeam,
    lookupScimUserInfo,
  )
where

import Bilge
import Data.ByteString.Conversion
import Data.Id
import Galley.Intra.Util
import Galley.Monad
import Imports
import Network.HTTP.Types.Method
import Wire.API.User (ScimUserInfo)

-- | Notify Spar that a team is being deleted.
deleteTeam :: TeamId -> App ()
deleteTeam tid = do
  void . call Spar $
    method DELETE
      . paths ["i", "teams", toByteString' tid]
      . expect2xx

-- | Get the SCIM user info for a user.
lookupScimUserInfo :: UserId -> App ScimUserInfo
lookupScimUserInfo uid = do
  response <-
    call Spar $
      method POST
        . paths ["i", "scim", "userinfo", toByteString' uid]
  responseJsonError response
