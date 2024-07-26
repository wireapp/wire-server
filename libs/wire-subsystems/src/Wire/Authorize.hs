-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2024 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.Authorize
  ( Authorized (runAuthorized),
    authorize,
    AuthorizeUpdateEmail,
  )
where

import Data.Id
import Data.Kind
import Imports
import Wire.API.User.Auth

-- | Simple access control (authorization).  Allows to define functions that take arguments
-- wrapped in `Authorized`.  `Authorized` is abstract, so the only way to obtain such a value
-- is by calling one of the `Authorize` instances (see below).
--
-- This way, the application logic is not cluttered with access control code, but it is
-- guaranteed that authorization is enforced.
--
-- This is inspired by [lio](https://hackage.haskell.org/package/lio), but only controls
-- access, not information flow.
--
-- TODO: Make this a proper subsystem and use polysemy for error handling rather than either?
-- TODO: put every instance X of Authorize into a module Wire.Authorize.*.  all those modules should re-export authorize.
data Authorized op val = Authorized {runAuthorized :: val}
  deriving (Eq, Show)

-- | Different flavours of `Authorized`s.
class Authorize op where
  type AuthorizeCreds op :: Type
  type AuthorizeResult op :: Type
  type AuthorizeError op :: Type
  authorize :: AuthorizeCreds op -> Either (AuthorizeError op) (Authorized op (AuthorizeResult op))

data AuthorizeUpdateEmail

instance Authorize AuthorizeUpdateEmail where
  type AuthorizeCreds AuthorizeUpdateEmail = ([Either Text SomeUserToken], Maybe (Either Text SomeAccessToken))
  type AuthorizeResult AuthorizeUpdateEmail = UserId
  type AuthorizeError AuthorizeUpdateEmail = () -- TODO

  authorize =
    undefined Authorized
