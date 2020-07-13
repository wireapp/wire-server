{-# LANGUAGE AllowAmbiguousTypes #-}

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

-- | HTTP authentication support. Can be used with basic auth, token-based
-- auth, or something else (though OAuth is likely not implementable with
-- this API).
module Web.Scim.Class.Auth
  ( AuthTypes (..),
    AuthDB (..),
  )
where

import Servant
import Web.Scim.Handler

-- | Types used in authentication routines.
class AuthTypes tag where
  -- | The type that the “Authorization” header will be parsed as. This info
  -- will be given to 'authCheck'.
  type AuthData tag

  -- | The result of performing authentication.
  --
  -- Can be '()' to handle just authorized/non-authorized, or something more
  -- complex – for instance, if the auth header provides a token that may or
  -- may not correspond to a particular organization, then the result could
  -- be the ID of that organization).
  type AuthInfo tag

-- | An interface that has to be implemented for a server to provide
-- authentication.
class (AuthTypes tag, FromHttpApiData (AuthData tag)) => AuthDB tag m where
  -- | Do authentication or throw an error in `ScimHandler` (e.g.
  -- 'Web.Scim.Schema.Error.unauthorized') if the provided credentials are
  -- invalid or don't correspond to any user.
  authCheck ::
    Maybe (AuthData tag) ->
    ScimHandler m (AuthInfo tag)
