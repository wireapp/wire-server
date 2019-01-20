-- | HTTP authentication support. Can be used with basic auth, token-based
-- auth, or something else (though OAuth is likely not implementable with
-- this API).
module Web.Scim.Class.Auth
    ( AuthDB (..)
    ) where

import Servant
import Web.Scim.Handler

-- | An interface that has to be implemented for a server to provide
-- authentication.
class FromHttpApiData (AuthData m) => AuthDB m where
  -- | The type that the “Authorization” header will be parsed as. This info
  -- will be given to 'AuthCheck'.
  type AuthData m

  -- | The result of performing authentication.
  --
  -- Can be '()' to handle just authorized/non-authorized, or something more
  -- complex – for instance, if the auth header provides a token that may or
  -- may not correspond to a particular organization, then the result could
  -- be the ID of that organization).
  type AuthInfo m

  -- | Do authentication or throw an error in `ScimHandler` (e.g.
  -- 'Web.Scim.Schema.Error.unauthorized') if the provided credentials are
  -- invalid or don't correspond to any user.
  authCheck
      :: Maybe (AuthData m)
      -> ScimHandler m (AuthInfo m)
