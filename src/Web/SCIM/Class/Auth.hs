{-# LANGUAGE TypeFamilies #-}

-- | Basic HTTP authentication support. See See
-- https://haskell-servant.readthedocs.io/en/stable/cookbook/jwt-and-basic-auth/JWTAndBasicAuth.html
module Web.SCIM.Class.Auth
    ( Admin (..)
    , AuthDB (..)
    ) where

import Data.Aeson
import GHC.Generics
import Servant.Auth.Server
import Data.UUID

-- | Someone who is allowed to provision users via SCIM.
data Admin = Admin
  { adminId :: UUID
  } deriving (Eq, Show, Read, Generic)

instance ToJSON Admin
instance ToJWT Admin
instance FromJSON Admin
instance FromJWT Admin

-- | Unfortunately, we have to pass an "authentication callback" to Servant
-- by instantiating a zero-argument type family. This can only be done once
-- per application, which is not the best possible design.
type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult Admin)

instance FromBasicAuthData Admin where
  fromBasicAuthData auth check = check auth

-- | An interface that has to be implemented for a server to provide
-- authentication.
class AuthDB m where
  -- | Check whether a set of credentials (login and password) corresponds
  -- to any 'Admin'.
  --
  -- We can only do IO in 'fromBasicAuthData', so this has to return an IO
  -- action.
  mkAuthChecker :: m (BasicAuthData -> IO (AuthResult Admin))
