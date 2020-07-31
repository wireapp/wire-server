{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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

-- TODO remove (orphans can be avoided by only implementing functions here, and gathering them
-- in the instance near the Spar type; alternatively, @hscim@ could be changed)

-- | An implementation of the SCIM API for doing bulk operations with users.
--
-- See <https://en.wikipedia.org/wiki/System_for_Cross-domain_Identity_Management>
--
-- = SCIM user creation flow
--
-- When a user is created via SCIM, a SAML user identity has to be created with it. Currently
-- we don't allow SCIM users without SAML user identities.
--
-- Creating these two user identities (SCIM and SAML) together requires constructing a
-- 'UserRef' from the SCIM request, which is then stored by 'Spar.Data.insertUser'.
--
-- The 'UserRef' consists of:
--
--   * tenant (the url-shaped ID the IdP assigns to itself);
--
--   * subject (usually an email, or an unstructured nickname, or a few more obscure
--     alternatives).
--
-- /Tenant:/ if there is only one IdP for the current team, the tenant can be found by calling
-- 'getIdPConfigsByTeam' and looking up @^. idpMetadata . edIssuer@ on the result. If there is
-- more than one IdP, we need a way to associate user creation requests with specific IdPs.
-- Currently we disallow teams with more than one IdP.
--
-- /Subject:/ there are different reasonable ways to pick a subject for a user; this should be
-- configurable in the team settings page (e.g. a choice of one field from the SCIM user
-- schema, optionally transformed with one of a few hard-coded functions). A simple default
-- could be "take the email address, and type it as an email address", or in saml2-web-sso
-- pseudo-code: @\email -> entityNameID (parseURI ("email:" <> renderEmail email))@.
module Spar.Scim
  ( -- * Reexports
    module Spar.Scim.Types,
    module Spar.Scim.Auth,
    module Spar.Scim.User,

    -- * API implementation
    apiScim,
  )
where

import Control.Lens
import Control.Monad.Catch (try)
import Control.Monad.Except
import Data.String.Conversions (cs)
import Imports
import qualified SAML2.WebSSO as SAML
import Servant
import Servant.API.Generic
import Servant.Server.Generic (AsServerT)
import Spar.App (Env (..), Spar (..))
import Spar.Error
  ( SparCustomError (SparScimError),
    SparError,
    sparToServerErrorWithLogging,
    throwSpar,
  )
import Spar.Scim.Auth
import Spar.Scim.Types
import Spar.Scim.User
import qualified Web.Scim.Capabilities.MetaSchema as Scim.Meta
import qualified Web.Scim.Class.Auth as Scim.Auth
import qualified Web.Scim.Class.User as Scim.User
import qualified Web.Scim.Handler as Scim
import qualified Web.Scim.Schema.Error as Scim
import qualified Web.Scim.Server as Scim

-- | SCIM config for our server.
--
-- TODO: the 'Scim.Meta.empty' configuration claims that we don't support filters, but we
-- actually do; it's a bug in hscim
configuration :: Scim.Meta.Configuration
configuration = Scim.Meta.empty

apiScim :: ServerT APIScim Spar
apiScim =
  hoistScim (toServant (server configuration))
    :<|> apiScimToken
  where
    hoistScim =
      hoistServer
        (Proxy @(ScimSiteAPI SparTag))
        (wrapScimErrors . Scim.fromScimHandler (throwSpar . SparScimError))
    -- Wrap /all/ errors into the format required by SCIM, even server exceptions that have
    -- nothing to do with SCIM.
    --
    -- FIXME: this doesn't catch impure exceptions (e.g. thrown with 'error').
    -- Let's hope that SCIM clients can handle non-SCIM-formatted errors
    -- properly. See <https://github.com/haskell-servant/servant/issues/1022>
    -- for why it's hard to catch impure exceptions.
    wrapScimErrors :: Spar a -> Spar a
    wrapScimErrors = over _Spar $ \act -> \env -> do
      result :: Either SomeException (Either SparError a) <- try (act env)
      case result of
        -- We caught an exception that's not a Spar exception at all. It is wrapped into
        -- Scim.serverError.
        Left someException ->
          pure $
            Left . SAML.CustomError . SparScimError $
              Scim.serverError (cs (displayException someException))
        -- We caught a 'SparScimError' exception. It is left as-is.
        Right err@(Left (SAML.CustomError (SparScimError _))) ->
          pure err
        -- We caught some other Spar exception. It is wrapped into Scim.serverError.
        --
        -- TODO: does it have to be logged?
        Right (Left sparError) -> do
          err <- sparToServerErrorWithLogging (sparCtxLogger env) sparError
          pure $
            Left . SAML.CustomError . SparScimError $
              Scim.serverError (cs (errBody err))
        -- No exceptions! Good.
        Right (Right x) -> pure $ Right x

-- | This is similar to 'Scim.siteServer, but does not include the 'Scim.groupServer',
-- as we don't support it (we don't implement 'Web.Scim.Class.Group.GroupDB').
server ::
  forall tag m.
  (Scim.User.UserDB tag m, Scim.Auth.AuthDB tag m) =>
  Scim.Meta.Configuration ->
  ScimSite tag (AsServerT (Scim.ScimHandler m))
server conf =
  ScimSite
    { config = toServant $ Scim.configServer conf,
      users = \authData -> toServant (Scim.userServer @tag authData)
    }

----------------------------------------------------------------------------
-- Utilities

-- | An isomorphism that unwraps the Spar stack (@Spar . ReaderT . ExceptT@) into a
-- newtype-less form that's easier to work with.
_Spar :: Iso' (Spar a) (Env -> IO (Either SparError a))
_Spar = coerced
