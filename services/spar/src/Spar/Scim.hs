{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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

-- TODO remove -Wno-orphans (orphans can be avoided by only implementing
-- functions here, and gathering them in the instance near the Spar type;
-- alternatively, @hscim@ could be changed)

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
    module Wire.API.User.Scim,
    module Spar.Scim.Auth,
    module Spar.Scim.User,

    -- * API implementation
    apiScim,
  )
where

import Data.ByteString (toStrict)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text.Encoding.Error
import Imports
import Polysemy
import Polysemy.Error (Error, fromExceptionSem, runError, throw, try)
import Polysemy.Input (Input)
import qualified SAML2.WebSSO as SAML
import Servant
import Servant.Server.Generic (AsServerT)
import Spar.App (sparToServerErrorWithLogging, throwSparSem)
import Spar.Error
  ( SparCustomError (SparScimError),
    SparError,
  )
import Spar.Options
import Spar.Scim.Auth
import Spar.Scim.User
import Spar.Sem.BrigAccess (BrigAccess)
import Spar.Sem.GalleyAccess (GalleyAccess)
import Spar.Sem.IdPConfigStore (IdPConfigStore)
import Spar.Sem.Reporter (Reporter)
import Spar.Sem.SAMLUserStore (SAMLUserStore)
import Spar.Sem.ScimExternalIdStore (ScimExternalIdStore)
import Spar.Sem.ScimTokenStore (ScimTokenStore)
import Spar.Sem.ScimUserTimesStore (ScimUserTimesStore)
import System.Logger (Msg)
import qualified Web.Scim.Capabilities.MetaSchema as Scim.Meta
import qualified Web.Scim.Class.Auth as Scim.Auth
import qualified Web.Scim.Class.User as Scim.User
import qualified Web.Scim.Handler as Scim
import qualified Web.Scim.Schema.Error as Scim
import qualified Web.Scim.Schema.Schema as Scim.Schema
import qualified Web.Scim.Server as Scim
import Wire.API.Routes.Public.Spar
import Wire.API.User.Scim
import Wire.Sem.Logger (Logger)
import Wire.Sem.Now (Now)
import Wire.Sem.Random (Random)

-- | SCIM config for our server.
--
-- TODO: the 'Scim.Meta.empty' configuration claims that we don't support filters, but we
-- actually do; it's a bug in hscim
configuration :: Scim.Meta.Configuration
configuration = Scim.Meta.empty

apiScim ::
  forall r.
  ( Member Random r,
    Member (Input Opts) r,
    Member (Logger (Msg -> Msg)) r,
    Member (Logger String) r,
    Member Now r,
    Member (Error SparError) r,
    Member GalleyAccess r,
    Member BrigAccess r,
    Member ScimExternalIdStore r,
    Member ScimUserTimesStore r,
    Member ScimTokenStore r,
    Member Reporter r,
    Member IdPConfigStore r,
    Member
      ( -- TODO(sandy): Only necessary for 'fromExceptionSem'. But can these errors even happen?
        Final IO
      )
      r,
    Member SAMLUserStore r
  ) =>
  ServerT APIScim (Sem r)
apiScim =
  hoistScim (toServant (server configuration))
    :<|> apiScimToken
  where
    hoistScim =
      hoistServer
        (Proxy @(ScimSiteAPI SparTag))
        (wrapScimErrors . Scim.fromScimHandler (throwSparSem . SparScimError))
    -- Wrap /all/ errors into the format required by SCIM, even server exceptions that have
    -- nothing to do with SCIM.
    --
    -- FIXME: this doesn't catch impure exceptions (e.g. thrown with 'error').
    -- Let's hope that SCIM clients can handle non-SCIM-formatted errors
    -- properly. See <https://github.com/haskell-servant/servant/issues/1022>
    -- for why it's hard to catch impure exceptions.
    wrapScimErrors :: Sem r a -> Sem r a
    wrapScimErrors act = do
      result :: Either SomeException (Either SparError a) <-
        runError $ fromExceptionSem @SomeException $ raise $ try @SparError act
      case result of
        Left someException -> do
          -- We caught an exception that's not a Spar exception at all. It is wrapped into
          -- Scim.serverError.
          throw . SAML.CustomError . SparScimError $
            Scim.serverError (T.pack (displayException someException))
        Right (Left err@(SAML.CustomError (SparScimError _))) ->
          -- We caught a 'SparScimError' exception. It is left as-is.
          throw err
        Right (Left sparError) -> do
          -- We caught some other Spar exception. It is rendered and wrapped into a scim error
          -- with the same status and message, and no scim error type.
          err :: ServerError <- sparToServerErrorWithLogging sparError
          throw . SAML.CustomError . SparScimError $
            Scim.ScimError
              { schemas = [Scim.Schema.Error20],
                status = Scim.Status $ errHTTPCode err,
                scimType = Nothing,
                detail =
                  Just . T.decodeUtf8With lenientDecode . toStrict . errBody $
                    err
              }
        Right (Right x) -> do
          -- No exceptions! Good.
          pure x

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
