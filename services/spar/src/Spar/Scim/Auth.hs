{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

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

-- For @instance AuthDB Spar@

-- | > docs/reference/provisioning/scim-token.md {#RefScimToken}
--
-- Logic for doing authentication in SCIM routes.
--
-- Every time a request to SCIM API is done, we grab a 'ScimToken' from the @"Authorization"@
-- header, check that it's valid, and resolve the team that this operation should apply to.
module Spar.Scim.Auth
  ( apiScimToken,
  )
where

import Control.Lens hiding (Strict, (.=))
import qualified Data.ByteString.Base64 as ES
import Data.Id (ScimTokenId, UserId)
import Data.String.Conversions (cs)
import Imports
-- FUTUREWORK: these imports are not very handy.  split up Spar.Scim into
-- Spar.Scim.{Core,User,Group} to avoid at least some of the hscim name clashes?

import Polysemy
import Polysemy.Error
import Polysemy.Input
import qualified SAML2.WebSSO as SAML
import Servant (NoContent (NoContent), ServerT, (:<|>) ((:<|>)))
import Spar.App (throwSparSem)
import qualified Spar.Error as E
import qualified Spar.Intra.BrigApp as Intra.Brig
import Spar.Options
import Spar.Sem.BrigAccess (BrigAccess)
import qualified Spar.Sem.BrigAccess as BrigAccess
import Spar.Sem.GalleyAccess (GalleyAccess)
import Spar.Sem.IdPConfigStore (IdPConfigStore)
import qualified Spar.Sem.IdPConfigStore as IdPConfigStore
import Spar.Sem.ScimTokenStore (ScimTokenStore)
import qualified Spar.Sem.ScimTokenStore as ScimTokenStore
import qualified Web.Scim.Class.Auth as Scim.Class.Auth
import qualified Web.Scim.Handler as Scim
import qualified Web.Scim.Schema.Error as Scim
import Wire.API.Routes.Public.Spar (APIScimToken)
import Wire.API.User as User
import Wire.API.User.Scim as Api
import Wire.Sem.Now (Now)
import qualified Wire.Sem.Now as Now
import Wire.Sem.Random (Random)
import qualified Wire.Sem.Random as Random

-- | An instance that tells @hscim@ how authentication should be done for SCIM routes.
instance Member ScimTokenStore r => Scim.Class.Auth.AuthDB SparTag (Sem r) where
  -- Validate and resolve a given token
  authCheck :: Maybe ScimToken -> Scim.ScimHandler (Sem r) ScimTokenInfo
  authCheck Nothing =
    Scim.throwScim (Scim.unauthorized "Token not provided")
  authCheck (Just token) =
    maybe (Scim.throwScim (Scim.unauthorized "Invalid token")) pure
      =<< lift (ScimTokenStore.lookup token)

----------------------------------------------------------------------------
-- Token API

-- TODO: don't forget to delete the tokens when the team is deleted

-- | API for manipulating SCIM tokens (protected by normal Wire authentication and available
-- only to team owners).
apiScimToken ::
  Members
    '[ Random,
       Input Opts,
       GalleyAccess,
       BrigAccess,
       ScimTokenStore,
       Now,
       IdPConfigStore,
       Error E.SparError
     ]
    r =>
  ServerT APIScimToken (Sem r)
apiScimToken =
  createScimToken
    :<|> deleteScimToken
    :<|> listScimTokens

-- | > docs/reference/provisioning/scim-token.md {#RefScimTokenCreate}
--
-- Create a token for user's team.
createScimToken ::
  forall r.
  Members
    '[ Random,
       Input Opts,
       GalleyAccess,
       BrigAccess,
       ScimTokenStore,
       IdPConfigStore,
       Now,
       Error E.SparError
     ]
    r =>
  -- | Who is trying to create a token
  Maybe UserId ->
  -- | Request body
  CreateScimToken ->
  Sem r CreateScimTokenResponse
createScimToken zusr Api.CreateScimToken {..} = do
  let descr = createScimTokenDescr
  teamid <- Intra.Brig.authorizeScimTokenManagement zusr
  BrigAccess.ensureReAuthorised zusr createScimTokenPassword createScimTokenCode (Just User.CreateScimToken)
  tokenNumber <- length <$> ScimTokenStore.lookupByTeam teamid
  maxTokens <- inputs maxScimTokens
  unless (tokenNumber < maxTokens) $
    throwSparSem E.SparProvisioningTokenLimitReached
  idps <- IdPConfigStore.getConfigsByTeam teamid

  let caseOneOrNoIdP :: Maybe SAML.IdPId -> Sem r CreateScimTokenResponse
      caseOneOrNoIdP midpid = do
        token <- ScimToken . cs . ES.encode <$> Random.bytes 32
        tokenid <- Random.scimTokenId
        -- FUTUREWORK(fisx): the fact that we're using @Now.get@
        -- here means that the 'Now' effect should not contain
        -- types from saml2-web-sso. We can just use 'UTCTime'
        -- there, right?
        now <- Now.get
        let info =
              ScimTokenInfo
                { stiId = tokenid,
                  stiTeam = teamid,
                  stiCreatedAt = now,
                  stiIdP = midpid,
                  stiDescr = descr
                }
        ScimTokenStore.insert token info
        pure $ CreateScimTokenResponse token info

  case idps of
    [idp] -> caseOneOrNoIdP . Just $ idp ^. SAML.idpId
    [] -> caseOneOrNoIdP Nothing
    -- NB: if the following case does not result in errors, 'validateScimUser' needs to
    -- be changed.  currently, it relies on the fact that there is never more than one IdP.
    -- https://wearezeta.atlassian.net/browse/SQSERVICES-165
    _ ->
      throwSparSem $
        E.SparProvisioningMoreThanOneIdP
          "SCIM tokens can only be created for a team with at most one IdP"

-- | > docs/reference/provisioning/scim-token.md {#RefScimTokenDelete}
--
-- Delete a token belonging to user's team.
deleteScimToken ::
  Members '[GalleyAccess, BrigAccess, ScimTokenStore, Error E.SparError] r =>
  -- | Who is trying to delete a token
  Maybe UserId ->
  ScimTokenId ->
  Sem r NoContent
deleteScimToken zusr tokenid = do
  teamid <- Intra.Brig.authorizeScimTokenManagement zusr
  ScimTokenStore.delete teamid tokenid
  pure NoContent

-- | > docs/reference/provisioning/scim-token.md {#RefScimTokenList}
--
-- List all tokens belonging to user's team. Tokens themselves are not available, only
-- metadata about them.
listScimTokens ::
  Members '[GalleyAccess, BrigAccess, ScimTokenStore, Error E.SparError] r =>
  -- | Who is trying to list tokens
  Maybe UserId ->
  Sem r ScimTokenList
listScimTokens zusr = do
  teamid <- Intra.Brig.authorizeScimTokenManagement zusr
  ScimTokenList <$> ScimTokenStore.lookupByTeam teamid
