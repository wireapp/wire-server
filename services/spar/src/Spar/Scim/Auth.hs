{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
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
import Data.Id
import Data.String.Conversions
import Data.Time
import Imports
import OpenSSL.Random (randBytes)
import qualified SAML2.WebSSO as SAML
import Servant
import Spar.App (Spar, sparCtxOpts, wrapMonadClient)
import qualified Spar.Data as Data
import Spar.Error
import qualified Spar.Intra.Brig as Intra.Brig
import Spar.Scim.Types
import Spar.Types
-- FUTUREWORK: these imports are not very handy.  split up Spar.Scim into
-- Spar.Scim.{Core,User,Group} to avoid at least some of the hscim name clashes?
import qualified Web.Scim.Class.Auth as Scim.Class.Auth
import qualified Web.Scim.Handler as Scim
import qualified Web.Scim.Schema.Error as Scim

-- | An instance that tells @hscim@ how authentication should be done for SCIM routes.
instance Scim.Class.Auth.AuthDB SparTag Spar where
  -- Validate and resolve a given token
  authCheck :: Maybe ScimToken -> Scim.ScimHandler Spar ScimTokenInfo
  authCheck Nothing =
    Scim.throwScim (Scim.unauthorized "Token not provided")
  authCheck (Just token) =
    maybe (Scim.throwScim (Scim.unauthorized "Invalid token")) pure
      =<< lift (wrapMonadClient (Data.lookupScimToken token))

----------------------------------------------------------------------------
-- Token API

-- TODO: don't forget to delete the tokens when the team is deleted

-- | API for manipulating SCIM tokens (protected by normal Wire authentication and available
-- only to team owners).
apiScimToken :: ServerT APIScimToken Spar
apiScimToken =
  createScimToken
    :<|> deleteScimToken
    :<|> listScimTokens

-- | > docs/reference/provisioning/scim-token.md {#RefScimTokenCreate}
--
-- Create a token for user's team.
createScimToken ::
  -- | Who is trying to create a token
  Maybe UserId ->
  -- | Request body
  CreateScimToken ->
  Spar CreateScimTokenResponse
createScimToken zusr CreateScimToken {..} = do
  let descr = createScimTokenDescr
  teamid <- Intra.Brig.getZUsrOwnedTeam zusr
  Intra.Brig.ensureReAuthorised zusr createScimTokenPassword
  tokenNumber <- fmap length $ wrapMonadClient $ Data.getScimTokens teamid
  maxTokens <- asks (maxScimTokens . sparCtxOpts)
  unless (tokenNumber < maxTokens) $
    throwSpar SparProvisioningTokenLimitReached
  idps <- wrapMonadClient $ Data.getIdPConfigsByTeam teamid
  case idps of
    [idp] -> do
      -- TODO: sign tokens. Also, we might want to use zauth, if we can / if
      -- it makes sense semantically
      token <- ScimToken . cs . ES.encode <$> liftIO (randBytes 32)
      tokenid <- randomId
      now <- liftIO getCurrentTime
      let idpid = idp ^. SAML.idpId
          info =
            ScimTokenInfo
              { stiId = tokenid,
                stiTeam = teamid,
                stiCreatedAt = now,
                stiIdP = Just idpid,
                stiDescr = descr
              }
      wrapMonadClient $ Data.insertScimToken token info
      pure $ CreateScimTokenResponse token info
    -- NB: if the two following cases do not result in errors, 'validateScimUser' needs to
    -- be changed.  currently, it relies on the fact that there is always an IdP.
    [] ->
      throwSpar $
        SparProvisioningNoSingleIdP
          "SCIM tokens can only be created for a team with an IdP, \
          \but none are found"
    _ ->
      throwSpar $
        SparProvisioningNoSingleIdP
          "SCIM tokens can only be created for a team with exactly one IdP, \
          \but more are found"

-- | > docs/reference/provisioning/scim-token.md {#RefScimTokenDelete}
--
-- Delete a token belonging to user's team.
deleteScimToken ::
  -- | Who is trying to delete a token
  Maybe UserId ->
  ScimTokenId ->
  Spar NoContent
deleteScimToken zusr tokenid = do
  teamid <- Intra.Brig.getZUsrOwnedTeam zusr
  wrapMonadClient $ Data.deleteScimToken teamid tokenid
  pure NoContent

-- | > docs/reference/provisioning/scim-token.md {#RefScimTokenList}
--
-- List all tokens belonging to user's team. Tokens themselves are not available, only
-- metadata about them.
listScimTokens ::
  -- | Who is trying to list tokens
  Maybe UserId ->
  Spar ScimTokenList
listScimTokens zusr = do
  teamid <- Intra.Brig.getZUsrOwnedTeam zusr
  ScimTokenList <$> wrapMonadClient (Data.getScimTokens teamid)
