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
import Data.Code as Code
import Data.Id
import Data.Misc
import qualified Data.Text.Encoding as T
import Data.Text.Encoding.Error
import Imports
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
import Wire.API.Routes.Named
import Wire.API.Routes.Public.Spar (APIScimToken)
import Wire.API.User as User
import Wire.API.User.Scim as Api
import Wire.Sem.Now (Now)
import qualified Wire.Sem.Now as Now
import Wire.Sem.Random (Random)
import qualified Wire.Sem.Random as Random

-- | An instance that tells @hscim@ how authentication should be done for SCIM routes.
instance (Member ScimTokenStore r) => Scim.Class.Auth.AuthDB SparTag (Sem r) where
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
  ( Member Random r,
    Member (Input Opts) r,
    Member GalleyAccess r,
    Member BrigAccess r,
    Member ScimTokenStore r,
    Member Now r,
    Member IdPConfigStore r,
    Member (Error E.SparError) r
  ) =>
  ServerT APIScimToken (Sem r)
apiScimToken =
  Named @"auth-tokens-create@v6" createScimTokenV6
    :<|> Named @"auth-tokens-create" createScimToken
    :<|> Named @"auth-tokens-put-name" updateScimTokenName
    :<|> Named @"auth-tokens-delete" deleteScimToken
    :<|> Named @"auth-tokens-list@v6" listScimTokensV6
    :<|> Named @"auth-tokens-list" listScimTokens

updateScimTokenName ::
  ( Member BrigAccess r,
    Member ScimTokenStore r,
    Member (Error E.SparError) r,
    Member GalleyAccess r
  ) =>
  UserId ->
  ScimTokenId ->
  ScimTokenName ->
  Sem r ()
updateScimTokenName lusr tokenId name = do
  teamid <- Intra.Brig.authorizeScimTokenManagement (Just lusr)
  ScimTokenStore.updateName teamid tokenId name.fromScimTokenName

-- | > docs/reference/provisioning/scim-token.md {#RefScimTokenCreate}
--
-- Create a token for user's team.
createScimTokenV6 ::
  forall r.
  ( Member Random r,
    Member (Input Opts) r,
    Member GalleyAccess r,
    Member BrigAccess r,
    Member ScimTokenStore r,
    Member IdPConfigStore r,
    Member Now r,
    Member (Error E.SparError) r
  ) =>
  -- | Who is trying to create a token
  Maybe UserId ->
  -- | Request body
  CreateScimToken ->
  Sem r CreateScimTokenResponseV6
createScimTokenV6 zusr createTok = do
  teamid <- guardScimTokenCreation zusr createTok.password createTok.verificationCode
  idps <- IdPConfigStore.getConfigsByTeam teamid
  mIdpId <- case idps of
    [config] -> pure . Just $ config ^. SAML.idpId
    [] -> pure Nothing
    -- NB: if we ever were to allow several idps for one scim peer (which we won't),
    -- 'validateScimUser' would need to be changed.  currently, it relies on the fact that
    -- there is never more than one IdP.
    -- https://wearezeta.atlassian.net/browse/SQSERVICES-165
    (_ : _ : _) -> throwSparSem $ E.SparProvisioningMoreThanOneIdP E.TwoIdpsAndScimTokenForbidden

  responseToV6 <$> createScimTokenUnchecked teamid Nothing createTok.description mIdpId
  where
    responseToV6 :: CreateScimTokenResponse -> CreateScimTokenResponseV6
    responseToV6 (CreateScimTokenResponse token info) = CreateScimTokenResponseV6 token (infoToV6 info)

    infoToV6 :: ScimTokenInfo -> ScimTokenInfoV6
    infoToV6 ScimTokenInfo {..} = ScimTokenInfoV6 {..}

-- | > docs/reference/provisioning/scim-token.md {#RefScimTokenCreate}
--
-- Create a token for user's team.
createScimToken ::
  forall r.
  ( Member Random r,
    Member (Input Opts) r,
    Member GalleyAccess r,
    Member BrigAccess r,
    Member ScimTokenStore r,
    Member IdPConfigStore r,
    Member Now r,
    Member (Error E.SparError) r
  ) =>
  -- | Who is trying to create a token
  Maybe UserId ->
  -- | Request body
  CreateScimToken ->
  Sem r CreateScimTokenResponse
createScimToken zusr Api.CreateScimToken {..} = do
  teamid <- guardScimTokenCreation zusr password verificationCode
  mIdPId <- maybe (pure Nothing) (\idpid -> IdPConfigStore.getConfig idpid $> Just idpid) idp
  createScimTokenUnchecked teamid name description mIdPId

guardScimTokenCreation ::
  forall r.
  ( Member (Input Opts) r,
    Member GalleyAccess r,
    Member BrigAccess r,
    Member ScimTokenStore r,
    Member (Error E.SparError) r
  ) =>
  -- | Who is trying to create a token
  Maybe UserId ->
  Maybe PlainTextPassword6 ->
  Maybe Code.Value ->
  Sem r TeamId
guardScimTokenCreation zusr password verificationCode = do
  teamid <- Intra.Brig.authorizeScimTokenManagement zusr
  BrigAccess.ensureReAuthorised zusr password verificationCode (Just User.CreateScimToken)
  tokenNumber <- length <$> ScimTokenStore.lookupByTeam teamid
  maxTokens <- inputs maxScimTokens
  unless (tokenNumber < maxTokens) $
    throwSparSem E.SparProvisioningTokenLimitReached
  pure teamid

-- Create a token for user's team.
createScimTokenUnchecked ::
  forall r.
  ( Member Random r,
    Member ScimTokenStore r,
    Member Now r
  ) =>
  TeamId ->
  Maybe Text ->
  Text ->
  Maybe SAML.IdPId ->
  Sem r CreateScimTokenResponse
createScimTokenUnchecked teamid mName desc mIdPId = do
  token <-
    ScimToken . T.decodeUtf8With lenientDecode . ES.encode
      <$> Random.bytes 32
  tokenid <- Random.scimTokenId
  now <- Now.get
  let info =
        ScimTokenInfo
          { stiId = tokenid,
            stiTeam = teamid,
            stiCreatedAt = now,
            stiIdP = mIdPId,
            stiDescr = desc,
            stiName = fromMaybe (idToText tokenid) mName
          }
  ScimTokenStore.insert token info
  pure $ CreateScimTokenResponse token info

-- | > docs/reference/provisioning/scim-token.md {#RefScimTokenDelete}
--
-- Delete a token belonging to user's team.
deleteScimToken ::
  ( Member GalleyAccess r,
    Member BrigAccess r,
    Member ScimTokenStore r,
    Member (Error E.SparError) r
  ) =>
  -- | Who is trying to delete a token
  Maybe UserId ->
  ScimTokenId ->
  Sem r NoContent
deleteScimToken zusr tokenid = do
  teamid <- Intra.Brig.authorizeScimTokenManagement zusr
  ScimTokenStore.delete teamid tokenid
  pure NoContent

listScimTokensV6 ::
  ( Member GalleyAccess r,
    Member BrigAccess r,
    Member ScimTokenStore r,
    Member (Error E.SparError) r
  ) =>
  -- | Who is trying to list tokens
  Maybe UserId ->
  Sem r ScimTokenListV6
listScimTokensV6 zusr = toV6 <$> listScimTokens zusr
  where
    toV6 :: ScimTokenList -> ScimTokenListV6
    toV6 (ScimTokenList tokens) = ScimTokenListV6 $ map infoToV6 tokens

    infoToV6 :: ScimTokenInfo -> ScimTokenInfoV6
    infoToV6 ScimTokenInfo {..} = ScimTokenInfoV6 {..}

-- | > docs/reference/provisioning/scim-token.md {#RefScimTokenList}
--
-- List all tokens belonging to user's team. Tokens themselves are not available, only
-- metadata about them.
listScimTokens ::
  ( Member GalleyAccess r,
    Member BrigAccess r,
    Member ScimTokenStore r,
    Member (Error E.SparError) r
  ) =>
  -- | Who is trying to list tokens
  Maybe UserId ->
  Sem r ScimTokenList
listScimTokens zusr = do
  teamid <- Intra.Brig.authorizeScimTokenManagement zusr
  ScimTokenList <$> ScimTokenStore.lookupByTeam teamid
