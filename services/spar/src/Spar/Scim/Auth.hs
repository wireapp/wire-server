{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

-- For @instance AuthDB Spar@
{-# OPTIONS_GHC -Wno-orphans #-}

-- | > docs/reference/provisioning/scim-token.md {#RefScimToken}
--
-- Logic for doing authentication in SCIM routes.
--
-- Every time a request to SCIM API is done, we grab a 'ScimToken' from the @"Authorization"@
-- header, check that it's valid, and resolve the team that this operation should apply to.
module Spar.Scim.Auth
    ( apiScimToken
    ) where

import Imports
import Control.Lens hiding ((.=), Strict)
import Data.Id
import Data.String.Conversions
import Data.Time
import OpenSSL.Random (randBytes)
import Servant
import Spar.App (Spar, wrapMonadClient, sparCtxOpts, wrapMonadClient)
import Spar.Error
import Spar.Scim.Types
import Spar.Types

import qualified Data.ByteString.Base64 as ES
import qualified SAML2.WebSSO as SAML
import qualified Spar.Data    as Data
import qualified Spar.Intra.Brig as Intra.Brig

-- FUTUREWORK: these imports are not very handy.  split up Spar.Scim into
-- Spar.Scim.{Core,User,Group} to avoid at least some of the hscim name clashes?
import qualified Web.Scim.Class.Auth              as Scim.Class.Auth
import qualified Web.Scim.Handler                 as Scim
import qualified Web.Scim.Schema.Error            as Scim

-- | An instance that tells @hscim@ how authentication should be done for SCIM routes.
instance Scim.Class.Auth.AuthDB SparTag Spar where
    -- Validate and resolve a given token
    authCheck :: Maybe ScimToken -> Scim.ScimHandler Spar ScimTokenInfo
    authCheck Nothing =
        Scim.throwScim (Scim.unauthorized "Token not provided")
    authCheck (Just token) =
        maybe (Scim.throwScim (Scim.unauthorized "Invalid token")) pure =<<
        lift (wrapMonadClient (Data.lookupScimToken token))

----------------------------------------------------------------------------
-- Token API

-- TODO: don't forget to delete the tokens when the team is deleted

-- | API for manipulating SCIM tokens (protected by normal Wire authentication and available
-- only to team owners).
apiScimToken :: ServerT APIScimToken Spar
apiScimToken
     = createScimToken
  :<|> deleteScimToken
  :<|> listScimTokens

-- | > docs/reference/provisioning/scim-token.md {#RefScimTokenCreate}
--
-- Create a token for user's team.
createScimToken
    :: Maybe UserId           -- ^ Who is trying to create a token
    -> CreateScimToken        -- ^ Request body
    -> Spar CreateScimTokenResponse
createScimToken zusr CreateScimToken{..} = do
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
                info = ScimTokenInfo
                    { stiId        = tokenid
                    , stiTeam      = teamid
                    , stiCreatedAt = now
                    , stiIdP       = Just idpid
                    , stiDescr     = descr
                    }
            wrapMonadClient $ Data.insertScimToken token info
            pure $ CreateScimTokenResponse token info

        -- NB: if the two following cases do not result in errors, 'validateScimUser' needs to
        -- be changed.  currently, it relies on the fact that there is always an IdP.
        [] -> throwSpar $ SparProvisioningNoSingleIdP
                "SCIM tokens can only be created for a team with an IdP, \
                \but none are found"
        _  -> throwSpar $ SparProvisioningNoSingleIdP
                "SCIM tokens can only be created for a team with exactly one IdP, \
                \but more are found"

-- | > docs/reference/provisioning/scim-token.md {#RefScimTokenDelete}
--
-- Delete a token belonging to user's team.
deleteScimToken
    :: Maybe UserId           -- ^ Who is trying to delete a token
    -> ScimTokenId
    -> Spar NoContent
deleteScimToken zusr tokenid = do
    teamid <- Intra.Brig.getZUsrOwnedTeam zusr
    wrapMonadClient $ Data.deleteScimToken teamid tokenid
    pure NoContent

-- | > docs/reference/provisioning/scim-token.md {#RefScimTokenList}
--
-- List all tokens belonging to user's team. Tokens themselves are not available, only
-- metadata about them.
listScimTokens
    :: Maybe UserId           -- ^ Who is trying to list tokens
    -> Spar ScimTokenList
listScimTokens zusr = do
    teamid <- Intra.Brig.getZUsrOwnedTeam zusr
    ScimTokenList <$> wrapMonadClient (Data.getScimTokens teamid)
