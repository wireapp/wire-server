{-# LANGUAGE TypeSynonymInstances #-}

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

-- | Error reporting in Spar.
--
-- All errors we throw are 'SparError's.
--
-- FUTUREWORK: since SCIM errors have their own format, the whole SCIM API subtree just wraps
-- errors into 'SAML.CustomServant'. This could be reworked by creating a new branch in
-- 'SparCustomError'.
module Spar.Error
  ( SparError,
    SparCustomError (..),
    throwSpar,
    sparToServerErrorWithLogging,
    renderSparErrorWithLogging,
    -- FUTUREWORK: we really shouldn't export this, but that requires that we can use our
    -- custom servant monad in the 'MakeCustomError' instances.
    servantToWaiError,
    sparToServerError,
    renderSparError,
    waiToServant,
  )
where

import Control.Monad.Except
import Data.Aeson
import Data.String.Conversions
import Imports
import Network.HTTP.Types.Status
import qualified Network.Wai as Wai
import qualified Network.Wai.Utilities.Error as Wai
import qualified Network.Wai.Utilities.Server as Wai
import qualified SAML2.WebSSO as SAML
import Servant
import Spar.Types (TTLError)
import qualified System.Logger.Class as Log
import qualified Web.Scim.Schema.Error as Scim

type SparError = SAML.Error SparCustomError

-- FUTUREWORK: This instance should probably be inside saml2-web-sso instead.
instance Exception SparError

throwSpar :: MonadError SparError m => SparCustomError -> m a
throwSpar = throwError . SAML.CustomError

data SparCustomError
  = SparIdPNotFound
  | SparMissingZUsr
  | SparNotInTeam
  | SparNotTeamOwner
  | SparSSODisabled
  | SparInitLoginWithAuth
  | SparInitBindWithoutAuth
  | SparNoSuchRequest
  | SparNoRequestRefInResponse LT
  | SparCouldNotSubstituteSuccessURI LT
  | SparCouldNotSubstituteFailureURI LT
  | SparBadInitiateLoginQueryParams LT
  | SparBindFromWrongOrNoTeam LT
  | SparBindUserRefTaken
  | SparBadUserName LT
  | SparCannotCreateUsersOnReplacedIdP LT
  | SparNoBodyInBrigResponse
  | SparCouldNotParseBrigResponse LT
  | SparReAuthRequired
  | SparBrigError LT
  | SparBrigErrorWith Status LT
  | SparNoBodyInGalleyResponse
  | SparCouldNotParseGalleyResponse LT
  | SparGalleyError LT
  | SparCouldNotRetrieveCookie
  | SparCassandraError LT
  | SparCassandraTTLError TTLError
  | SparNewIdPBadMetadata LT
  | SparNewIdPPubkeyMismatch
  | SparNewIdPAlreadyInUse
  | SparNewIdPWantHttps LT
  | SparIdPHasBoundUsers
  | SparIdPIssuerInUse
  | SparProvisioningNoSingleIdP LT
  | SparProvisioningTokenLimitReached
  | -- | All errors returned from SCIM handlers are wrapped into 'SparScimError'
    SparScimError Scim.ScimError
  deriving (Eq, Show)

sparToServerErrorWithLogging :: MonadIO m => Log.Logger -> SparError -> m ServerError
sparToServerErrorWithLogging logger err = do
  let errServant = sparToServerError err
  liftIO $ Wai.logError logger (Nothing :: Maybe Wai.Request) (servantToWaiError errServant)
  pure errServant

servantToWaiError :: ServerError -> Wai.Error
servantToWaiError (ServerError code phrase body _headers) =
  Wai.Error (Status code (cs phrase)) (cs phrase) (cs body)

sparToServerError :: SparError -> ServerError
sparToServerError = either id waiToServant . renderSparError

waiToServant :: Wai.Error -> ServerError
waiToServant waierr@(Wai.Error status label _) =
  ServerError
    { errHTTPCode = statusCode status,
      errReasonPhrase = cs label,
      errBody = encode waierr,
      errHeaders = []
    }

renderSparErrorWithLogging :: MonadIO m => Log.Logger -> SparError -> m (Either ServerError Wai.Error)
renderSparErrorWithLogging logger err = do
  let errPossiblyWai = renderSparError err
  liftIO $ Wai.logError logger (Nothing :: Maybe Wai.Request) (either servantToWaiError id $ errPossiblyWai)
  pure errPossiblyWai

renderSparError :: SparError -> Either ServerError Wai.Error
renderSparError (SAML.CustomError SparNoSuchRequest) = Right $ Wai.Error status500 "server-error" "AuthRequest seems to have disappeared (could not find verdict format)."
renderSparError (SAML.CustomError (SparNoRequestRefInResponse msg)) = Right $ Wai.Error status400 "server-error-unsupported-saml" ("The IdP needs to provide an InResponseTo attribute in the assertion: " <> msg)
renderSparError (SAML.CustomError (SparCouldNotSubstituteSuccessURI msg)) = Right $ Wai.Error status400 "bad-success-redirect" ("re-parsing the substituted URI failed: " <> msg)
renderSparError (SAML.CustomError (SparCouldNotSubstituteFailureURI msg)) = Right $ Wai.Error status400 "bad-failure-redirect" ("re-parsing the substituted URI failed: " <> msg)
renderSparError (SAML.CustomError (SparBadInitiateLoginQueryParams label)) = Right $ Wai.Error status400 label label
renderSparError (SAML.CustomError (SparBindFromWrongOrNoTeam msg)) = Right $ Wai.Error status403 "bad-team" ("Forbidden: wrong user team " <> msg)
renderSparError (SAML.CustomError SparBindUserRefTaken) = Right $ Wai.Error status403 "subject-id-taken" "Forbidden: SubjectID is used by another wire user.  If you have an old user bound to this IdP, unbind or delete that user."
renderSparError (SAML.CustomError (SparBadUserName msg)) = Right $ Wai.Error status400 "bad-username" ("Bad UserName in SAML response, except len [1, 128]: " <> msg)
renderSparError (SAML.CustomError (SparCannotCreateUsersOnReplacedIdP replacingIdPId)) = Right $ Wai.Error status400 "cannont-provision-on-replaced-idp" ("This IdP has been replaced, users can only be auto-provisioned on the replacing IdP " <> replacingIdPId)
-- Brig-specific errors
renderSparError (SAML.CustomError SparNoBodyInBrigResponse) = Right $ Wai.Error status502 "bad-upstream" "Failed to get a response from an upstream server."
renderSparError (SAML.CustomError (SparCouldNotParseBrigResponse msg)) = Right $ Wai.Error status502 "bad-upstream" ("Could not parse response body: " <> msg)
renderSparError (SAML.CustomError SparReAuthRequired) = Right $ Wai.Error status403 "access-denied" "This operation requires reauthentication."
renderSparError (SAML.CustomError (SparBrigError msg)) = Right $ Wai.Error status500 "bad-upstream" msg
renderSparError (SAML.CustomError (SparBrigErrorWith status msg)) = Right $ Wai.Error status "bad-upstream" msg
-- Galley-specific errors
renderSparError (SAML.CustomError SparNoBodyInGalleyResponse) = Right $ Wai.Error status502 "bad-upstream" "Failed to get a response from an upstream server."
renderSparError (SAML.CustomError (SparCouldNotParseGalleyResponse msg)) = Right $ Wai.Error status502 "bad-upstream" ("Could not parse response body: " <> msg)
renderSparError (SAML.CustomError (SparGalleyError msg)) = Right $ Wai.Error status500 "bad-upstream" msg
renderSparError (SAML.CustomError SparCouldNotRetrieveCookie) = Right $ Wai.Error status502 "bad-upstream" "Unable to get a cookie from an upstream server."
renderSparError (SAML.CustomError (SparCassandraError msg)) = Right $ Wai.Error status500 "server-error" msg -- TODO: should we be more specific here and make it 'db-error'?
renderSparError (SAML.CustomError (SparCassandraTTLError ttlerr)) = Right $ Wai.Error status400 "ttl-error" (cs $ show ttlerr)
renderSparError (SAML.UnknownIdP msg) = Right $ Wai.Error status404 "not-found" ("IdP not found: " <> msg)
renderSparError (SAML.Forbidden msg) = Right $ Wai.Error status403 "forbidden" ("Forbidden: " <> msg)
renderSparError (SAML.BadSamlResponseBase64Error msg) = Right $ Wai.Error status400 "bad-response-encoding" ("Bad response: base64 error: " <> cs msg)
renderSparError (SAML.BadSamlResponseXmlError msg) = Right $ Wai.Error status400 "bad-response-xml" ("Bad response: XML parse error: " <> cs msg)
renderSparError (SAML.BadSamlResponseSamlError msg) = Right $ Wai.Error status400 "bad-response-saml" ("Bad response: SAML parse error: " <> cs msg)
renderSparError SAML.BadSamlResponseFormFieldMissing = Right $ Wai.Error status400 "bad-response-saml" ("Bad response: SAMLResponse form field missing from HTTP body")
renderSparError SAML.BadSamlResponseIssuerMissing = Right $ Wai.Error status400 "bad-response-saml" ("Bad response: no Issuer in AuthnResponse")
renderSparError SAML.BadSamlResponseNoAssertions = Right $ Wai.Error status400 "bad-response-saml" ("Bad response: no assertions in AuthnResponse")
renderSparError SAML.BadSamlResponseAssertionWithoutID = Right $ Wai.Error status400 "bad-response-saml" ("Bad response: assertion without ID")
renderSparError (SAML.BadSamlResponseInvalidSignature msg) = Right $ Wai.Error status400 "bad-response-signature" (cs msg)
renderSparError (SAML.CustomError SparIdPNotFound) = Right $ Wai.Error status404 "not-found" "Could not find IdP."
renderSparError (SAML.CustomError SparMissingZUsr) = Right $ Wai.Error status400 "client-error" "[header] 'Z-User' required"
renderSparError (SAML.CustomError SparNotInTeam) = Right $ Wai.Error status403 "no-team-member" "Requesting user is not a team member or not a member of this team."
renderSparError (SAML.CustomError SparNotTeamOwner) = Right $ Wai.Error status403 "insufficient-permissions" "You need to be a team owner."
renderSparError (SAML.CustomError SparSSODisabled) = Right $ Wai.Error status403 "sso-disabled" "Please ask customer support to enable this feature for your team."
renderSparError (SAML.CustomError SparInitLoginWithAuth) = Right $ Wai.Error status403 "login-with-auth" "This end-point is only for login, not binding."
renderSparError (SAML.CustomError SparInitBindWithoutAuth) = Right $ Wai.Error status403 "bind-without-auth" "This end-point is only for binding, not login."
renderSparError SAML.UnknownError = Right $ Wai.Error status500 "server-error" "Unknown server error."
renderSparError (SAML.BadServerConfig msg) = Right $ Wai.Error status500 "server-error" ("Error in server config: " <> msg)
renderSparError (SAML.InvalidCert msg) = Right $ Wai.Error status500 "invalid-certificate" ("Error in idp certificate: " <> msg)
-- Errors related to IdP creation
renderSparError (SAML.CustomError (SparNewIdPBadMetadata msg)) = Right $ Wai.Error status400 "invalid-metadata" msg
renderSparError (SAML.CustomError SparNewIdPPubkeyMismatch) = Right $ Wai.Error status400 "key-mismatch" "public keys in body, metadata do not match"
renderSparError (SAML.CustomError SparNewIdPAlreadyInUse) = Right $ Wai.Error status400 "idp-already-in-use" "an idp issuer can only be used within one team"
renderSparError (SAML.CustomError (SparNewIdPWantHttps msg)) = Right $ Wai.Error status400 "idp-must-be-https" ("an idp request uri must be https, not http or other: " <> msg)
renderSparError (SAML.CustomError SparIdPHasBoundUsers) = Right $ Wai.Error status412 "idp-has-bound-users" "an idp can only be deleted if it is empty"
renderSparError (SAML.CustomError SparIdPIssuerInUse) = Right $ Wai.Error status400 "idp-issuer-in-use" "The issuer of your IdP is already in use.  Remove the entry in the team that uses it, or construct a new IdP issuer."
-- Errors related to provisioning
renderSparError (SAML.CustomError (SparProvisioningNoSingleIdP msg)) = Right $ Wai.Error status400 "no-single-idp" ("Team should have exactly one IdP configured: " <> msg)
renderSparError (SAML.CustomError SparProvisioningTokenLimitReached) = Right $ Wai.Error status403 "token-limit-reached" "The limit of provisioning tokens per team has been reached"
-- SCIM errors
renderSparError (SAML.CustomError (SparScimError err)) = Left $ Scim.scimToServerError err
-- Other
renderSparError (SAML.CustomServant err) = Left err
