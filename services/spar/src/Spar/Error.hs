{-# LANGUAGE TypeSynonymInstances #-}

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
    rethrow,
    parseResponse,
    -- FUTUREWORK: we really shouldn't export this, but that requires that we can use our
    -- custom servant monad in the 'MakeCustomError' instances.
    servantToWaiError,
    sparToServerError,
    renderSparError,
    waiToServant,
  )
where

import Bilge (ResponseLBS, responseBody, responseJsonMaybe)
import qualified Bilge
import Control.Monad.Except
import Data.Aeson
import Data.String.Conversions
import Data.Typeable (typeRep)
import GHC.Stack (callStack, prettyCallStack)
import Imports
import Network.HTTP.Types.Status
import qualified Network.Wai as Wai
import qualified Network.Wai.Utilities.Error as Wai
import qualified Network.Wai.Utilities.Server as Wai
import qualified SAML2.WebSSO as SAML
import Servant
import qualified System.Logger.Class as Log
import qualified Web.Scim.Schema.Error as Scim
import Wire.API.User.Saml (TTLError)

type SparError = SAML.Error SparCustomError

-- FUTUREWORK: This instance should probably be inside saml2-web-sso instead.
instance Exception SparError

throwSpar :: MonadError SparError m => SparCustomError -> m a
throwSpar = throwError . SAML.CustomError

data SparCustomError
  = SparIdPNotFound LT
  | SparSamlCredentialsNotFound
  | SparMissingZUsr
  | SparNotInTeam
  | SparNoPermission LT
  | SparSSODisabled
  | SparInitLoginWithAuth
  | SparInitBindWithoutAuth
  | SparNoSuchRequest
  | SparNoRequestRefInResponse LT
  | SparCouldNotSubstituteSuccessURI LT
  | SparCouldNotSubstituteFailureURI LT
  | SparBadInitiateLoginQueryParams LT
  | SparBindFromWrongOrNoTeam LT
  | SparBindFromBadAccountStatus LT
  | SparBindUserRefTaken
  | SparUserRefInNoOrMultipleTeams LT
  | SparBadUserName LT
  | SparCannotCreateUsersOnReplacedIdP LT
  | SparCouldNotParseRfcResponse LT LT
  | SparReAuthRequired
  | SparReAuthCodeAuthFailed
  | SparReAuthCodeAuthRequired
  | SparCouldNotRetrieveCookie
  | SparCassandraError LT
  | SparCassandraTTLError TTLError
  | SparNewIdPBadMetadata LT
  | SparNewIdPPubkeyMismatch
  | SparNewIdPAlreadyInUse LT
  | SparNewIdPWantHttps LT
  | SparIdPHasBoundUsers
  | SparIdPIssuerInUse
  | SparProvisioningMoreThanOneIdP LT
  | SparProvisioningTokenLimitReached
  | -- | FUTUREWORK(fisx): This constructor is used in exactly one place (see
    -- "Spar.Sem.SAML2.Library"), for an error that immediately gets caught.
    -- Instead, we could just use an IO exception, and catch it with
    -- 'catchErrors' (see "Spar.Run"). Maybe we want to remove this case
    -- altogether? Not sure.
    SparInternalError LT
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
  Wai.mkError (Status code (cs phrase)) (cs phrase) (cs body)

sparToServerError :: SparError -> ServerError
sparToServerError = either id waiToServant . renderSparError

waiToServant :: Wai.Error -> ServerError
waiToServant waierr@(Wai.Error status label _ _) =
  ServerError
    { errHTTPCode = statusCode status,
      errReasonPhrase = cs label,
      errBody = encode waierr,
      errHeaders = []
    }

renderSparError :: SparError -> Either ServerError Wai.Error
renderSparError (SAML.CustomError SparNoSuchRequest) = Right $ Wai.mkError status500 "server-error" "AuthRequest seems to have disappeared (could not find verdict format)."
renderSparError (SAML.CustomError (SparNoRequestRefInResponse msg)) = Right $ Wai.mkError status400 "server-error-unsupported-saml" ("The IdP needs to provide an InResponseTo attribute in the assertion: " <> msg)
renderSparError (SAML.CustomError (SparCouldNotSubstituteSuccessURI msg)) = Right $ Wai.mkError status400 "bad-success-redirect" ("re-parsing the substituted URI failed: " <> msg)
renderSparError (SAML.CustomError (SparCouldNotSubstituteFailureURI msg)) = Right $ Wai.mkError status400 "bad-failure-redirect" ("re-parsing the substituted URI failed: " <> msg)
renderSparError (SAML.CustomError (SparBadInitiateLoginQueryParams label)) = Right $ Wai.mkError status400 label label
renderSparError (SAML.CustomError (SparBindFromWrongOrNoTeam msg)) = Right $ Wai.mkError status403 "bad-team" ("Forbidden: wrong user team " <> msg)
renderSparError (SAML.CustomError (SparBindFromBadAccountStatus msg)) = Right $ Wai.mkError status403 "bad-account-status" ("Forbidden: user has account status " <> msg <> "; only Active, PendingInvitation are supported")
renderSparError (SAML.CustomError SparBindUserRefTaken) = Right $ Wai.mkError status403 "subject-id-taken" "Forbidden: SubjectID is used by another wire user.  If you have an old user bound to this IdP, unbind or delete that user."
renderSparError (SAML.CustomError (SparUserRefInNoOrMultipleTeams msg)) = Right $ Wai.mkError status403 "bad-team" ("Forbidden: multiple teams or no team for same UserRef " <> msg)
renderSparError (SAML.CustomError (SparBadUserName msg)) = Right $ Wai.mkError status400 "bad-username" ("Bad UserName in SAML response, except len [1, 128]: " <> msg)
renderSparError (SAML.CustomError (SparCannotCreateUsersOnReplacedIdP replacingIdPId)) = Right $ Wai.mkError status400 "cannont-provision-on-replaced-idp" ("This IdP has been replaced, users can only be auto-provisioned on the replacing IdP " <> replacingIdPId)
-- RFC-specific errors
renderSparError (SAML.CustomError (SparCouldNotParseRfcResponse service msg)) = Right $ Wai.mkError status502 "bad-upstream" ("Could not parse " <> service <> " response body: " <> msg)
renderSparError (SAML.CustomError SparReAuthRequired) = Right $ Wai.mkError status403 "access-denied" "This operation requires reauthentication."
renderSparError (SAML.CustomError SparReAuthCodeAuthFailed) = Right $ Wai.mkError status403 "code-authentication-failed" "Reauthentication failed with invalid verification code."
renderSparError (SAML.CustomError SparReAuthCodeAuthRequired) = Right $ Wai.mkError status403 "code-authentication-required" "Reauthentication failed. Verification code required."
renderSparError (SAML.CustomError SparCouldNotRetrieveCookie) = Right $ Wai.mkError status502 "bad-upstream" "Unable to get a cookie from an upstream server."
renderSparError (SAML.CustomError (SparCassandraError msg)) = Right $ Wai.mkError status500 "server-error" msg -- TODO: should we be more specific here and make it 'db-error'?
renderSparError (SAML.CustomError (SparCassandraTTLError ttlerr)) = Right $ Wai.mkError status400 "ttl-error" (cs $ show ttlerr)
renderSparError (SAML.UnknownIdP msg) = Right $ Wai.mkError status404 "not-found" ("IdP not found: " <> msg)
renderSparError (SAML.Forbidden msg) = Right $ Wai.mkError status403 "forbidden" ("Forbidden: " <> msg)
renderSparError (SAML.BadSamlResponseBase64Error msg) = Right $ Wai.mkError status400 "bad-response-encoding" ("Bad response: base64 error: " <> cs msg)
renderSparError (SAML.BadSamlResponseXmlError msg) = Right $ Wai.mkError status400 "bad-response-xml" ("Bad response: XML parse error: " <> cs msg)
renderSparError (SAML.BadSamlResponseSamlError msg) = Right $ Wai.mkError status400 "bad-response-saml" ("Bad response: SAML parse error: " <> cs msg)
renderSparError SAML.BadSamlResponseFormFieldMissing = Right $ Wai.mkError status400 "bad-response-saml" ("Bad response: SAMLResponse form field missing from HTTP body")
renderSparError SAML.BadSamlResponseIssuerMissing = Right $ Wai.mkError status400 "bad-response-saml" ("Bad response: no Issuer in AuthnResponse")
renderSparError SAML.BadSamlResponseNoAssertions = Right $ Wai.mkError status400 "bad-response-saml" ("Bad response: no assertions in AuthnResponse")
renderSparError SAML.BadSamlResponseAssertionWithoutID = Right $ Wai.mkError status400 "bad-response-saml" ("Bad response: assertion without ID")
renderSparError (SAML.BadSamlResponseInvalidSignature msg) = Right $ Wai.mkError status400 "bad-response-signature" (cs msg)
renderSparError (SAML.CustomError (SparIdPNotFound "")) = Right $ Wai.mkError status404 "not-found" "Could not find IdP."
renderSparError (SAML.CustomError (SparIdPNotFound msg)) = Right $ Wai.mkError status404 "not-found" ("Could not find IdP: " <> msg)
renderSparError (SAML.CustomError SparSamlCredentialsNotFound) = Right $ Wai.mkError status404 "not-found" "Could not find SAML credentials, and auto-provisioning is disabled."
renderSparError (SAML.CustomError SparMissingZUsr) = Right $ Wai.mkError status400 "client-error" "[header] 'Z-User' required"
renderSparError (SAML.CustomError SparNotInTeam) = Right $ Wai.mkError status403 "no-team-member" "Requesting user is not a team member or not a member of this team."
renderSparError (SAML.CustomError (SparNoPermission perm)) = Right $ Wai.mkError status403 "insufficient-permissions" ("You need permission " <> cs perm <> ".")
renderSparError (SAML.CustomError SparSSODisabled) = Right $ Wai.mkError status403 "sso-disabled" "Please ask customer support to enable this feature for your team."
renderSparError (SAML.CustomError SparInitLoginWithAuth) = Right $ Wai.mkError status403 "login-with-auth" "This end-point is only for login, not binding."
renderSparError (SAML.CustomError SparInitBindWithoutAuth) = Right $ Wai.mkError status403 "bind-without-auth" "This end-point is only for binding, not login."
renderSparError SAML.UnknownError = Right $ Wai.mkError status500 "server-error" "Unknown server error."
renderSparError (SAML.BadServerConfig msg) = Right $ Wai.mkError status500 "server-error" ("Error in server config: " <> msg)
renderSparError (SAML.InvalidCert msg) = Right $ Wai.mkError status500 "invalid-certificate" ("Error in idp certificate: " <> msg)
-- Errors related to IdP creation
renderSparError (SAML.CustomError (SparNewIdPBadMetadata msg)) = Right $ Wai.mkError status400 "invalid-metadata" msg
renderSparError (SAML.CustomError SparNewIdPPubkeyMismatch) = Right $ Wai.mkError status400 "key-mismatch" "public keys in body, metadata do not match"
renderSparError (SAML.CustomError (SparNewIdPAlreadyInUse msg)) = Right $ Wai.mkError status400 "idp-already-in-use" msg
renderSparError (SAML.CustomError (SparNewIdPWantHttps msg)) = Right $ Wai.mkError status400 "idp-must-be-https" ("an idp request uri must be https, not http or other: " <> msg)
renderSparError (SAML.CustomError SparIdPHasBoundUsers) = Right $ Wai.mkError status412 "idp-has-bound-users" "an idp can only be deleted if it is empty"
renderSparError (SAML.CustomError SparIdPIssuerInUse) = Right $ Wai.mkError status400 "idp-issuer-in-use" "The issuer of your IdP is already in use.  Remove the entry in the team that uses it, or construct a new IdP issuer."
-- Errors related to provisioning
renderSparError (SAML.CustomError (SparProvisioningMoreThanOneIdP msg)) = Right $ Wai.mkError status400 "more-than-one-idp" ("Team can have at most one IdP configured: " <> msg)
renderSparError (SAML.CustomError SparProvisioningTokenLimitReached) = Right $ Wai.mkError status403 "token-limit-reached" "The limit of provisioning tokens per team has been reached"
-- SCIM errors
renderSparError (SAML.CustomError (SparScimError err)) = Left $ Scim.scimToServerError err
renderSparError (SAML.CustomError (SparInternalError err)) = Right $ Wai.mkError status500 "server-error" ("Internal error: " <> err)
-- Other
renderSparError (SAML.CustomServant err) = Left err

-- | If a call to another backend service fails, just respond with whatever it said.
--
-- FUTUREWORK: with servant, there will be a way for the type checker to confirm that we
-- handle all exceptions that brig can legally throw!
rethrow :: LText -> ResponseLBS -> (HasCallStack, Log.MonadLogger m, MonadError SparError m) => m a
rethrow serviceName resp = do
  Log.info
    ( Log.msg ("rfc error" :: Text)
        . Log.field "status" (Bilge.statusCode resp)
        . Log.field "error" (show err)
        . Log.field "callstack" (prettyCallStack callStack)
    )
  throwError err
  where
    err :: SparError
    err =
      responseJsonMaybe resp
        & maybe
          ( SAML.CustomError
              . SparCouldNotParseRfcResponse serviceName
              . ("internal error: " <>)
              . cs
              . show
              . (Bilge.statusCode resp,)
              . fromMaybe "<empty body>"
              . responseBody
              $ resp
          )
          (SAML.CustomServant . waiToServant)

parseResponse :: forall a m. (FromJSON a, MonadError SparError m, Typeable a) => LT -> ResponseLBS -> m a
parseResponse serviceName resp = do
  let typeinfo :: LT
      typeinfo = cs $ show (typeRep ([] @a)) <> ": "

      err :: forall a'. LT -> m a'
      err = throwSpar . SparCouldNotParseRfcResponse serviceName . (typeinfo <>)

  bdy <- maybe (err "no body") pure $ responseBody resp
  either (err . cs) pure $ eitherDecode' bdy
