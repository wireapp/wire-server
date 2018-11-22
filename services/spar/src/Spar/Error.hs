{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Spar.Error
  ( SparError
  , SparCustomError(..)
  , throwSpar
  , sparToServantErr
  , sparToWaiError
  ) where

import Imports
import Control.Monad.Except
import Data.Aeson
import Data.String.Conversions
import Network.HTTP.Types.Status
import Servant
import Spar.Types (TTLError)

import qualified Network.Wai.Utilities.Error as Wai
import qualified SAML2.WebSSO as SAML


type SparError = SAML.Error SparCustomError

throwSpar :: MonadError SparError m => SparCustomError -> m a
throwSpar = throwError . SAML.CustomError

data SparCustomError
  = SparNotFound
  | SparMissingZUsr
  | SparNotInTeam
  | SparNotTeamOwner
  | SparInitLoginWithAuth
  | SparInitBindWithoutAuth
  | SparBindUserDisappearedFromBrig

  | SparNoRequestRefInResponse LT
  | SparCouldNotSubstituteSuccessURI LT
  | SparCouldNotSubstituteFailureURI LT
  | SparBadInitiateLoginQueryParams LT
  | SparBindFromWrongOrNoTeam LT
  | SparBindUserRefTaken

  | SparBadUserName LT
  | SparNoBodyInBrigResponse
  | SparCouldNotParseBrigResponse LT
  | SparBrigError LT
  | SparNoBodyInGalleyResponse
  | SparCouldNotParseGalleyResponse LT
  | SparGalleyError LT
  | SparCouldNotRetrieveCookie
  | SparCassandraError LT
  | SparCassandraTTLError TTLError

  | SparNewIdPBadMetaUrl LT
  | SparNewIdPBadMetaSig
  | SparNewIdPBadReqUrl LT
  | SparNewIdPPubkeyMismatch
  | SparNewIdPAlreadyInUse
  | SparNewIdPWantHttps LT

  | SparProvisioningNoSingleIdP LT
  | SparProvisioningTokenLimitReached
  deriving (Eq, Show)

sparToServantErr :: SparError -> ServantErr
sparToServantErr = either id waiToServant . sparToWaiError

waiToServant :: Wai.Error -> ServantErr
waiToServant waierr@(Wai.Error status label _) = ServantErr
  { errHTTPCode     = statusCode status
  , errReasonPhrase = cs label
  , errBody         = encode waierr
  , errHeaders      = []
  }

sparToWaiError :: SparError -> Either ServantErr Wai.Error
sparToWaiError (SAML.CustomError (SparNoRequestRefInResponse msg))        = Right $ Wai.Error status400 "server-error-unsupported-saml" ("The IdP needs to provide an InResponseTo attribute in the assertion: " <> msg)
sparToWaiError (SAML.CustomError (SparCouldNotSubstituteSuccessURI msg))  = Right $ Wai.Error status400 "bad-success-redirect" ("re-parsing the substituted URI failed: " <> msg)
sparToWaiError (SAML.CustomError (SparCouldNotSubstituteFailureURI msg))  = Right $ Wai.Error status400 "bad-failure-redirect" ("re-parsing the substituted URI failed: " <> msg)
sparToWaiError (SAML.CustomError (SparBadInitiateLoginQueryParams label)) = Right $ Wai.Error status400 label label
sparToWaiError (SAML.CustomError (SparBindFromWrongOrNoTeam msg))         = Right $ Wai.Error status403 "bad-team" ("Forbidden: wrong user team " <> msg)
sparToWaiError (SAML.CustomError SparBindUserRefTaken)                    = Right $ Wai.Error status403 "subject-id-taken" "Forbidden: SubjectID is used by another wire user.  If you have an old user bound to this IdP, unbind or delete that user."

sparToWaiError (SAML.CustomError (SparBadUserName msg))                   = Right $ Wai.Error status400 "bad-username" ("Bad UserName in SAML response, except len [1, 128]: " <> msg)
-- Brig-specific errors
sparToWaiError (SAML.CustomError SparNoBodyInBrigResponse)                = Right $ Wai.Error status502 "bad-upstream" "Failed to get a response from an upstream server."
sparToWaiError (SAML.CustomError (SparCouldNotParseBrigResponse msg))     = Right $ Wai.Error status502 "bad-upstream" ("Could not parse response body: " <> msg)
sparToWaiError (SAML.CustomError (SparBrigError msg))                     = Right $ Wai.Error status500 "bad-upstream" msg
-- Galley-specific errors
sparToWaiError (SAML.CustomError SparNoBodyInGalleyResponse)              = Right $ Wai.Error status502 "bad-upstream" "Failed to get a response from an upstream server."
sparToWaiError (SAML.CustomError (SparCouldNotParseGalleyResponse msg))   = Right $ Wai.Error status502 "bad-upstream" ("Could not parse response body: " <> msg)
sparToWaiError (SAML.CustomError (SparGalleyError msg))                   = Right $ Wai.Error status500 "bad-upstream" msg
sparToWaiError (SAML.CustomError SparCouldNotRetrieveCookie)              = Right $ Wai.Error status502 "bad-upstream" "Unable to get a cookie from an upstream server."
sparToWaiError (SAML.CustomError (SparCassandraError msg))                = Right $ Wai.Error status500 "server-error" msg  -- TODO: should we be more specific here and make it 'db-error'?
sparToWaiError (SAML.CustomError (SparCassandraTTLError ttlerr))          = Right $ Wai.Error status400 "ttl-error" (cs $ show ttlerr)
sparToWaiError (SAML.UnknownIdP _msg)                                     = Right $ Wai.Error status404 "not-found" "IdP not found."
sparToWaiError (SAML.Forbidden msg)                                       = Right $ Wai.Error status403 "forbidden" ("Forbidden: " <> msg)
sparToWaiError (SAML.BadSamlResponse msg)                                 = Right $ Wai.Error status400 "no-matching-auth-req" ("Missing auth request: " <> msg) -- (need to fix this: it could also be an error parsing the response)
sparToWaiError (SAML.CustomError SparNotFound)                            = Right $ Wai.Error status404 "not-found" "Could not find IdP."
sparToWaiError (SAML.CustomError SparMissingZUsr)                         = Right $ Wai.Error status400 "client-error" "[header] 'Z-User' required"
sparToWaiError (SAML.CustomError SparNotInTeam)                           = Right $ Wai.Error status403 "no-team-member" "Requesting user is not a team member or not a member of this team."
sparToWaiError (SAML.CustomError SparNotTeamOwner)                        = Right $ Wai.Error status403 "insufficient-permissions" "You need to be team owner to create an IdP."
sparToWaiError (SAML.CustomError SparInitLoginWithAuth)                   = Right $ Wai.Error status403 "login-with-auth" "This end-point is only for login, not binding."
sparToWaiError (SAML.CustomError SparInitBindWithoutAuth)                 = Right $ Wai.Error status403 "bind-without-auth" "This end-point is only for binding, not login."
sparToWaiError (SAML.CustomError SparBindUserDisappearedFromBrig)         = Right $ Wai.Error status404 "bind-user-disappeared" "Your user appears to have been deleted?"
sparToWaiError SAML.UnknownError                                          = Right $ Wai.Error status500 "server-error" "Unknown server error."
sparToWaiError (SAML.BadServerConfig msg)                                 = Right $ Wai.Error status500 "server-error" ("Error in server config: " <> msg)
sparToWaiError (SAML.InvalidCert msg)                                     = Right $ Wai.Error status500 "invalid-certificate" ("Error in idp certificate: " <> msg)
-- Errors related to IdP creation
sparToWaiError (SAML.CustomError (SparNewIdPBadMetaUrl msg))              = Right $ Wai.Error status400 "idp-error" ("Bad or unresponsive metadata url: " <> msg)
sparToWaiError (SAML.CustomError SparNewIdPBadMetaSig)                    = Right $ Wai.Error status400 "invalid-signature" "bad metadata signature"
sparToWaiError (SAML.CustomError (SparNewIdPBadReqUrl msg))               = Right $ Wai.Error status400 "invalid-req-url" ("bad request url: " <> msg)
sparToWaiError (SAML.CustomError SparNewIdPPubkeyMismatch)                = Right $ Wai.Error status400 "key-mismatch" "public keys in body, metadata do not match"
sparToWaiError (SAML.CustomError SparNewIdPAlreadyInUse)                  = Right $ Wai.Error status400 "idp-already-in-use" "an idp issuer can only be used within one team"
sparToWaiError (SAML.CustomError (SparNewIdPWantHttps msg))               = Right $ Wai.Error status400 "idp-must-be-https" ("an idp request uri must be https, not http or other: " <> msg)
-- Errors related to provisioning
sparToWaiError (SAML.CustomError (SparProvisioningNoSingleIdP msg))       = Right $ Wai.Error status400 "no-single-idp" ("Team should have exactly one IdP configured: " <> msg)
sparToWaiError (SAML.CustomError SparProvisioningTokenLimitReached)       = Right $ Wai.Error status403 "token-limit-reached" "The limit of provisioning tokens per team has been reached"
-- Other
sparToWaiError (SAML.CustomServant err)                                   = Left err
