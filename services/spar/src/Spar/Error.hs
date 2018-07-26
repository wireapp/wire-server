{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Spar.Error
  ( SparError
  , SparCustomError(..)
  , throwSpar
  , sparToServantErr
  ) where

import Control.Monad.Except
import Data.Aeson
import Data.String.Conversions
import Network.HTTP.Types.Status
import Servant

import qualified Network.Wai.Utilities.Error as Wai
import qualified SAML2.WebSSO as SAML


type SparError = SAML.Error SparCustomError

throwSpar :: MonadError SparError m => SparCustomError -> m a
throwSpar = throwError . SAML.CustomError

data SparCustomError
  = SparNotFound
  | SparNotInTeam
  | SparNotTeamOwner

  | SparNoRequestRefInResponse  -- (this is technically legal, but unnecessary, and should probably fixed in saml2-web-sso.)
  | SparCouldNotSubstituteSuccessURI LT
  | SparCouldNotSubstituteFailureURI LT
  | SparBadInitiateLoginQueryParams LT

  | SparBadUserName LT
  | SparNoBodyInBrigResponse
  | SparCouldNotParseBrigResponse LT
  | SparCouldNotRetrieveCookie
  | SparCassandraError LT

  | SparNewIdPBadMetaUrl LT
  | SparNewIdPBadMetaSig
  | SparNewIdPBadReqUrl LT
  | SparNewIdPPubkeyMismatch
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
sparToWaiError (SAML.UnknownIdP _msg)                                     = Right $ Wai.Error status404 "not-found" "Not found."
sparToWaiError (SAML.Forbidden msg)                                       = Right $ Wai.Error status403 "forbidden" ("Forbidden: " <> msg)
sparToWaiError (SAML.BadSamlResponse msg)                                 = Right $ Wai.Error status400 "client-error" ("Invalid credentials: " <> msg)
sparToWaiError (SAML.BadServerConfig msg)                                 = Right $ Wai.Error status500 "server-error" ("Error in server config: " <> msg)
sparToWaiError SAML.UnknownError                                          = Right $ Wai.Error status500 "server-error" "Unknown server error."
sparToWaiError (SAML.CustomServant err)                                   = Left err
-- Errors related to IdP management
sparToWaiError (SAML.CustomError (SparNewIdPBadMetaUrl msg))              = Right $ Wai.Error status400 "idp-error" ("Bad or unresponsive metadata url: " <> msg)
sparToWaiError (SAML.CustomError SparNewIdPBadMetaSig)                    = Right $ Wai.Error status400 "invalid-signature" "bad metadata signature"
sparToWaiError (SAML.CustomError (SparNewIdPBadReqUrl msg))               = Right $ Wai.Error status400 "invalid-req-url" ("bad request url: " <> msg)
sparToWaiError (SAML.CustomError SparNewIdPPubkeyMismatch)                = Right $ Wai.Error status400 "key-mismatch" "public keys in body, metadata do not match"
sparToWaiError (SAML.CustomError SparNotFound)                            = Right $ Wai.Error status404 "not-found" "Could not find IdP."
sparToWaiError (SAML.CustomError SparNotInTeam)                           = Right $ Wai.Error status404 "not-found" "User not in team."
sparToWaiError (SAML.CustomError SparNotTeamOwner)                        = Right $ Wai.Error status403 "insufficient-permissions" "You need to be team owner to create an IdP."
-- Errors related to the log in process
sparToWaiError (SAML.CustomError SparNoRequestRefInResponse)              = Right $ Wai.Error status400 "server-error-unsupported-saml" "The IdP needs to provide an InResponseTo attribute in the top-level element of the response."
sparToWaiError (SAML.CustomError (SparCouldNotSubstituteSuccessURI msg))  = Right $ Wai.Error status400 "bad-success-redirect" ("re-parsing the substituted URI failed: " <> msg)
sparToWaiError (SAML.CustomError (SparCouldNotSubstituteFailureURI msg))  = Right $ Wai.Error status400 "bad-failure-redirect" ("re-parsing the substituted URI failed: " <> msg)
sparToWaiError (SAML.CustomError (SparBadInitiateLoginQueryParams label)) = Right $ Wai.Error status400 label label
sparToWaiError (SAML.CustomError (SparBadUserName msg))                   = Right $ Wai.Error status400 "bad-username" ("Bad UserName in SAML response, except len [1, 128]: " <> msg)
sparToWaiError (SAML.CustomError SparNoBodyInBrigResponse)                = Right $ Wai.Error status502 "bad-upstream" "Failed to get a response from an upstream server."
sparToWaiError (SAML.CustomError (SparCouldNotParseBrigResponse msg))     = Right $ Wai.Error status502 "bad-upstream" ("Could not parse response body: " <> msg)
sparToWaiError (SAML.CustomError SparCouldNotRetrieveCookie)              = Right $ Wai.Error status502 "bad-upstream" "Unable to get a cookie from an upstream server."
sparToWaiError (SAML.CustomError (SparCassandraError msg))                = Right $ Wai.Error status500 "server-error" ("DB error: " <> msg)
