{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Spar.Error where

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

  | SparNoBodyInBrigResponse
  | SparCouldNotParseBrigResponse
  | SparCouldNotRetrieveCookie
  | SparCassandraError LT

  | SparNewIdPBadMetaUrl LT
  | SparNewIdPBadMetaSig
  | SparNewIdPBadReqUrl LT
  | SparNewIdPPubkeyMismatch
  deriving (Eq, Show)

instance ToJSON SparError where
  toJSON = toJSON . sparToWaiError

sparToServantErr :: SparError -> ServantErr
sparToServantErr err = case sparToWaiError err of
  waierr@(Wai.Error status label _) -> ServantErr
    { errHTTPCode     = statusCode status
    , errReasonPhrase = cs label
    , errBody         = encode waierr
    , errHeaders      = []
    }

sparToWaiError :: SparError -> Wai.Error
sparToWaiError (SAML.UnknownIdP _msg)                           = sparToWaiError $ SAML.CustomError SparNotInTeam
sparToWaiError (SAML.Forbidden msg)                             = Wai.Error status403 "forbidden" ("Forbidden: " <> msg)
sparToWaiError (SAML.BadSamlResponse msg)                       = Wai.Error status400 "client-error" ("Invalid credentials: " <> msg)
sparToWaiError (SAML.BadServerConfig msg)                       = Wai.Error status500 "server-error" ("Error in server config: " <> msg)
sparToWaiError SAML.UnknownError                                = Wai.Error status500 "server-error" "Unknown server error."
sparToWaiError (SAML.CustomError SparNotFound)                  = Wai.Error status404 "not-found" "Not found."
sparToWaiError (SAML.CustomError SparNotInTeam)                 = Wai.Error status404 "not-found" "Not found."
sparToWaiError (SAML.CustomError SparNotTeamOwner)              = Wai.Error status403 "forbidden" "You need to be team owner to create an IdP."
sparToWaiError (SAML.CustomError SparNoBodyInBrigResponse)      = Wai.Error status400 "server-error" "Brig response without body."
sparToWaiError (SAML.CustomError SparCouldNotParseBrigResponse) = Wai.Error status400 "server-error" "Could not parse brig response body."
sparToWaiError (SAML.CustomError SparCouldNotRetrieveCookie)    = Wai.Error status400 "server-error" "Brig response contained no Set-Cookie header."
sparToWaiError (SAML.CustomError (SparCassandraError msg))      = Wai.Error status500 "server-error" ("Cassandra error: " <> msg)
sparToWaiError (SAML.CustomError (SparNewIdPBadMetaUrl msg))    = Wai.Error status400 "client-error" ("bad or unresponsive metadata url: " <> msg)
sparToWaiError (SAML.CustomError SparNewIdPBadMetaSig)          = Wai.Error status400 "client-error" "bad metadata signature"
sparToWaiError (SAML.CustomError (SparNewIdPBadReqUrl msg))     = Wai.Error status400 "client-error" ("bad request url: " <> msg)
sparToWaiError (SAML.CustomError SparNewIdPPubkeyMismatch)      = Wai.Error status400 "client-error" "public keys in body, metadata do not match"
