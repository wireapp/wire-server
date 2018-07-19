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
sparToWaiError (SAML.UnknownIdP _msg)                           = Right $ Wai.Error status404 "not-found" "Not found."
sparToWaiError (SAML.Forbidden msg)                             = Right $ Wai.Error status403 "forbidden" ("Forbidden: " <> msg)
sparToWaiError (SAML.BadSamlResponse msg)                       = Right $ Wai.Error status400 "client-error" ("Invalid credentials: " <> msg)
sparToWaiError (SAML.BadServerConfig msg)                       = Right $ Wai.Error status500 "server-error" ("Error in server config: " <> msg)
sparToWaiError SAML.UnknownError                                = Right $ Wai.Error status500 "server-error" "Unknown server error."
sparToWaiError (SAML.CustomServant err)                         = Left err
sparToWaiError (SAML.CustomError SparNotFound)                  = Right $ Wai.Error status404 "not-found" "Not found."
sparToWaiError (SAML.CustomError SparNotInTeam)                 = Right $ Wai.Error status404 "not-found" "Not found."
sparToWaiError (SAML.CustomError SparNotTeamOwner)              = Right $ Wai.Error status403 "forbidden" "You need to be team owner to create an IdP."
sparToWaiError (SAML.CustomError (SparBadUserName msg))         = Right $ Wai.Error status400 "client-error" ("Bad UserName in SAML response: " <> msg)
sparToWaiError (SAML.CustomError SparNoBodyInBrigResponse)      = Right $ Wai.Error status400 "server-error" "Brig response without body."
sparToWaiError (SAML.CustomError (SparCouldNotParseBrigResponse msg)) = Right $ Wai.Error status400 "server-error" ("Could not parse brig response body: " <> msg)
sparToWaiError (SAML.CustomError SparCouldNotRetrieveCookie)    = Right $ Wai.Error status400 "server-error" "Brig response contained no Set-Cookie header."
sparToWaiError (SAML.CustomError (SparCassandraError msg))      = Right $ Wai.Error status500 "server-error" ("Cassandra error: " <> msg)
sparToWaiError (SAML.CustomError (SparNewIdPBadMetaUrl msg))    = Right $ Wai.Error status400 "client-error" ("bad or unresponsive metadata url: " <> msg)
sparToWaiError (SAML.CustomError SparNewIdPBadMetaSig)          = Right $ Wai.Error status400 "client-error" "bad metadata signature"
sparToWaiError (SAML.CustomError (SparNewIdPBadReqUrl msg))     = Right $ Wai.Error status400 "client-error" ("bad request url: " <> msg)
sparToWaiError (SAML.CustomError SparNewIdPPubkeyMismatch)      = Right $ Wai.Error status400 "client-error" "public keys in body, metadata do not match"
