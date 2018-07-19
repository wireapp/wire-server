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
import qualified Text.XML.Util as SAML
import qualified URI.ByteString as URI


-- | TODO: this should probably be moved to the Options yaml file.
errorRenderURIPrefix :: URI.URI
Right errorRenderURIPrefix = SAML.parseURI' "https://app.wire.com/report-error"

errorRenderURI :: Wai.Error -> SBS
errorRenderURI = undefined


type SparError = SAML.Error SparCustomError

-- | Throw an error that should be shown to the user (@F@ is for @Frontend@).
throwSparF :: MonadError SparError m => FrontendError -> m a
throwSparF = throwError . SAML.CustomError . FrontendError

-- | Throw an error that should result in a @>= 400@ response code and will be processed by the
-- client (@B@ is for @Backend@).
throwSparB :: MonadError SparError m => BackendError -> m a
throwSparB = throwError . SAML.CustomError . BackendError

type SparCustomError = SparCustomError' FrontendError BackendError

data SparCustomError' f b
  = FrontendError f
  | BackendError b
  deriving (Eq, Show)

data FrontendError
  = SparBadUserName LT
  | SparNoBodyInBrigResponse
  | SparCouldNotParseBrigResponse LT
  | SparCouldNotRetrieveCookie
  | SparCassandraError LT
  deriving (Eq, Show)

data BackendError
  = SparTeamNotFound
  | SparNotInTeam
  | SparNotTeamOwner
  | SparNewIdPBadMetaUrl LT
  | SparNewIdPBadMetaSig
  | SparNewIdPBadReqUrl LT
  | SparNewIdPPubkeyMismatch
  deriving (Eq, Show)

instance ToJSON SparError where
  toJSON = toJSON . sparToWaiError

sparToServantErr :: SparError -> ServantErr
sparToServantErr err = case sparToWaiError err of
  BackendError (waierr@(Wai.Error status label _)) -> ServantErr
    { errHTTPCode     = statusCode status
    , errReasonPhrase = cs label
    , errBody         = encode waierr
    , errHeaders      = []
    }

  FrontendError (waierr@(Wai.Error status label _)) -> ServantErr
    { errHTTPCode     = 302
    , errReasonPhrase = cs label
    , errBody         = encode waierr
    , errHeaders      = [("Location", errorRenderURI waierr)]
    }


-- | (Errors from saml2-web-sso are all frontend errors.)
sparToWaiError :: SparError -> SparCustomError' Wai.Error Wai.Error
sparToWaiError (SAML.UnknownIdP _msg)               = FrontendError $ Wai.Error status404 "not-found" "IdP not found."
sparToWaiError (SAML.Forbidden msg)                 = FrontendError $ Wai.Error status403 "forbidden" ("Forbidden: " <> msg)
sparToWaiError (SAML.BadSamlResponse msg)           = FrontendError $ Wai.Error status400 "client-error" ("Invalid credentials: " <> msg)
sparToWaiError (SAML.BadServerConfig msg)           = FrontendError $ Wai.Error status500 "server-error" ("Error in server config: " <> msg)
sparToWaiError SAML.UnknownError                    = FrontendError $ Wai.Error status500 "server-error" "Unknown server error."

sparToWaiError (SAML.CustomError (FrontendError f)) = FrontendError $ sparToWaiErrorF f
sparToWaiError (SAML.CustomError (BackendError b))  = BackendError  $ sparToWaiErrorB b

sparToWaiErrorF :: FrontendError -> Wai.Error
sparToWaiErrorF = undefined

sparToWaiErrorB :: BackendError -> Wai.Error
sparToWaiErrorB = undefined


{-
sparToWaiError (SAML.CustomError SparTeamNotFound)                  = Wai.Error status404 "not-found" "Not found."
sparToWaiError (SAML.CustomError SparNotInTeam)                 = Wai.Error status404 "not-found" "Not found."
sparToWaiError (SAML.CustomError SparNotTeamOwner)              = Wai.Error status403 "forbidden" "You need to be team owner to create an IdP."
sparToWaiError (SAML.CustomError (SparBadUserName msg))         = Wai.Error status400 "client-error" ("Bad UserName in SAML response: " <> msg)
sparToWaiError (SAML.CustomError SparNoBodyInBrigResponse)      = Wai.Error status400 "server-error" "Brig response without body."
sparToWaiError (SAML.CustomError (SparCouldNotParseBrigResponse msg)) = Wai.Error status400 "server-error" ("Could not parse brig response body: " <> msg)
sparToWaiError (SAML.CustomError SparCouldNotRetrieveCookie)    = Wai.Error status400 "server-error" "Brig response contained no Set-Cookie header."
sparToWaiError (SAML.CustomError (SparCassandraError msg))      = Wai.Error status500 "server-error" ("Cassandra error: " <> msg)
sparToWaiError (SAML.CustomError (SparNewIdPBadMetaUrl msg))    = Wai.Error status400 "client-error" ("bad or unresponsive metadata url: " <> msg)
sparToWaiError (SAML.CustomError SparNewIdPBadMetaSig)          = Wai.Error status400 "client-error" "bad metadata signature"
sparToWaiError (SAML.CustomError (SparNewIdPBadReqUrl msg))     = Wai.Error status400 "client-error" ("bad request url: " <> msg)
sparToWaiError (SAML.CustomError SparNewIdPPubkeyMismatch)      = Wai.Error status400 "client-error" "public keys in body, metadata do not match"


-- make labels unique.  look at galley.  https://github.com/wireapp/wire-server/blob/develop/services/galley/src/Galley/API/Error.hs#L94
-- server errrors should be status500, not 400.
-}
