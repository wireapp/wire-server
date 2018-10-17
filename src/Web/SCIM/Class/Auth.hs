-- | Basic HTTP authentication support. We roll our own mechanism instead of
-- doing 'BasicAuthCheck' because we don't want to muck with Servant
-- contexts.
module Web.SCIM.Class.Auth
    ( Admin (..)
    , AuthDB (..)
    , SCIMAuthData (..)
    ) where

import Data.Aeson
import GHC.Generics
import Servant
import Data.UUID as UUID
import Data.Char
import Control.Monad
import Data.Text.Encoding
import Web.SCIM.Handler
import Data.Text (Text)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base64 as Base64

-- | Someone who is allowed to provision users via SCIM.
data Admin = Admin
  { adminId :: UUID
  } deriving (Eq, Show, Read, Generic)

instance ToJSON Admin
instance FromJSON Admin

-- | An interface that has to be implemented for a server to provide
-- authentication.
class AuthDB m where
  -- | Check whether a set of credentials (UUID and password) corresponds
  -- to any 'Admin'.
  authCheck :: Maybe SCIMAuthData -> SCIMHandler m (BasicAuthResult Admin)

----------------------------------------------------------------------------
-- Auth header

-- | Data contained in the basic auth header.
--
-- TODO: parametrize with 'Admin'
data SCIMAuthData = SCIMAuthData
  { scimAdmin :: UUID
  , scimPassword :: BS.ByteString
  } deriving (Eq, Show)

-- | Decode a basic auth header.
decodeAuth :: BS.ByteString -> Either Text SCIMAuthData
decodeAuth ah = do
  -- This code was copied from http://hackage.haskell.org/package/servant-server-0.14.1/docs/src/Servant.Server.Internal.BasicAuth.html#decodeBAHdr
  let (b, rest) = BS.break isSpace ah
  when (BS.map toLower b /= "basic") $
    Left "expected \"basic\""
  let decoded = Base64.decodeLenient (BS.dropWhile isSpace rest)
  let (username, passWithColonAtHead) = BS.break (== ':') decoded
  admin <-
    maybe (Left "couldn't decode the username as UUID") Right $
    UUID.fromASCIIBytes username
  (_, password) <-
    maybe (Left "expected username:password, but ':' was not found") Right $
    BS.uncons passWithColonAtHead
  return (SCIMAuthData admin password)

instance FromHttpApiData SCIMAuthData where
  parseUrlPiece = decodeAuth . encodeUtf8
  parseHeader = decodeAuth
