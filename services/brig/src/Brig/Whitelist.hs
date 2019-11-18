-- | > docs/reference/user/activation.md {#RefActivationWhitelist}
--
-- Email/phone whitelist.
module Brig.Whitelist
    ( Whitelist (..)
    , InternalWhitelist (..)
    , verifyInternal
    , verifyService
    ) where

import Imports
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Bilge.IO
import Bilge.Request
import Bilge.Response
import Bilge.Retry
import Brig.Types
import Control.Monad.Catch (MonadMask, throwM)
import Control.Retry
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..), parseRequest)
import Util.Options.Common (toOptionFieldName)

import qualified Data.Text as T

-- | A service providing a whitelist of allowed email addresses and phone numbers
-- DEPRECATED: use internal whitelisting instead!
data Whitelist = Whitelist
    { whitelistUrl  :: !Text     -- ^ Service URL
    , whitelistUser :: !Text     -- ^ Service Username (basic auth)
    , whitelistPass :: !Text     -- ^ Service Password (basic auth)

    } deriving (Show, Generic)

instance FromJSON Whitelist

data InternalWhitelist = InternalWhitelist
    { internalWhitelistDomains :: ![Text]} deriving (Show, Generic)

deriveJSON toOptionFieldName 'internalWhitelistDomains

-- | Check internal whitelist for given email/phone number. Currently this
-- consults a statically configured whitelist of allowed email domains..
verifyInternal :: InternalWhitelist -> Either Email Phone -> Bool
verifyInternal (InternalWhitelist domains) key =
    let allowFromDomains (Left e) = (emailDomain e) `elem` domains
        allowFromDomains _        = False
    in allowFromDomains key

-- | Do a request to the whitelist service and verify that the provided email/phone address is
-- whitelisted.
verifyService :: (MonadIO m, MonadMask m, MonadHttp m) => Whitelist -> Either Email Phone -> m Bool
verifyService (Whitelist url user pass) key = if isKnownDomain key
    then return True
    else recovering x3 httpHandlers . const $ do
        rq <- parseRequest $ T.unpack url
        rsp <- get' rq $ req (encodeUtf8 user) (encodeUtf8 pass)
        case statusCode rsp of
            200 -> return True
            404 -> return False
            _   -> throwM $
                HttpExceptionRequest rq (StatusCodeException (rsp { responseBody = () }) mempty)
    where
      -- Duplicate behaviour from verifyLocal, keeping old behaviour for
      -- compatibility.
      isKnownDomain (Left e) = emailDomain e == "wire.com"
      isKnownDomain _        = False

      urlEmail = queryItem "email" . encodeUtf8 . fromEmail
      urlPhone = queryItem "mobile" . encodeUtf8 . fromPhone

      req u p = port 443 . secure
              . either urlEmail urlPhone key
              . applyBasicAuth u p

      x3 = limitRetries 3 <> exponentialBackoff 100000
