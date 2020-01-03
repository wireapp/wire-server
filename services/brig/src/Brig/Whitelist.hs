-- | > docs/reference/user/activation.md {#RefActivationWhitelist}
--
-- Email/phone whitelist.
module Brig.Whitelist
    ( verifyInternal
    , verifyService
    ) where

import Imports
import Brig.App (AppIO)
import Bilge.IO
import Bilge.Request
import Bilge.Response
import Bilge.Retry
import Brig.Types
import Brig.Options (Whitelist (..), InternalWhitelist (..))
import Control.Monad.Catch (MonadMask, throwM)
import Control.Retry
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..), parseRequest)

import qualified Brig.API.User as User
import qualified Data.Text as T

-- | Check internal whitelist for given email/phone number. Currently this
-- consults a statically configured whitelist of allowed email domains..
verifyInternal :: InternalWhitelist -> Either Email Phone -> AppIO Bool
verifyInternal (InternalWhitelist domains) key =
    let allowFromDomains (Left e) = (emailDomain e) `elem` domains
        allowFromDomains _        = False

    in liftA2 (||) (pure $ allowFromDomains key) (User.isWhitelisted key)

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
