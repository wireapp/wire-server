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

module Galley.External.LegalHoldService.Internal
  ( makeVerifiedRequest,
    makeVerifiedRequestFreshManager,
  )
where

import Bilge qualified
import Bilge.Retry
import Control.Lens (view)
import Control.Monad.Catch
import Control.Retry
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LC8
import Data.Misc
import Galley.API.Error
import Galley.Env
import Galley.Monad
import Imports
import Network.HTTP.Client qualified as Http
import OpenSSL.Session qualified as SSL
import Ssl.Util
import System.Logger.Class qualified as Log
import URI.ByteString (uriPath)

-- | Check that the given fingerprint is valid and make the request over ssl.
-- If the team has a device registered use 'makeLegalHoldServiceRequest' instead.
makeVerifiedRequestWithManager :: Http.Manager -> ([Fingerprint Rsa] -> SSL.SSL -> IO ()) -> Fingerprint Rsa -> HttpsUrl -> (Http.Request -> Http.Request) -> App (Http.Response LC8.ByteString)
makeVerifiedRequestWithManager mgr verifyFingerprints fpr (HttpsUrl url) reqBuilder = do
  let verified = verifyFingerprints [fpr]
  extHandleAll errHandler $ do
    recovering x3 httpHandlers $
      const $
        liftIO $
          withVerifiedSslConnection verified mgr (reqBuilderMods . reqBuilder) $
            \req ->
              Http.httpLbs req mgr
  where
    reqBuilderMods =
      maybe id Bilge.host (Bilge.extHost url)
        . Bilge.port (fromMaybe 443 (Bilge.extPort url))
        . Bilge.secure
        . prependPath (uriPath url)
    errHandler e = do
      Log.info . Log.msg $ "error making request to legalhold service: " <> show e
      throwM (legalHoldServiceUnavailable e)
    prependPath :: ByteString -> Http.Request -> Http.Request
    prependPath pth req = req {Http.path = pth </> Http.path req}
    -- append two paths with exactly one slash
    (</>) :: ByteString -> ByteString -> ByteString
    a </> b = fromMaybe a (BS.stripSuffix "/" a) <> "/" <> fromMaybe b (BS.stripPrefix "/" b)
    x3 :: RetryPolicy
    x3 = limitRetries 3 <> exponentialBackoff 100000
    extHandleAll :: (MonadCatch m) => (SomeException -> m a) -> m a -> m a
    extHandleAll f ma =
      catches
        ma
        [ Handler $ \(ex :: SomeAsyncException) -> throwM ex,
          Handler $ \(ex :: SomeException) -> f ex
        ]

makeVerifiedRequest ::
  Fingerprint Rsa ->
  HttpsUrl ->
  (Http.Request -> Http.Request) ->
  App (Http.Response LC8.ByteString)
makeVerifiedRequest fpr url reqBuilder = do
  (mgr, verifyFingerprints) <- view (extEnv . extGetManager)
  makeVerifiedRequestWithManager mgr verifyFingerprints fpr url reqBuilder

-- | NOTE: Use this function wisely - this creates a new manager _every_ time it is called.
--   We should really _only_ use it in `checkLegalHoldServiceStatus` for the time being because
--   this is where we check for signatures, etc. If we reuse the manager, we are likely to reuse
--   an existing connection which will _not_ cause the new public key to be verified.
makeVerifiedRequestFreshManager ::
  Fingerprint Rsa ->
  HttpsUrl ->
  (Http.Request -> Http.Request) ->
  App (Http.Response LC8.ByteString)
makeVerifiedRequestFreshManager fpr url reqBuilder = do
  ExtEnv (mgr, verifyFingerprints) <- liftIO initExtEnv
  makeVerifiedRequestWithManager mgr verifyFingerprints fpr url reqBuilder
