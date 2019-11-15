module Brig.API.Handler
    ( -- * Handler Monad
      Handler
    , runHandler

      -- * Utilities
    , JSON
    , parseJsonBody
    , checkWhitelist
    ) where

import Imports
import Bilge (RequestId (..))
import Brig.App (Env, AppIO, runAppT, requestId, applog, settings)
import Brig.Options (setWhitelist, setInternalWhitelist)
import Brig.API.Error
import Brig.Email (Email)
import Brig.Phone (Phone, PhoneException (..))
import Control.Error
import Control.Lens (set, view)
import Control.Monad.Catch (catches, throwM)
import Data.Aeson (FromJSON)
import Data.Default (def)
import Network.Wai.Predicate (Media)
import Network.Wai (Request, ResponseReceived)
import Network.Wai.Routing (Continue)
import Network.Wai.Utilities.Error ((!>>))
import Network.Wai.Utilities.Request (JsonRequest, lookupRequestId, parseBody)
import Network.Wai.Utilities.Response (setStatus, json, addHeader)
import System.Logger.Class (Logger)

import qualified Brig.AWS                     as AWS
import qualified Brig.Whitelist               as Whitelist
import qualified Control.Monad.Catch          as Catch
import qualified Network.Wai.Utilities.Error  as WaiError
import qualified Network.Wai.Utilities.Server as Server
import qualified Data.ZAuth.Validation        as ZV

-------------------------------------------------------------------------------
-- HTTP Handler Monad

type Handler = ExceptT Error AppIO

runHandler :: Env -> Request -> Handler ResponseReceived -> Continue IO -> IO ResponseReceived
runHandler e r h k = do
    let e' = set requestId (maybe def RequestId (lookupRequestId r)) e
    a <- runAppT e' (runExceptT h) `catches` errors
    either (onError (view applog e') r k) return a
  where
    errors =
        [ Catch.Handler $ \(ex :: PhoneException) ->
            pure (Left (phoneError ex))
        , Catch.Handler $ \(ex :: ZV.Failure) ->
            pure (Left (zauthError ex))
        , Catch.Handler $ \(ex :: AWS.Error) ->
            case ex of
                AWS.SESInvalidDomain -> pure (Left (StdError invalidEmail))
                _                    -> throwM ex
        ]

onError :: Logger -> Request -> Continue IO -> Error -> IO ResponseReceived
onError g r k e = do
    Server.logError g (Just r) we
    Server.flushRequestBody r
    k $ setStatus (WaiError.code we)
      . appEndo (foldMap (Endo . uncurry addHeader) hs)
      $ json e
  where
    (we, hs) = case e of
        StdError  x     -> (x, [])
        RichError x _ h -> (x, h)

-------------------------------------------------------------------------------
-- Utilities

-- TODO: move to libs/wai-utilities?
type JSON = Media "application" "json"

-- TODO: move to libs/wai-utilities?  there is a parseJson' in "Network.Wai.Utilities.Request",
-- but adjusting its signature to this here would require to move more code out of brig (at least
-- badRequest and probably all the other errors).
parseJsonBody :: FromJSON a => JsonRequest a -> Handler a
parseJsonBody req = parseBody req !>> StdError . badRequest

-- | If a whitelist is configured, consult it, otherwise a no-op. {#RefActivationWhitelist}
checkWhitelist :: Either Email Phone -> Handler ()
checkWhitelist key = do
    wex <- setWhitelist <$> view settings
    win <- setInternalWhitelist <$> view settings
    ok <- case (wex, win) of (Just b, _) -> Whitelist.verifyService b key
                             (_, Just b) -> pure $ Whitelist.verifyInternal b key
                             _           -> pure True
    unless ok (throwStd whitelistError)
