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

module Brig.API.Handler
  ( -- * Handler Monad
    Handler,
    runHandler,
    toServantHandler,

    -- * Utilities
    JSON,
    parseJsonBody,
    checkWhitelist,
    checkWhitelistWithError,
    isWhiteListed,
    UserNotAllowedToJoinTeam (..),
  )
where

import Bilge (MonadHttp, RequestId (..))
import Brig.API.Error
import qualified Brig.AWS as AWS
import Brig.App
import Brig.CanonicalInterpreter (BrigCanonicalEffects, runBrigToIO)
import Brig.Email (Email)
import Brig.Options (setWhitelist)
import Brig.Phone (Phone, PhoneException (..))
import qualified Brig.Whitelist as Whitelist
import Control.Error
import Control.Exception (throwIO)
import Control.Lens (set, view)
import Control.Monad.Catch (catches, throwM)
import qualified Control.Monad.Catch as Catch
import Control.Monad.Except (MonadError, throwError)
import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import Data.Default (def)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ZAuth.Validation as ZV
import Imports
import Network.HTTP.Types (Status (statusCode, statusMessage))
import Network.Wai (Request, ResponseReceived)
import Network.Wai.Predicate (Media)
import Network.Wai.Routing (Continue)
import Network.Wai.Utilities.Error ((!>>))
import qualified Network.Wai.Utilities.Error as WaiError
import Network.Wai.Utilities.Request (JsonRequest, lookupRequestId, parseBody)
import Network.Wai.Utilities.Response (addHeader, json, setStatus)
import qualified Network.Wai.Utilities.Server as Server
import qualified Servant
import qualified System.Logger as Log
import System.Logger.Class (Logger)
import Wire.API.Error
import Wire.API.Error.Brig

-------------------------------------------------------------------------------
-- HTTP Handler Monad

type Handler r = ExceptT Error (AppT r)

runHandler ::
  Env ->
  Request ->
  (Handler BrigCanonicalEffects) ResponseReceived ->
  Continue IO ->
  IO ResponseReceived
runHandler e r h k = do
  let e' = set requestId (maybe def RequestId (lookupRequestId r)) e
  a <-
    runBrigToIO e' (runExceptT h)
      `catches` brigErrorHandlers (view applog e) (unRequestId (view requestId e))
  either (onError (view applog e') r k) pure a

toServantHandler :: Env -> (Handler BrigCanonicalEffects) a -> Servant.Handler a
toServantHandler env action = do
  let logger = view applog env
      reqId = unRequestId $ view requestId env
  a <-
    liftIO $
      runBrigToIO env (runExceptT action)
        `catches` brigErrorHandlers logger reqId
  case a of
    Left werr -> handleWaiErrors logger reqId werr
    Right x -> pure x
  where
    mkCode = statusCode . WaiError.code
    mkPhrase = Text.unpack . Text.decodeUtf8 . statusMessage

    handleWaiErrors logger reqId =
      \case
        -- throw in IO so that the `catchErrors` middleware can turn this error
        -- into a response and log accordingly
        StdError werr -> liftIO $ throwIO werr
        RichError werr body headers -> do
          when (statusCode (WaiError.code werr) < 500) $
            -- 5xx are logged by the middleware, so we only log errors < 500 to avoid duplicated entries
            Server.logError' logger (Just reqId) werr
          Servant.throwError $
            Servant.ServerError (mkCode werr) (mkPhrase (WaiError.code werr)) (Aeson.encode body) headers

newtype UserNotAllowedToJoinTeam = UserNotAllowedToJoinTeam WaiError.Error
  deriving (Show)

instance Exception UserNotAllowedToJoinTeam

brigErrorHandlers :: Logger -> ByteString -> [Catch.Handler IO (Either Error a)]
brigErrorHandlers logger reqId =
  [ Catch.Handler $ \(ex :: PhoneException) ->
      pure (Left (phoneError ex)),
    Catch.Handler $ \(ex :: ZV.Failure) ->
      pure (Left (zauthError ex)),
    Catch.Handler $ \(ex :: AWS.Error) ->
      case ex of
        AWS.SESInvalidDomain -> pure (Left (StdError (errorToWai @'InvalidEmail)))
        _ -> throwM ex,
    Catch.Handler $ \(UserNotAllowedToJoinTeam e) -> pure (Left $ StdError e),
    Catch.Handler $ \(e :: SomeException) -> do
      Log.err logger $
        Log.msg ("IO Exception occurred" :: ByteString)
          . Log.field "message" (displayException e)
          . Log.field "request" reqId
      throwIO e
  ]

onError :: Logger -> Request -> Continue IO -> Error -> IO ResponseReceived
onError g r k e = do
  Server.logError g (Just r) we
  -- This function exists to workaround a problem that existed in nginx 5 years
  -- ago. Context here:
  -- https://github.com/zinfra/wai-utilities/commit/3d7e8349d3463e5ee2c3ebe89c717baeef1a8241
  -- So, this can probably be deleted and is not part of the new servant
  -- handler.
  Server.flushRequestBody r
  k $
    setStatus (WaiError.code we)
      . appEndo (foldMap (Endo . uncurry addHeader) hs)
      $ json e
  where
    (we, hs) = case e of
      StdError x -> (x, [])
      RichError x _ h -> (x, h)

-------------------------------------------------------------------------------
-- Utilities

-- TODO: move to libs/wai-utilities?
type JSON = Media "application" "json"

-- TODO: move to libs/wai-utilities?  there is a parseJson' in "Network.Wai.Utilities.Request",
-- but adjusting its signature to this here would require to move more code out of brig (at least
-- badRequest and probably all the other errors).
parseJsonBody :: (FromJSON a, MonadIO m) => JsonRequest a -> ExceptT Error m a
parseJsonBody req = parseBody req !>> StdError . badRequest

-- | If a whitelist is configured, consult it, otherwise a no-op. {#RefActivationWhitelist}
checkWhitelist :: Either Email Phone -> (Handler r) ()
checkWhitelist = wrapHttpClientE . checkWhitelistWithError (StdError whitelistError)

checkWhitelistWithError :: (Monad m, MonadReader Env m, MonadIO m, Catch.MonadMask m, MonadHttp m, MonadError e m) => e -> Either Email Phone -> m ()
checkWhitelistWithError e key = do
  ok <- isWhiteListed key
  unless ok (throwError e)

isWhiteListed :: (Monad m, MonadReader Env m, MonadIO m, Catch.MonadMask m, MonadHttp m) => Either Email Phone -> m Bool
isWhiteListed key = do
  eb <- setWhitelist <$> view settings
  case eb of
    Nothing -> pure True
    Just b -> Whitelist.verify b key
