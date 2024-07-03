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
    toServantHandler,

    -- * Utilities
    parseJsonBody,
    checkAllowlist,
    checkAllowlistWithError,
    isAllowlisted,
    UserNotAllowedToJoinTeam (..),
  )
where

import Bilge (RequestId (..))
import Brig.API.Error
import Brig.AWS qualified as AWS
import Brig.App
import Brig.CanonicalInterpreter (BrigCanonicalEffects, runBrigToIO)
import Brig.Options (setAllowlistEmailDomains)
import Control.Error
import Control.Exception (throwIO)
import Control.Lens (view)
import Control.Monad.Catch (catches, throwM)
import Control.Monad.Catch qualified as Catch
import Control.Monad.Except (MonadError, throwError)
import Data.Aeson (FromJSON)
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.ZAuth.Validation qualified as ZV
import Imports
import Network.HTTP.Types (Status (statusCode, statusMessage))
import Network.Wai.Utilities.Error ((!>>))
import Network.Wai.Utilities.Error qualified as WaiError
import Network.Wai.Utilities.Request (JsonRequest, parseBody)
import Network.Wai.Utilities.Server qualified as Server
import Servant qualified
import System.Logger qualified as Log
import System.Logger.Class (Logger)
import Wire.API.Allowlists qualified as Allowlists
import Wire.API.Error
import Wire.API.Error.Brig
import Wire.API.User (Email)

-------------------------------------------------------------------------------
-- HTTP Handler Monad

type Handler r = ExceptT Error (AppT r)

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
  [ Catch.Handler $ \(ex :: ZV.Failure) ->
      pure (Left (zauthError ex)),
    Catch.Handler $ \(ex :: AWS.Error) ->
      case ex of
        AWS.SESInvalidDomain ->
          pure (Left (StdError (errorToWai @'InvalidEmail)))
        _ -> throwM ex,
    Catch.Handler $ \(UserNotAllowedToJoinTeam e) -> pure (Left $ StdError e),
    Catch.Handler $ \(e :: SomeException) -> do
      Log.err logger $
        Log.msg ("IO Exception occurred" :: ByteString)
          . Log.field "message" (displayException e)
          . Log.field "request" reqId
      throwIO e
  ]

-------------------------------------------------------------------------------
-- Utilities

-- This could go to libs/wai-utilities.  There is a `parseJson'` in
-- "Network.Wai.Utilities.Request", but adding `parseJsonBody` there would require to move
-- more code out of brig.
parseJsonBody :: (FromJSON a, MonadIO m) => JsonRequest a -> ExceptT Error m a
parseJsonBody req = parseBody req !>> StdError . badRequest

-- | If an Allowlist is configured, consult it, otherwise a no-op. {#RefActivationAllowlist}
checkAllowlist :: Email -> Handler r ()
checkAllowlist = wrapHttpClientE . checkAllowlistWithError (StdError allowlistError)

checkAllowlistWithError :: (MonadReader Env m, MonadError e m) => e -> Email -> m ()
checkAllowlistWithError e key = do
  ok <- isAllowlisted key
  unless ok (throwError e)

isAllowlisted :: (MonadReader Env m) => Email -> m Bool
isAllowlisted key = do
  env <- view settings
  pure $ Allowlists.verify (setAllowlistEmailDomains env) key
