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

-- | RPCs towards service providers.
module Brig.Provider.RPC
  ( -- * External RPC
    ServiceError (..),
    createBot,
    -- Internal RPC
    setServiceConn,
    removeServiceConn,
    addBotMember,
    removeBotMember,
  )
where

import Bilge
import Bilge.RPC
import Bilge.Retry (httpHandlers)
import Brig.App
import Brig.Provider.DB (ServiceConn (..))
import Brig.RPC
import Control.Error
import Control.Lens (set, view, (^.))
import Control.Monad.Catch
import Control.Retry (recovering)
import Data.Aeson
import Data.ByteString.Conversion
import Data.Id
import Data.List1 qualified as List1
import Galley.Types.Bot qualified as Galley
import Galley.Types.Bot.Service (serviceEnabled)
import Galley.Types.Bot.Service qualified as Galley
import Imports
import Network.HTTP.Client qualified as Http
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Ssl.Util (withVerifiedSslConnection)
import System.Logger.Class (MonadLogger, field, msg, val, (~~))
import System.Logger.Class qualified as Log
import URI.ByteString
import Wire.API.Event.Conversation qualified as Conv
import Wire.API.Provider (httpsUrl)
import Wire.API.Provider.External
import Wire.API.Provider.Service qualified as Galley
import Wire.Rpc

--------------------------------------------------------------------------------
-- External RPC

data ServiceError
  = ServiceUnavailableWith String
  | ServiceBotConflict

-- | Request a new bot to be created by an external service.
--
-- If the external service is unavailable, returns a specific error
-- or the response body cannot be parsed, a 'ServiceError' is returned.
createBot :: ServiceConn -> NewBotRequest -> ExceptT ServiceError (AppT r) NewBotResponse
createBot scon new = do
  let fprs = toList (sconFingerprints scon)
  (man, verifyFingerprints) <- view extGetManager
  extHandleAll onExc $ do
    rs <- lift $
      wrapHttp $
        recovering x3 httpHandlers $
          const $
            liftIO $
              withVerifiedSslConnection (verifyFingerprints fprs) man reqBuilder $
                \req ->
                  Http.httpLbs req man
    case Bilge.statusCode rs of
      201 -> decodeBytes "External" (responseBody rs)
      409 -> throwE ServiceBotConflict
      _ -> lift (extLogError scon rs) >> throwE (ServiceUnavailableWith $ show rs)
  where
    -- we can't use 'responseJsonEither' instead, because we have a @Response ByteString@
    -- here, not a @Response (Maybe ByteString)@.
    decodeBytes ctx bs = case eitherDecode' bs of
      Left e -> throwM $ ParseException ctx e
      Right a -> pure a
    reqBuilder =
      extReq scon ["bots"]
        . method POST
        . Bilge.json new
    onExc ex = lift (extLogError scon ex) >> throwE (ServiceUnavailableWith $ displayException ex)

extReq :: ServiceConn -> [ByteString] -> Request -> Request
extReq scon ps =
  maybe id host (extHost url)
    . port (fromMaybe 443 (extPort url))
    . header "Authorization" ("Bearer " <> toByteString' tok)
    . paths (url ^. pathL : ps)
    . secure
  where
    url = httpsUrl (sconBaseUrl scon)
    tok = List1.head (sconAuthTokens scon)

extHandleAll :: MonadCatch m => (SomeException -> m a) -> m a -> m a
extHandleAll f ma =
  catches
    ma
    [ Handler $ \(ex :: SomeAsyncException) -> throwM ex,
      Handler $ \(ex :: SomeException) -> f ex
    ]

-- nb. We log these errors on 'Info' level since we're usually not
-- able to do anything about them and don't want to distract from
-- other important errors.
extLogError :: (MonadLogger m, Show e) => ServiceConn -> e -> m ()
extLogError scon e =
  Log.info $
    field "provider" (toByteString pid)
      ~~ field "service" (toByteString sid)
      ~~ field "error" (show e)
      ~~ msg (val "External service error")
  where
    pid = sconProvider scon
    sid = sconService scon

--------------------------------------------------------------------------------
-- Internal RPC

-- | Set service connection information in galley.
setServiceConn :: ServiceConn -> (AppT r) ()
setServiceConn scon = do
  Log.debug $
    remote "galley"
      . field "provider" (toByteString pid)
      . field "service" (toByteString sid)
      . msg (val "Setting service connection")
  void $ wrapHttp $ galleyRequest POST req
  where
    pid = sconProvider scon
    sid = sconService scon
    ref = Galley.newServiceRef sid pid
    url = sconBaseUrl scon
    tok = List1.head (sconAuthTokens scon)
    fps = toList (sconFingerprints scon)
    req =
      path "/i/services"
        . contentJson
        . lbytes (encode svc)
        . expect2xx
    svc =
      Galley.newService ref url tok fps
        & set serviceEnabled (sconEnabled scon)

-- | Remove service connection information from galley.
removeServiceConn ::
  ( MonadReader Env m,
    MonadIO m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m,
    MonadLogger m
  ) =>
  ProviderId ->
  ServiceId ->
  m ()
removeServiceConn pid sid = do
  Log.debug $
    remote "galley"
      . field "provider" (toByteString pid)
      . field "service" (toByteString sid)
      . msg (val "Removing service connection")
  void $ galleyRequest DELETE req
  where
    req =
      path "/i/services"
        . contentJson
        . lbytes (encode (Galley.newServiceRef sid pid))
        . expect2xx

-- | Tell galley to add a service bot as a member to a conversation.
addBotMember ::
  UserId ->
  ConnId ->
  ConvId ->
  BotId ->
  ClientId ->
  ProviderId ->
  ServiceId ->
  (AppT r) Conv.Event
addBotMember zusr zcon conv bot clt pid sid = do
  Log.debug $
    remote "galley"
      . field "provider" (toByteString pid)
      . field "service" (toByteString sid)
      . field "conv" (toByteString conv)
      . field "user" (toByteString zusr)
      . field "bot" (toByteString bot)
      . msg (val "Adding bot member")
  decodeBody "galley" =<< wrapHttp (galleyRequest POST req)
  where
    req =
      path "/i/bots"
        . header "Z-User" (toByteString' zusr)
        . header "Z-Connection" (toByteString' zcon)
        . contentJson
        . lbytes (encode (Galley.addBot (Galley.newServiceRef sid pid) conv bot clt))
        . expect2xx

-- | Tell galley to remove a service bot from a conversation.
removeBotMember ::
  ( MonadHttp m,
    MonadReader Env m,
    MonadIO m,
    MonadMask m,
    HasRequestId m,
    MonadLogger m
  ) =>
  UserId ->
  Maybe ConnId ->
  ConvId ->
  BotId ->
  m (Maybe Conv.Event)
removeBotMember zusr zcon conv bot = do
  Log.debug $
    remote "galley"
      . field "user" (toByteString zusr)
      . field "conv" (toByteString conv)
      . field "bot" (toByteString bot)
      . msg (val "Removing bot member")
  rs <- galleyRequest DELETE req
  if isJust (responseBody rs) && Bilge.statusCode rs == 200
    then Just <$> decodeBody "galley" rs
    else pure Nothing
  where
    req =
      path "/i/bots"
        . header "Z-User" (toByteString' zusr)
        . maybe id (header "Z-Connection" . toByteString') zcon
        . contentJson
        . lbytes (encode (Galley.removeBot conv bot))
        . expect [status200, status404] -- 404 is allowed: a given conversation may no longer exist
