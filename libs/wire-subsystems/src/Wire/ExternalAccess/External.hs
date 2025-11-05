-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.ExternalAccess.External (interpretExternalAccess, ExtEnv (..)) where

import Bilge.Request
import Bilge.Retry (httpHandlers)
import Control.Concurrent.Async (Async)
import Control.Exception (try)
import Control.Lens
import Control.Retry
import Data.ByteString.Conversion.To
import Data.Id
import Data.Misc
import Imports
import Network.HTTP.Client qualified as Http
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status (status410)
import OpenSSL.Session qualified as Ssl
import Polysemy
import Polysemy.Async qualified as Async
import Polysemy.TinyLog
import Ssl.Util (withVerifiedSslConnection)
import System.Logger.Message (field, msg, val, (~~))
import URI.ByteString
import Wire.API.Bot.Service
import Wire.API.Event.Conversation (Event)
import Wire.API.Provider.Service (serviceRefId, serviceRefProvider)
import Wire.BrigAPIAccess
import Wire.ExternalAccess (ExternalAccess (..))
import Wire.FireAndForget
import Wire.ServiceStore
import Wire.StoredConversation (BotMember, botMemId, botMemService)
import Wire.Util

data ExtEnv = ExtEnv
  { extGetManager :: (Http.Manager, [Fingerprint Rsa] -> Ssl.SSL -> IO ())
  }

interpretExternalAccess ::
  ( Member TinyLog r,
    Member BrigAPIAccess r,
    Member FireAndForget r,
    Member ServiceStore r,
    Member (Final IO) r,
    Member Async.Async r
  ) =>
  ExtEnv ->
  Sem (ExternalAccess ': r) a ->
  Sem r a
interpretExternalAccess env = interpret $ \case
  Deliver pp -> do
    logEffect "ExternalAccess.Deliver"
    deliver env (toList pp)
  DeliverAsync pp -> do
    logEffect "ExternalAccess.DeliverAsync"
    deliverAsync env (toList pp)
  DeliverAndDeleteAsync cid pp -> do
    logEffect "ExternalAccess.DeliverAndDeleteAsync"
    deliverAndDeleteAsync env cid (toList pp)

-- | Like deliver, but ignore orphaned bots and return immediately.
--
-- FUTUREWORK: Check if this can be removed.
deliverAsync ::
  ( Member FireAndForget r,
    Member TinyLog r,
    Member ServiceStore r,
    Member (Final IO) r,
    Member Async.Async r
  ) =>
  ExtEnv ->
  [(BotMember, Event)] ->
  Sem r ()
deliverAsync env = fireAndForget . void . deliver env

-- | Like deliver, but remove orphaned bots and return immediately.
deliverAndDeleteAsync ::
  ( Member BrigAPIAccess r,
    Member FireAndForget r,
    Member TinyLog r,
    Member ServiceStore r,
    Member (Final IO) r,
    Member Async.Async r
  ) =>
  ExtEnv ->
  ConvId ->
  [(BotMember, Event)] ->
  Sem r ()
deliverAndDeleteAsync env cnv pushes = fireAndForget $ do
  gone <- deliver env pushes
  mapM_ (deleteBot cnv . botMemId) gone

deliver ::
  forall r.
  ( Member ServiceStore r,
    Member TinyLog r,
    Member Async.Async r,
    Member (Final IO) r
  ) =>
  ExtEnv ->
  [(BotMember, Event)] ->
  Sem r [BotMember]
deliver env pp = mapM (Async.async . exec) pp >>= foldM eval [] . zip (map fst pp)
  where
    exec :: (BotMember, Event) -> Sem r (Either SomeException Bool)
    exec (b, e) =
      getService (botMemService b) >>= \case
        Nothing -> pure $ Right False
        Just s ->
          embedFinal $ try $ (deliver1 env s b e $> True)
    eval :: [BotMember] -> (BotMember, Async (Maybe (Either SomeException Bool))) -> Sem r [BotMember]
    eval gone (b, a) = do
      let s = botMemService b
      r <- Async.await a
      case r of
        Just (Right True) -> do
          debug $
            field "provider" (toByteString (s ^. serviceRefProvider))
              ~~ field "service" (toByteString (s ^. serviceRefId))
              ~~ field "bot" (toByteString (botMemId b))
              ~~ msg (val "External delivery success")
          pure gone
        Just (Right False) -> do
          debug $
            field "provider" (toByteString (s ^. serviceRefProvider))
              ~~ field "service" (toByteString (s ^. serviceRefId))
              ~~ field "bot" (toByteString (botMemId b))
              ~~ msg (val "External service gone")
          pure (b : gone)
        Just (Left ex)
          | Just (Http.HttpExceptionRequest _ (Http.StatusCodeException rs _)) <- fromException ex,
            Http.responseStatus rs == status410 -> do
              debug $
                field "provider" (toByteString (s ^. serviceRefProvider))
                  ~~ field "service" (toByteString (s ^. serviceRefId))
                  ~~ field "bot" (toByteString (botMemId b))
                  ~~ msg (val "External bot gone")
              pure (b : gone)
        Just (Left ex) -> do
          info $
            field "provider" (toByteString (s ^. serviceRefProvider))
              ~~ field "service" (toByteString (s ^. serviceRefId))
              ~~ field "bot" (toByteString (botMemId b))
              ~~ field "error" (show ex)
              ~~ msg (val "External delivery failure")
          pure gone
        Nothing -> do
          info $
            field "provider" (toByteString (s ^. serviceRefProvider))
              ~~ field "service" (toByteString (s ^. serviceRefId))
              ~~ field "bot" (toByteString (botMemId b))
              ~~ msg (val "External delivery failure due to local error in Async")
          pure gone

-- Internal -------------------------------------------------------------------

deliver1 :: ExtEnv -> Service -> BotMember -> Event -> IO ()
deliver1 env s bm e
  | s ^. serviceEnabled = do
      let t = toByteString' (s ^. serviceToken)
      let u = s ^. serviceUrl
      let b = botMemId bm
      let HttpsUrl url = u
      recovering x3 httpHandlers $
        const $
          sendMessage env (s ^. serviceFingerprints) $
            method POST
              . maybe id host (urlHost u)
              . maybe (port 443) port (urlPort u)
              . paths [url ^. pathL, "bots", toByteString' b, "messages"]
              . header "Authorization" ("Bearer " <> t)
              . json e
              . timeout 5000
              . secure
              . expect2xx
  | otherwise = pure ()

urlHost :: HttpsUrl -> Maybe ByteString
urlHost (HttpsUrl u) = u ^. authorityL <&> view (authorityHostL . hostBSL)

urlPort :: HttpsUrl -> Maybe Word16
urlPort (HttpsUrl u) = do
  a <- u ^. authorityL
  p <- a ^. authorityPortL
  pure (fromIntegral (p ^. portNumberL))

sendMessage :: ExtEnv -> [Fingerprint Rsa] -> (Request -> Request) -> IO ()
sendMessage env fprs reqBuilder = do
  let (man, verifyFingerprints) = env.extGetManager
  liftIO . withVerifiedSslConnection (verifyFingerprints fprs) man reqBuilder $ \req ->
    Http.withResponse req man (const $ pure ())

x3 :: RetryPolicy
x3 = limitRetries 3 <> constantDelay 1000000
