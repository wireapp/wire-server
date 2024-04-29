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

module Galley.External (interpretExternalAccess) where

import Bilge.Request
import Bilge.Retry (httpHandlers)
import Control.Lens
import Control.Retry
import Data.Aeson (ToJSON)
import Data.ByteString.Conversion.To
import Data.Id
import Data.Misc
import Galley.Cassandra.Services
import Galley.Cassandra.Util
import Galley.Data.Services (BotMember, botMemId, botMemService)
import Galley.Effects
import Galley.Effects.ExternalAccess (ExternalAccess (..))
import Galley.Env
import Galley.Intra.User
import Galley.Monad
import Imports
import Network.HTTP.Client qualified as Http
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status (status410)
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog
import Ssl.Util (withVerifiedSslConnection)
import System.Logger.Class qualified as Log
import System.Logger.Message (field, msg, val, (~~))
import URI.ByteString
import UnliftIO (Async, async, waitCatch)
import Wire.API.Bot.Service
import Wire.API.Event.Conversation (Event)
import Wire.API.Provider.Service (serviceRefId, serviceRefProvider)

interpretExternalAccess ::
  ( Member (Embed IO) r,
    Member (Input Env) r,
    Member TinyLog r
  ) =>
  Sem (ExternalAccess ': r) a ->
  Sem r a
interpretExternalAccess = interpret $ \case
  Deliver pp -> do
    logEffect "ExternalAccess.Deliver"
    embedApp $ deliver (toList pp)
  DeliverAsync pp -> do
    logEffect "ExternalAccess.DeliverAsync"
    embedApp $ deliverAsync (toList pp)
  DeliverAndDeleteAsync cid pp -> do
    logEffect "ExternalAccess.DeliverAndDeleteAsync"
    embedApp $ deliverAndDeleteAsync cid (toList pp)

-- | Like deliver, but ignore orphaned bots and return immediately.
--
-- FUTUREWORK: Check if this can be removed.
deliverAsync :: [(BotMember, Event)] -> App ()
deliverAsync = void . forkIO . void . deliver

-- | Like deliver, but remove orphaned bots and return immediately.
deliverAndDeleteAsync :: ConvId -> [(BotMember, Event)] -> App ()
deliverAndDeleteAsync cnv pushes = void . forkIO $ do
  gone <- deliver pushes
  mapM_ (deleteBot cnv . botMemId) gone

deliver :: [(BotMember, Event)] -> App [BotMember]
deliver pp = mapM (async . exec) pp >>= foldM eval [] . zip (map fst pp)
  where
    exec :: (BotMember, Event) -> App Bool
    exec (b, e) =
      lookupService (botMemService b) >>= \case
        Nothing -> pure False
        Just s -> do
          deliver1 s b e
          pure True
    eval :: [BotMember] -> (BotMember, Async Bool) -> App [BotMember]
    eval gone (b, a) = do
      let s = botMemService b
      r <- waitCatch a
      case r of
        Right True -> do
          Log.debug $
            field "provider" (toByteString (s ^. serviceRefProvider))
              ~~ field "service" (toByteString (s ^. serviceRefId))
              ~~ field "bot" (toByteString (botMemId b))
              ~~ msg (val "External delivery success")
          pure gone
        Right False -> do
          Log.debug $
            field "provider" (toByteString (s ^. serviceRefProvider))
              ~~ field "service" (toByteString (s ^. serviceRefId))
              ~~ field "bot" (toByteString (botMemId b))
              ~~ msg (val "External service gone")
          pure (b : gone)
        Left ex
          | Just (Http.HttpExceptionRequest _ (Http.StatusCodeException rs _)) <- fromException ex,
            Http.responseStatus rs == status410 -> do
              Log.debug $
                field "provider" (toByteString (s ^. serviceRefProvider))
                  ~~ field "service" (toByteString (s ^. serviceRefId))
                  ~~ field "bot" (toByteString (botMemId b))
                  ~~ msg (val "External bot gone")
              pure (b : gone)
        Left ex -> do
          Log.info $
            field "provider" (toByteString (s ^. serviceRefProvider))
              ~~ field "service" (toByteString (s ^. serviceRefId))
              ~~ field "bot" (toByteString (botMemId b))
              ~~ field "error" (show ex)
              ~~ msg (val "External delivery failure")
          pure gone

-- Internal -------------------------------------------------------------------

deliver1 :: ToJSON Event => Service -> BotMember -> Event -> App ()
deliver1 s bm e
  | s ^. serviceEnabled = do
      let t = toByteString' (s ^. serviceToken)
      let u = s ^. serviceUrl
      let b = botMemId bm
      let HttpsUrl url = u
      recovering x3 httpHandlers $
        const $
          sendMessage (s ^. serviceFingerprints) $
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

sendMessage :: [Fingerprint Rsa] -> (Request -> Request) -> App ()
sendMessage fprs reqBuilder = do
  (man, verifyFingerprints) <- view (extEnv . extGetManager)
  liftIO . withVerifiedSslConnection (verifyFingerprints fprs) man reqBuilder $ \req ->
    Http.withResponse req man (const $ pure ())

x3 :: RetryPolicy
x3 = limitRetries 3 <> constantDelay 1000000
