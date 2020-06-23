-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Galley.External
  ( deliver,
  )
where

import Bilge.Request
import Bilge.Retry (httpHandlers)
import Control.Exception (fromException)
import Control.Lens
import Control.Retry
import Data.ByteString.Conversion.To
import Data.Misc
import Galley.App
import Galley.Data.Services (BotMember, botMemId, botMemService)
import qualified Galley.Data.Services as Data
import Galley.Types (Event)
import Galley.Types.Bot
import Imports
import qualified Network.HTTP.Client as Http
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status (status410)
import Ssl.Util (withVerifiedSslConnection)
import qualified System.Logger.Class as Log
import System.Logger.Message (field, msg, val, (~~))
import URI.ByteString
import UnliftIO (Async, async, waitCatch)

-- | Deliver events to external (bot) services.
--
-- Returns those bots which are found to be orphaned by the external
-- service, e.g. when the service tells us that it no longer knows about the
-- bot.
deliver :: [(BotMember, Event)] -> Galley [BotMember]
deliver pp = mapM (async . exec) pp >>= foldM eval [] . zip (map fst pp)
  where
    exec :: (BotMember, Event) -> Galley Bool
    exec (b, e) =
      Data.lookupService (botMemService b) >>= \case
        Nothing -> return False
        Just s -> do
          deliver1 s b e
          return True
    eval :: [BotMember] -> (BotMember, Async Bool) -> Galley [BotMember]
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
          return gone
        Right False -> do
          Log.debug $
            field "provider" (toByteString (s ^. serviceRefProvider))
              ~~ field "service" (toByteString (s ^. serviceRefId))
              ~~ field "bot" (toByteString (botMemId b))
              ~~ msg (val "External service gone")
          return (b : gone)
        Left ex
          | Just (Http.HttpExceptionRequest _ (Http.StatusCodeException rs _)) <- fromException ex,
            Http.responseStatus rs == status410 -> do
            Log.debug $
              field "provider" (toByteString (s ^. serviceRefProvider))
                ~~ field "service" (toByteString (s ^. serviceRefId))
                ~~ field "bot" (toByteString (botMemId b))
                ~~ msg (val "External bot gone")
            return (b : gone)
        Left ex -> do
          Log.info $
            field "provider" (toByteString (s ^. serviceRefProvider))
              ~~ field "service" (toByteString (s ^. serviceRefId))
              ~~ field "bot" (toByteString (botMemId b))
              ~~ field "error" (show ex)
              ~~ msg (val "External delivery failure")
          return gone

-- Internal -------------------------------------------------------------------

deliver1 :: Service -> BotMember -> Event -> Galley ()
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
  | otherwise = return ()

urlHost :: HttpsUrl -> Maybe ByteString
urlHost (HttpsUrl u) = u ^. authorityL <&> view (authorityHostL . hostBSL)

urlPort :: HttpsUrl -> Maybe Word16
urlPort (HttpsUrl u) = do
  a <- u ^. authorityL
  p <- a ^. authorityPortL
  return (fromIntegral (p ^. portNumberL))

sendMessage :: [Fingerprint Rsa] -> (Request -> Request) -> Galley ()
sendMessage fprs reqBuilder = do
  (man, verifyFingerprints) <- view (extEnv . extGetManager)
  liftIO . withVerifiedSslConnection (verifyFingerprints fprs) man reqBuilder $ \req ->
    Http.withResponse req man (const $ return ())

x3 :: RetryPolicy
x3 = limitRetries 3 <> constantDelay 1000000
