{-# LANGUAGE OverloadedStrings #-}

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

module API.Cannon where

import Data.ByteString.Conversion (toByteString')
import Data.Text (Text)
import qualified Network.WebSockets as WS
import Testlib.Prelude
import UnliftIO (timeout, try, withRunInIO)

-- | Fan-in has its own endpoint alongside the existing /events protocol.
withFanInWebSocket :: (HasCallStack, MakesValue user) => user -> Maybe String -> (WS.Connection -> App a) -> App a
withFanInWebSocket user client action = do
  services <- getServiceMap =<< objDomain user
  uid <- objId user
  apiVersion <- getAPIVersionFor (objDomain user)
  let HostPort host port = serviceHostPort services Cannon
      path = "/v" <> show apiVersion <> "/events/fan-in" <> maybe "" ("?client=" <>) client
  withRunInIO $ \run -> WS.runClientWith host (fromIntegral port) path WS.defaultConnectionOptions [("Z-User", toByteString' uid)] $ \connection -> do
    result <- run (action connection)
    WS.sendClose connection ("" :: Text)
    let waitClosed = try @IO @WS.ConnectionException (WS.receiveData connection :: IO ByteString) >>= either (const (pure ())) (const waitClosed)
    closed <- timeout 5_000_000 waitClosed
    when (isNothing closed) $ run (assertFailure "Fan-in WebSocket did not complete its close handshake")
    pure result

notifyFanInCannon :: (HasCallStack, MakesValue user) => user -> Value -> App Response
notifyFanInCannon user payload = do
  req <- baseRequest user Cannon Unversioned "/i/fan-in"
  submit "POST" $ req & addJSON payload

receiveFanInMessage :: (HasCallStack) => WS.Connection -> App Value
receiveFanInMessage connection = do
  received <- liftIO $ timeout 10_000_000 (WS.receiveData connection)
  bytes <- maybe (assertFailure "Timed out waiting for a fan-in WebSocket message") pure received
  case eitherDecode bytes of
    Left err -> assertFailure ("Invalid fan-in message: " <> err)
    Right value -> pure value

sendFanInMessage :: WS.Connection -> Value -> App ()
sendFanInMessage connection value = liftIO $ WS.sendBinaryData connection (encode value)
