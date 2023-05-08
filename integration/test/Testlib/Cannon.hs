{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

module Testlib.Cannon
  ( WebSocket (..),
    WSConnect (..),
    ToWSConnect (..),
    withWebSocket,
    withWebSockets,
    awaitNMatchesResult,
    awaitNMatches,
    awaitMatch,
    nPayload,
    printAwaitResult,
  )
where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM.TChan
import Control.Exception (throwIO)
import Control.Monad
import Control.Monad.Catch hiding (bracket)
import qualified Control.Monad.Catch as Catch
import Control.Monad.IO.Class
import Control.Monad.STM
import Data.Aeson (Value (..), decodeStrict')
import Data.ByteString (ByteString)
import Data.ByteString.Conversion (fromByteString)
import Data.ByteString.Conversion.To
import Data.Function
import Data.Maybe
import Data.Traversable
import Data.Word
import GHC.Stack
import qualified Network.HTTP.Client as Http
import qualified Network.WebSockets as WS
import System.Random (randomIO)
import System.Timeout (timeout)
import Testlib.App
import Testlib.Assertions
import Testlib.Env
import Testlib.HTTP
import Testlib.JSON
import Testlib.Printing
import Testlib.Types

data WebSocket = WebSocket
  { wsChan :: TChan Value,
    wsCloseLatch :: MVar (),
    wsAppThread :: Async ()
  }

-- Specifies how a Websocket at cannon should be opened
data WSConnect = WSConnect
  { user :: String,
    client :: Maybe String,
    -- | If this is Nothing then a random Z-Connection will be used
    conn :: Maybe String
  }

class ToWSConnect a where
  toWSConnect :: HasCallStack => a -> App WSConnect

instance {-# OVERLAPPING #-} ToWSConnect WSConnect where
  toWSConnect = pure

instance {-# OVERLAPPABLE #-} (MakesValue user) => ToWSConnect user where
  toWSConnect u = do
    uid <- objId u & asString
    pure (WSConnect uid Nothing Nothing)

instance (MakesValue user, MakesValue conn) => ToWSConnect (user, conn) where
  toWSConnect (u, c) = do
    uid <- objId u & asString
    conn <- make c & asString
    pure (WSConnect uid Nothing (Just conn))

instance (MakesValue user, MakesValue conn, MakesValue client) => ToWSConnect (user, conn, client) where
  toWSConnect (u, c, cl) = do
    uid <- objId u & asString
    client <- make cl & asString
    conn <- make c & asString
    pure (WSConnect uid (Just client) (Just conn))

connect :: HasCallStack => WSConnect -> App WebSocket
connect wsConnect = do
  nchan <- liftIO newTChanIO
  latch <- liftIO newEmptyMVar
  wsapp <- run wsConnect (clientApp nchan latch)
  pure $ WebSocket nchan latch wsapp

clientApp :: HasCallStack => TChan Value -> MVar () -> WS.ClientApp ()
clientApp wsChan latch conn = do
  r <- async wsRead
  w <- async wsWrite
  void $ waitEitherCancel r w
  where
    wsRead = forever $ do
      bs <- WS.receiveData conn
      case decodeStrict' bs of
        Just n -> atomically $ writeTChan wsChan n
        Nothing -> putStrLn $ "Failed to decode notification: " ++ show bs
    wsWrite = forever $ do
      takeMVar latch
      WS.sendClose conn ("close" :: ByteString)

-- | Start a client thread in 'Async' that opens a web socket to a Cannon, wait
--   for the connection to register with Gundeck, and return the 'Async' thread.
run :: HasCallStack => WSConnect -> WS.ClientApp () -> App (Async ())
run wsConnect app = do
  domain <- ownDomain
  serviceMap <- getServiceMap domain

  let HostPort caHost caPort = serviceHostPort serviceMap Cannon
  latch <- liftIO newEmptyMVar

  connId <- case wsConnect.conn of
    Just c -> pure c
    Nothing -> show <$> liftIO (randomIO :: IO Word32)

  let path =
        "/await"
          <> ( case wsConnect.client of
                 Nothing -> ""
                 Just client -> fromJust . fromByteString $ Http.queryString (Http.setQueryString [("client", Just (toByteString' client))] Http.defaultRequest)
             )
      caHdrs =
        [ ("Z-User", toByteString' (wsConnect.user)),
          ("Z-Connection", toByteString' connId)
        ]

  wsapp <-
    liftIO $
      async $
        WS.runClientWith
          caHost
          (fromIntegral caPort)
          path
          WS.defaultConnectionOptions
          caHdrs
          ( \conn ->
              putMVar latch () >> app conn
          )
          `onException` tryPutMVar latch ()

  let waitForRegistry :: HasCallStack => App ()
      waitForRegistry = unrace $ do
        request <- baseRequest ownDomain Cannon Unversioned ("/i/presences/" <> wsConnect.user <> "/" <> connId)
        response <- submit "HEAD" request
        status response `shouldMatchInt` 200

  liftIO $ takeMVar latch
  stat <- liftIO $ poll wsapp
  case stat of
    Just (Left ex) -> liftIO $ throwIO ex
    _ -> waitForRegistry >> pure wsapp

close :: MonadIO m => WebSocket -> m ()
close ws = liftIO $ do
  putMVar (wsCloseLatch ws) ()
  void $ waitCatch (wsAppThread ws)

withWebSocket :: (HasCallStack, ToWSConnect w) => w -> (WebSocket -> App a) -> App a
withWebSocket w k = do
  wsConnect <- toWSConnect w
  Catch.bracket (connect wsConnect) close k

withWebSockets :: forall a w. (HasCallStack, (ToWSConnect w)) => [w] -> ([WebSocket] -> App a) -> App a
withWebSockets twcs k = do
  wcs <- for twcs toWSConnect
  go wcs []
  where
    go :: HasCallStack => [WSConnect] -> [WebSocket] -> App a
    go [] wss = k (reverse wss)
    go (wc : wcs) wss = withWebSocket wc (\ws -> go wcs (ws : wss))

data AwaitResult = AwaitResult
  { success :: Bool,
    nMatchesExpected :: Int,
    matches :: [Value],
    nonMatches :: [Value]
  }

prettyAwaitResult :: AwaitResult -> App String
prettyAwaitResult r = do
  matchesS <- for r.matches prettyJSON
  nonMatchesS <- for r.nonMatches prettyJSON
  pure $
    "AwaitResult\n"
      <> indent
        4
        ( unlines
            [ "success: " <> show (r.success),
              "matches:\n" <> unlines matchesS,
              "non-matches:\n" <> unlines nonMatchesS
            ]
        )

printAwaitResult :: AwaitResult -> App ()
printAwaitResult = prettyAwaitResult >=> liftIO . putStrLn

awaitAnyEvent :: MonadIO m => Int -> WebSocket -> m (Maybe Value)
awaitAnyEvent tSecs = liftIO . timeout (tSecs * 1000 * 1000) . atomically . readTChan . wsChan

-- | 'await' an expected number of notification events on the websocket that
-- satisfy the provided predicate. If there isn't any new event (matching or
-- non-matching) for a 'tSecs' seconds then AwaitResult is a failure. This
-- funciton will never terminate if there is a constant stream of events
-- received. When this functions retruns it will push any non-matching
-- events back to the websocket.
awaitNMatchesResult ::
  HasCallStack =>
  -- | Number of matches
  Int ->
  -- | Timeout in seconds
  Int ->
  -- | Selection function. Exceptions are *not* caught.
  (Value -> App Bool) ->
  WebSocket ->
  App AwaitResult
awaitNMatchesResult nExpected tSecs checkMatch ws = go nExpected [] []
  where
    go 0 nonMatches matches = do
      refill nonMatches
      pure $
        AwaitResult
          { success = True,
            nMatchesExpected = nExpected,
            matches = reverse matches,
            nonMatches = reverse nonMatches
          }
    go nLeft nonMatches matches = do
      mEvent <- awaitAnyEvent tSecs ws
      case mEvent of
        Just event ->
          do
            isMatch <- checkMatch event
            if isMatch
              then go (nLeft - 1) nonMatches (event : matches)
              else go nLeft (event : nonMatches) matches
        Nothing -> do
          refill nonMatches
          pure $
            AwaitResult
              { success = False,
                nMatchesExpected = nExpected,
                matches = reverse matches,
                nonMatches = reverse nonMatches
              }
    refill = mapM_ (liftIO . atomically . writeTChan (wsChan ws))

awaitNMatches ::
  HasCallStack =>
  -- | Number of matches
  Int ->
  -- | Timeout in seconds
  Int ->
  -- | Selection function. Should not throw any exceptions
  (Value -> App Bool) ->
  WebSocket ->
  App [Value]
awaitNMatches nExpected tSecs checkMatch ws = do
  res <- awaitNMatchesResult nExpected tSecs checkMatch ws
  if res.success
    then pure res.matches
    else do
      let msgHeader = "Expected " <> show nExpected <> " matching events, but got " <> show (length res.matches) <> "."
      details <- ("Details:\n" <>) <$> prettyAwaitResult res
      assertFailure $ unlines [msgHeader, details]

awaitMatch ::
  HasCallStack =>
  -- | Timeout in seconds
  Int ->
  -- | Selection function. Should not throw any exceptions
  (Value -> App Bool) ->
  WebSocket ->
  App Value
awaitMatch tSecs checkMatch ws = head <$> awaitNMatches 1 tSecs checkMatch ws

nPayload :: MakesValue a => a -> App Value
nPayload event = do
  payloads <- event %. "payload" & asList
  assertOne payloads
