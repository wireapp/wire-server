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
    AwaitResult (..),
    withWebSocket,
    withWebSockets,
    awaitNMatchesResult,
    awaitNMatches,
    awaitMatch,
    awaitAnyEvent,
    awaitAtLeastNMatchesResult,
    awaitAtLeastNMatches,
    awaitNToMMatchesResult,
    awaitNToMMatches,
    assertAwaitResult,
    nPayload,
    printAwaitResult,
    printAwaitAtLeastResult,
    waitForResponse,
  )
where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM.TChan
import Control.Exception (throwIO)
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Catch hiding (bracket)
import qualified Control.Monad.Catch as Catch
import Control.Monad.IO.Class
import Control.Monad.Reader (asks)
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
import qualified Network.HTTP.Client as HTTP
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
import UnliftIO (withRunInIO)
import Prelude

data WebSocket = WebSocket
  { wsChan :: TChan Value,
    wsCloseLatch :: MVar (),
    wsAppThread :: Async ()
  }

-- Specifies how a Websocket at cannon should be opened
data WSConnect = WSConnect
  { user :: String,
    domain :: String,
    client :: Maybe String,
    -- | If this is Nothing then a random Z-Connection will be used
    conn :: Maybe String
  }

class ToWSConnect a where
  toWSConnect :: HasCallStack => a -> App WSConnect

instance {-# OVERLAPPING #-} ToWSConnect WSConnect where
  toWSConnect = pure

instance {-# OVERLAPPABLE #-} MakesValue user => ToWSConnect user where
  toWSConnect u = do
    (domain, uid) <- objQid u
    mc <- lookupField u "client_id"
    c <- traverse asString mc
    pure (WSConnect uid domain c Nothing)

instance (MakesValue user, MakesValue conn) => ToWSConnect (user, conn) where
  toWSConnect (u, c) = do
    (domain, uid) <- objQid u
    conn <- make c & asString
    pure (WSConnect uid domain Nothing (Just conn))

instance (MakesValue user, MakesValue conn, MakesValue client) => ToWSConnect (user, conn, client) where
  toWSConnect (u, c, cl) = do
    (domain, uid) <- objQid u
    client <- make cl & asString
    conn <- make c & asString
    pure (WSConnect uid domain (Just client) (Just conn))

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
run ::
  HasCallStack =>
  WSConnect ->
  WS.ClientApp () ->
  App (Async ())
run wsConnect app = do
  domain <- asString wsConnect.domain
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
  request <- do
    r <- rawBaseRequest domain Cannon Versioned path
    pure r {HTTP.requestHeaders = caHdrs}

  wsapp <-
    liftIO
      $ async
      $ catch
        ( WS.runClientWith
            caHost
            (fromIntegral caPort)
            path
            WS.defaultConnectionOptions
            caHdrs
            app
        )
      $ \(e :: SomeException) -> putMVar latch e

  presenceRequest <-
    baseRequest domain Cannon Unversioned $
      "/i/presences/" <> wsConnect.user <> "/" <> connId

  waitForPresence <- appToIO $ retryT $ do
    response <- submit "HEAD" presenceRequest
    status response `shouldMatchInt` 200
  let waitForException = do
        ex <- takeMVar latch
        -- Construct a "fake" response. We do not really have access to the
        -- websocket connection requests and response, unfortunately, but it is
        -- useful to display some information about the request in case an
        -- exception occurs.
        let r =
              Response
                { jsonBody = Nothing,
                  body = "This is a fake response. The actual response from cannon is not available.",
                  status = 101,
                  headers = mempty,
                  request = request
                }
        throwIO (AssertionFailure callStack (Just r) (displayException ex))

  liftIO $ race_ waitForPresence waitForException
  pure wsapp

close :: MonadIO m => WebSocket -> m ()
close ws = liftIO $ do
  putMVar (wsCloseLatch ws) ()
  void $ waitCatch (wsAppThread ws)

withWebSocket :: (HasCallStack, ToWSConnect w) => w -> (WebSocket -> App a) -> App a
withWebSocket w k = do
  wsConnect <- toWSConnect w
  Catch.bracket (connect wsConnect) close k

withWebSockets :: forall a w. (HasCallStack, ToWSConnect w) => [w] -> ([WebSocket] -> App a) -> App a
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

data AwaitAtLeastResult = AwaitAtLeastResult
  { success :: Bool,
    nMatchesExpectedMin :: Int,
    nMatchesExpectedMax :: Maybe Int,
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

prettyAwaitAtLeastResult :: AwaitAtLeastResult -> App String
prettyAwaitAtLeastResult r = do
  matchesS <- for r.matches prettyJSON
  nonMatchesS <- for r.nonMatches prettyJSON
  pure $
    "AwaitAtLeastResult\n"
      <> indent
        4
        ( unlines
            [ "min expected:" <> show r.nMatchesExpectedMin,
              "max expected:" <> show r.nMatchesExpectedMax,
              "success: " <> show (r.success),
              "matches:\n" <> unlines matchesS,
              "non-matches:\n" <> unlines nonMatchesS
            ]
        )

printAwaitResult :: AwaitResult -> App ()
printAwaitResult = prettyAwaitResult >=> liftIO . putStrLn

printAwaitAtLeastResult :: AwaitAtLeastResult -> App ()
printAwaitAtLeastResult = prettyAwaitAtLeastResult >=> liftIO . putStrLn

awaitAnyEvent :: Int -> WebSocket -> App (Maybe Value)
awaitAnyEvent tSecs = liftIO . timeout (tSecs * 1000 * 1000) . atomically . readTChan . wsChan

-- | 'await' an expected number of notification events on the websocket that
-- satisfy the provided predicate. If there isn't any new event (matching or
-- non-matching) for a 'tSecs' seconds then AwaitResult is a failure. This
-- function will never terminate if there is a constant stream of events
-- received. When this functions returns it will push any non-matching
-- events back to the websocket.
awaitNMatchesResult ::
  HasCallStack =>
  -- | Number of matches
  Int ->
  -- | Selection function. Exceptions are *not* caught.
  (Value -> App Bool) ->
  WebSocket ->
  App AwaitResult
awaitNMatchesResult nExpected checkMatch ws = go nExpected [] []
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
      tSecs <- asks timeOutSeconds
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

awaitAtLeastNMatchesResult ::
  HasCallStack =>
  -- | Minimum number of matches
  Int ->
  -- | Selection function. Exceptions are *not* caught.
  (Value -> App Bool) ->
  WebSocket ->
  App AwaitAtLeastResult
awaitAtLeastNMatchesResult nExpected checkMatch ws = go 0 [] []
  where
    go nSeen nonMatches matches = do
      tSecs <- asks timeOutSeconds
      mEvent <- awaitAnyEvent tSecs ws
      case mEvent of
        Just event ->
          do
            isMatch <- checkMatch event
            if isMatch
              then go (nSeen + 1) nonMatches (event : matches)
              else go nSeen (event : nonMatches) matches
        Nothing -> do
          refill nonMatches
          pure $
            AwaitAtLeastResult
              { success = nSeen >= nExpected,
                nMatchesExpectedMin = nExpected,
                nMatchesExpectedMax = Nothing,
                matches = reverse matches,
                nonMatches = reverse nonMatches
              }
    refill = mapM_ (liftIO . atomically . writeTChan (wsChan ws))

awaitNToMMatchesResult ::
  HasCallStack =>
  -- | Minimum number of matches
  Int ->
  -- | Maximum number of matches
  Int ->
  -- | Selection function. Exceptions are *not* caught.
  (Value -> App Bool) ->
  WebSocket ->
  App AwaitAtLeastResult
awaitNToMMatchesResult nMin nMax checkMatch ws = go 0 [] []
  where
    go nSeen nonMatches matches = do
      tSecs <- asks timeOutSeconds
      mEvent <- awaitAnyEvent tSecs ws
      case mEvent of
        Just event ->
          do
            isMatch <- checkMatch event
            if isMatch
              then go (nSeen + 1) nonMatches (event : matches)
              else go nSeen (event : nonMatches) matches
        Nothing -> do
          refill nonMatches
          pure $
            AwaitAtLeastResult
              { success = nMin <= nSeen && nSeen <= nMax,
                nMatchesExpectedMin = nMin,
                nMatchesExpectedMax = pure nMax,
                matches = reverse matches,
                nonMatches = reverse nonMatches
              }
    refill = mapM_ (liftIO . atomically . writeTChan (wsChan ws))

awaitNMatches ::
  HasCallStack =>
  -- | Number of matches
  Int ->
  -- | Selection function. Should not throw any exceptions
  (Value -> App Bool) ->
  WebSocket ->
  App [Value]
awaitNMatches nExpected checkMatch ws = do
  res <- awaitNMatchesResult nExpected checkMatch ws
  assertAwaitResult res

assertAwaitResult :: HasCallStack => AwaitResult -> App [Value]
assertAwaitResult res = do
  if res.success
    then pure res.matches
    else do
      let msgHeader = "Expected " <> show res.nMatchesExpected <> " matching events, but got " <> show (length res.matches) <> "."
      details <- ("Details:\n" <>) <$> prettyAwaitResult res
      assertFailure $ unlines [msgHeader, details]

awaitAtLeastNMatches ::
  HasCallStack =>
  -- | Minumum number of matches
  Int ->
  -- | Selection function. Should not throw any exceptions
  (Value -> App Bool) ->
  WebSocket ->
  App [Value]
awaitAtLeastNMatches nExpected checkMatch ws = do
  res <- awaitAtLeastNMatchesResult nExpected checkMatch ws
  if res.success
    then pure res.matches
    else do
      let msgHeader = "Expected " <> show nExpected <> " matching events, but got " <> show (length res.matches) <> "."
      details <- ("Details:\n" <>) <$> prettyAwaitAtLeastResult res
      assertFailure $ unlines [msgHeader, details]

awaitNToMMatches ::
  HasCallStack =>
  -- | Minimum Number of matches
  Int ->
  -- | Maximum Number of matches
  Int ->
  -- | Selection function. Should not throw any exceptions
  (Value -> App Bool) ->
  WebSocket ->
  App [Value]
awaitNToMMatches nMin nMax checkMatch ws = do
  res <- awaitNToMMatchesResult nMin nMax checkMatch ws
  if res.success
    then pure res.matches
    else do
      let msgHeader = "Expected between" <> show nMin <> " to " <> show nMax <> " matching events, but got " <> show (length res.matches) <> "."
      details <- ("Details:\n" <>) <$> prettyAwaitAtLeastResult res
      assertFailure $ unlines [msgHeader, details]

awaitMatch ::
  HasCallStack =>
  -- | Selection function. Should not throw any exceptions
  (Value -> App Bool) ->
  WebSocket ->
  App Value
awaitMatch checkMatch ws = head <$> awaitNMatches 1 checkMatch ws

nPayload :: MakesValue a => a -> App Value
nPayload event = do
  payloads <- event %. "payload" & asList
  assertOne payloads

-- | waits for an http response to satisfy a predicate
waitForResponse :: HasCallStack => App Response -> (Response -> App r) -> App r
waitForResponse act p = do
  tSecs <- asks timeOutSeconds
  r <- withRunInIO \inIO ->
    timeout (1000 * 1000 * tSecs) do
      let go = do
            inIO (bindResponse act p) `E.catch` \(_ :: AssertionFailure) -> do
              threadDelay 1000
              go
      go
  let err = unwords ["Expected event didn't come true after", show tSecs, "seconds"]
  maybe (assertFailure err) pure r
