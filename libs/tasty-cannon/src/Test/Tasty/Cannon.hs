{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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

module Test.Tasty.Cannon
  ( Cannon,

    -- * WebSockets
    WebSocket,
    connect,
    connectAsClient,
    close,
    bracket,
    bracketAsClient,
    bracketN,
    bracketAsClientN,

    -- ** Random Connection IDs
    connectR,
    connectAsClientR,
    bracketR,
    bracketAsClientR,
    bracketR2,
    bracketR3,
    bracketRN,
    bracketAsClientRN,

    -- * Awaiting & Asserting on Notifications
    MatchTimeout (..),
    MatchFailure (..),
    await,
    awaitMatch,
    awaitMatchN,
    assertMatch,
    assertMatch_,
    assertMatchN,
    assertMatchN_,
    assertSuccess,
    assertNoEvent,

    -- * Unpacking Notifications
    unpackPayload,

    -- * Randomness
    randomConnId,

    -- * Re-exports
    Timeout,
    TimeoutUnit (..),
    (#),
  )
where

import Bilge.Request (queryItem)
import Control.Concurrent.Async
import Control.Concurrent.Timeout hiding (threadDelay)
import Control.Exception (asyncExceptionFromException, throwIO)
import Control.Monad.Catch hiding (bracket)
import Control.Monad.Catch qualified as Catch
import Data.Aeson (FromJSON, Value (..), decodeStrict', fromJSON)
import Data.Aeson qualified as JSON
import Data.ByteString.Char8 qualified as C
import Data.ByteString.Conversion
import Data.Id
import Data.List1
import Data.Timeout (Timeout, TimeoutUnit (..), (#))
import Imports
import Network.HTTP.Client
import Network.HTTP.Client qualified as Http
import Network.HTTP.Types.Status
import Network.WebSockets qualified as WS
import System.Random (randomIO)
import Test.Tasty.HUnit
import Wire.API.Internal.Notification

type Cannon = Http.Request -> Http.Request

-----------------------------------------------------------------------------
-- WebSockets

data WebSocket = WebSocket
  { wsChan :: TChan Notification,
    wsCloseLatch :: MVar (),
    wsAppThread :: Async ()
  }

connect :: (MonadIO m) => Cannon -> UserId -> ConnId -> m WebSocket
connect can uid = connectAsMaybeClient can uid Nothing

connectAsClient :: (MonadIO m) => Cannon -> UserId -> ClientId -> ConnId -> m WebSocket
connectAsClient can uid client = connectAsMaybeClient can uid (Just client)

connectAsMaybeClient :: (MonadIO m) => Cannon -> UserId -> Maybe ClientId -> ConnId -> m WebSocket
connectAsMaybeClient can uid client conn = liftIO $ do
  nchan <- newTChanIO
  latch <- newEmptyMVar
  wsapp <- run can uid client conn (clientApp nchan latch)
  pure $ WebSocket nchan latch wsapp

close :: (MonadIO m) => WebSocket -> m ()
close ws = liftIO $ do
  putMVar (wsCloseLatch ws) ()
  void $ waitCatch (wsAppThread ws)

bracket ::
  (MonadIO m, MonadMask m) =>
  Cannon ->
  UserId ->
  ConnId ->
  (WebSocket -> m a) ->
  m a
bracket can uid conn =
  Catch.bracket (connect can uid conn) close

bracketAsClient ::
  (MonadMask m, MonadIO m) =>
  Cannon ->
  UserId ->
  ClientId ->
  ConnId ->
  (WebSocket -> m a) ->
  m a
bracketAsClient can uid client conn =
  Catch.bracket (connectAsClient can uid client conn) close

bracketN ::
  (MonadIO m, MonadMask m) =>
  Cannon ->
  [(UserId, ConnId)] ->
  ([WebSocket] -> m a) ->
  m a
bracketN c us f = go [] us
  where
    go wss [] = f (reverse wss)
    go wss ((x, y) : xs) = bracket c x y (\ws -> go (ws : wss) xs)

bracketAsClientN ::
  (MonadMask m, MonadIO m) =>
  Cannon ->
  [(UserId, ClientId, ConnId)] ->
  ([WebSocket] -> m a) ->
  m a
bracketAsClientN c us f = go [] us
  where
    go wss [] = f (reverse wss)
    go wss ((x, y, z) : xs) = bracketAsClient c x y z (\ws -> go (ws : wss) xs)

-- Random Connection IDs

connectR :: (MonadIO m) => Cannon -> UserId -> m WebSocket
connectR can uid = randomConnId >>= connect can uid

connectAsClientR :: (MonadIO m) => Cannon -> UserId -> ClientId -> m WebSocket
connectAsClientR can uid clientId = randomConnId >>= connectAsClient can uid clientId

bracketR :: (MonadIO m, MonadMask m) => Cannon -> UserId -> (WebSocket -> m a) -> m a
bracketR can usr f = do
  cid <- randomConnId
  bracket can usr cid f

bracketAsClientR :: (MonadIO m, MonadMask m) => Cannon -> UserId -> ClientId -> (WebSocket -> m a) -> m a
bracketAsClientR can usr clientId f = do
  connId <- randomConnId
  bracketAsClient can usr clientId connId f

bracketR2 ::
  (MonadIO m, MonadMask m) =>
  Cannon ->
  UserId ->
  UserId ->
  ((WebSocket, WebSocket) -> m a) ->
  m a
bracketR2 c u1 u2 f =
  bracketR c u1 $ \c1 ->
    bracketR c u2 $ \c2 ->
      f (c1, c2)

bracketR3 ::
  (MonadIO m, MonadMask m) =>
  Cannon ->
  UserId ->
  UserId ->
  UserId ->
  ((WebSocket, WebSocket, WebSocket) -> m a) ->
  m a
bracketR3 c u1 u2 u3 f =
  bracketR c u1 $ \c1 ->
    bracketR c u2 $ \c2 ->
      bracketR c u3 $ \c3 ->
        f (c1, c2, c3)

bracketRN ::
  (MonadIO m, MonadMask m) =>
  Cannon ->
  [UserId] ->
  ([WebSocket] -> m a) ->
  m a
bracketRN c us f = go [] us
  where
    go wss [] = f (reverse wss)
    go wss (x : xs) = bracketR c x (\ws -> go (ws : wss) xs)

bracketAsClientRN ::
  (MonadIO m, MonadMask m) =>
  Cannon ->
  [(UserId, ClientId)] ->
  ([WebSocket] -> m a) ->
  m a
bracketAsClientRN can us f = go [] us
  where
    go wss [] = f (reverse wss)
    go wss ((u, c) : xs) = bracketAsClientR can u c (\ws -> go (ws : wss) xs)

-----------------------------------------------------------------------------
-- Awaiting & Asserting on Notifications

newtype MatchFailure = MatchFailure SomeException
  deriving (Typeable)

instance Exception MatchFailure

instance Show MatchFailure where
  show (MatchFailure ex) = case fromException ex of
    Just (HUnitFailure _src msg) -> msg
    Nothing -> show ex

newtype MatchTimeout = MatchTimeout
  { timeoutFailures :: [MatchFailure]
  }
  deriving (Typeable)

instance Exception MatchTimeout

instance Show MatchTimeout where
  showsPrec _ (MatchTimeout mfs) =
    showString "Timeout: No matching notification received.\n"
      . showFailures mfs
    where
      showFailures [] = id
      showFailures (e : es) =
        showString "Match failure: "
          . shows e
          . showString "\n"
          . showFailures es

newtype RegistrationTimeout = RegistrationTimeout Int
  deriving (Typeable)

instance Exception RegistrationTimeout

instance Show RegistrationTimeout where
  show (RegistrationTimeout s) =
    "Failed to find a registration after " ++ show s ++ " retries.\n"

await :: (MonadIO m) => Timeout -> WebSocket -> m (Maybe Notification)
await t = liftIO . timeout t . atomically . readTChan . wsChan

-- | 'await' a 'Notification' on the 'WebSocket'.  If it satisfies the 'Assertion', return it.
-- Otherwise, collect the 'Notification' and the exception thrown by the 'Assertion', and keep
-- 'await'ing.  If 'await' times out or a satisfactory notification is available, fill all
-- unsatisfactory notifications back to the web socket.
--
-- NB: (1) if 'await' receives irrelevant 'Notification's frequently enough, this function will
-- never terminate.  The 'Timeout' argument is just passed through to 'await'.  (2) If an
-- asynchronous exception is thrown, this drops all collected 'Notification's and exceptions, and
-- re-throws.  This may be surprising if you run 'awaitMatch' inside a 'timeout' guard, and want to
-- resume testing other things once the timeout triggers.
awaitMatch ::
  (HasCallStack, MonadIO m, MonadCatch m) =>
  Timeout ->
  WebSocket ->
  (Notification -> IO a) ->
  m (Either MatchTimeout a)
awaitMatch t ws match = go [] []
  where
    go buf errs = do
      mn <- await t ws
      case mn of
        Just n ->
          do
            a <- liftIO (match n)
            refill buf
            pure (Right a)
            `catchAll` \e -> case asyncExceptionFromException e of
              Just x -> throwM (x :: SomeAsyncException)
              Nothing ->
                let e' = MatchFailure e
                 in go (n : buf) (e' : errs)
        Nothing -> do
          refill buf
          pure (Left (MatchTimeout errs))
    refill = mapM_ (liftIO . atomically . writeTChan (wsChan ws))

assertMatch ::
  (HasCallStack, MonadIO m, MonadCatch m) =>
  Timeout ->
  WebSocket ->
  (Notification -> IO a) ->
  m a
assertMatch t ws f = awaitMatch t ws f >>= assertSuccess

assertMatch_ ::
  (HasCallStack, MonadIO m, MonadCatch m) =>
  Timeout ->
  WebSocket ->
  (Notification -> IO a) ->
  m ()
assertMatch_ t w = void . assertMatch t w

awaitMatchN ::
  (HasCallStack, MonadIO m) =>
  Timeout ->
  [WebSocket] ->
  (Notification -> IO a) ->
  m [Either MatchTimeout a]
awaitMatchN t wss f = snd <$$> awaitMatchN' t (((),) <$> wss) f

awaitMatchN' ::
  (HasCallStack, MonadIO m) =>
  Timeout ->
  [(extra, WebSocket)] ->
  (Notification -> IO a) ->
  m [(extra, Either MatchTimeout a)]
awaitMatchN' t wss f = liftIO $ mapConcurrently (\(extra, ws) -> (extra,) <$> awaitMatch t ws f) wss

assertMatchN ::
  (HasCallStack, MonadIO m, MonadThrow m) =>
  Timeout ->
  [WebSocket] ->
  (Notification -> IO a) ->
  m [a]
assertMatchN t wss f = awaitMatchN t wss f >>= mapM assertSuccess

assertMatchN_ ::
  (HasCallStack, MonadIO m, MonadThrow m) =>
  Timeout ->
  [WebSocket] ->
  (Notification -> IO a) ->
  m ()
assertMatchN_ t wss f = void $ assertMatchN t wss f

assertSuccess :: (HasCallStack, MonadIO m, MonadThrow m) => Either MatchTimeout a -> m a
assertSuccess = either throwM pure

assertNoEvent :: (HasCallStack, MonadIO m) => Timeout -> [WebSocket] -> m ()
assertNoEvent t ww = do
  results <- awaitMatchN' t (zip [(0 :: Int) ..] ww) pure
  for_ results $ \(ix, result) ->
    either (const $ pure ()) (liftIO . f ix) result
  where
    f ix n = assertFailure $ "unexpected notification received: " ++ show (ix, n)

-----------------------------------------------------------------------------
-- Unpacking Notifications

unpackPayload :: (FromJSON a) => Notification -> List1 a
unpackPayload = fmap decodeEvent . ntfPayload
  where
    decodeEvent o = case fromJSON (Object o) of
      JSON.Success x -> x
      JSON.Error e -> error e

-----------------------------------------------------------------------------
-- Randomness

randomConnId :: (MonadIO m) => m ConnId
randomConnId = liftIO $ do
  r <- randomIO :: IO Word32
  pure . ConnId $ C.pack $ show r

-----------------------------------------------------------------------------
-- Internals

-- | Start a client thread in 'Async' that opens a web socket to a Cannon, wait
--   for the connection to register with Gundeck, and return the 'Async' thread.
run :: (MonadIO m) => Cannon -> UserId -> Maybe ClientId -> ConnId -> WS.ClientApp () -> m (Async ())
run cannon@(($ Http.defaultRequest) -> ca) uid client connId app = liftIO $ do
  latch <- newEmptyMVar
  wsapp <-
    async $
      WS.runClientWith
        caHost
        caPort
        caPath
        caOpts
        caHdrs
        ( \conn ->
            putMVar latch () >> app conn
        )
        `onException` tryPutMVar latch ()
  takeMVar latch
  stat <- poll wsapp
  case stat of
    Just (Left ex) -> throwIO ex
    _ -> waitForRegistry numRetries >> pure wsapp
  where
    caHost = C.unpack (Http.host ca)
    caPort = Http.port ca
    caPath = "/await" ++ C.unpack caQuery
    caQuery = Http.queryString . cannon . maybe id (queryItem "client" . toByteString') client $ Http.defaultRequest
    caOpts = WS.defaultConnectionOptions
    caHdrs = [("Z-User", toByteString' uid), ("Z-Connection", toByteString' connId)]
    numRetries = 30
    waitForRegistry 0 = throwIO $ RegistrationTimeout numRetries
    waitForRegistry (n :: Int) = do
      man <- newManager defaultManagerSettings
      let ca' =
            ca
              { method = "HEAD",
                path = "/i/presences/" <> toByteString' uid <> "/" <> toByteString' connId
              }
      res <- httpLbs ca' man
      unless (responseStatus res == status200) $ do
        threadDelay $ 100 * 1000
        waitForRegistry (n - 1)

clientApp :: TChan Notification -> MVar () -> WS.ClientApp ()
clientApp nchan latch conn = do
  r <- async wsRead
  w <- async wsWrite
  void $ waitEitherCancel r w
  where
    wsRead = forever $ do
      bs <- WS.receiveData conn
      case decodeStrict' bs of
        Just n -> atomically $ writeTChan nchan n
        Nothing -> putStrLn $ "Failed to decode notification: " ++ show bs
    wsWrite = forever $ do
      takeMVar latch
      WS.sendClose conn ("close" :: ByteString)
