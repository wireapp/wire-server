{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

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

module Test.Tasty.Cannon
  ( Cannon,

    -- * WebSockets
    WebSocket,
    connect,
    close,
    bracket,
    bracketN,

    -- ** Random Connection IDs
    connectR,
    bracketR,
    bracketR2,
    bracketR3,
    bracketRN,

    -- * Awaiting & Asserting on Notifications
    MatchTimeout (..),
    MatchFailure (..),
    await,
    awaitMatch,
    awaitMatch_,
    awaitMatchN,
    assertMatch,
    assertMatch_,
    assertMatchN,
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

import Control.Concurrent.Async
import Control.Concurrent.Timeout hiding (threadDelay)
import Control.Exception (SomeAsyncException, asyncExceptionFromException, throwIO)
import Control.Monad.Catch hiding (bracket)
import qualified Control.Monad.Catch as Catch
import Data.Aeson (FromJSON, Value (..), decodeStrict', fromJSON)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Conversion
import Data.Id
import Data.List1
import Data.Misc ((<$$>))
import Data.Timeout (Timeout, TimeoutUnit (..), (#))
import Gundeck.Types
import Imports
import Network.HTTP.Client
import qualified Network.HTTP.Client as Http
import Network.HTTP.Types.Status
import qualified Network.WebSockets as WS
import System.Random (randomIO)
import Test.Tasty.HUnit

type Cannon = Http.Request -> Http.Request

-----------------------------------------------------------------------------
-- WebSockets

data WebSocket = WebSocket
  { wsChan :: TChan Notification,
    wsCloseLatch :: MVar (),
    wsAppThread :: Async ()
  }

connect :: MonadIO m => Cannon -> UserId -> ConnId -> m WebSocket
connect can uid cid = liftIO $ do
  nchan <- newTChanIO
  latch <- newEmptyMVar
  wsapp <- run can uid cid (clientApp nchan latch)
  return $ WebSocket nchan latch wsapp

close :: MonadIO m => WebSocket -> m ()
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
bracket can uid cid = Catch.bracket (connect can uid cid) close

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

-- Random Connection IDs

connectR :: MonadIO m => Cannon -> UserId -> m WebSocket
connectR can uid = randomConnId >>= connect can uid

bracketR :: (MonadIO m, MonadMask m) => Cannon -> UserId -> (WebSocket -> m a) -> m a
bracketR can usr f = do
  cid <- randomConnId
  bracket can usr cid f

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

await :: MonadIO m => Timeout -> WebSocket -> m (Maybe Notification)
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
  (Notification -> Assertion) ->
  m (Either MatchTimeout Notification)
awaitMatch t ws match = go [] []
  where
    go buf errs = do
      mn <- await t ws
      case mn of
        Just n ->
          do
            liftIO (match n)
            refill buf
            return (Right n)
            `catchAll` \e -> case asyncExceptionFromException e of
              Just x -> throwM (x :: SomeAsyncException)
              Nothing ->
                let e' = MatchFailure e
                 in go (n : buf) (e' : errs)
        Nothing -> do
          refill buf
          return (Left (MatchTimeout errs))
    refill = mapM_ (liftIO . atomically . writeTChan (wsChan ws))

awaitMatch_ ::
  (HasCallStack, MonadIO m, MonadCatch m) =>
  Timeout ->
  WebSocket ->
  (Notification -> Assertion) ->
  m ()
awaitMatch_ t w = void . awaitMatch t w

assertMatch ::
  (HasCallStack, MonadIO m, MonadCatch m) =>
  Timeout ->
  WebSocket ->
  (Notification -> Assertion) ->
  m Notification
assertMatch t ws f = awaitMatch t ws f >>= assertSuccess

assertMatch_ ::
  (HasCallStack, MonadIO m, MonadCatch m) =>
  Timeout ->
  WebSocket ->
  (Notification -> Assertion) ->
  m ()
assertMatch_ t w = void . assertMatch t w

awaitMatchN ::
  (HasCallStack, MonadIO m) =>
  Timeout ->
  [WebSocket] ->
  (Notification -> Assertion) ->
  m [Either MatchTimeout Notification]
awaitMatchN t wss f = snd <$$> awaitMatchN' t (((),) <$> wss) f

awaitMatchN' ::
  (HasCallStack, MonadIO m) =>
  Timeout ->
  [(extra, WebSocket)] ->
  (Notification -> Assertion) ->
  m [(extra, Either MatchTimeout Notification)]
awaitMatchN' t wss f = liftIO $ mapConcurrently (\(extra, ws) -> (extra,) <$> awaitMatch t ws f) wss

assertMatchN ::
  (HasCallStack, MonadIO m, MonadThrow m) =>
  Timeout ->
  [WebSocket] ->
  (Notification -> Assertion) ->
  m [Notification]
assertMatchN t wss f = awaitMatchN t wss f >>= mapM assertSuccess

assertSuccess :: (HasCallStack, MonadIO m, MonadThrow m) => Either MatchTimeout Notification -> m Notification
assertSuccess = either throwM return

assertNoEvent :: (HasCallStack, MonadIO m, MonadCatch m) => Timeout -> [WebSocket] -> m ()
assertNoEvent t ww = do
  results <- awaitMatchN' t (zip [(0 :: Int) ..] ww) (const $ pure ())
  for_ results $ \(ix, result) ->
    either (const $ pure ()) (liftIO . f ix) result
  where
    f ix n = assertFailure $ "unexpected notification received: " ++ show (ix, n)

-----------------------------------------------------------------------------
-- Unpacking Notifications

unpackPayload :: FromJSON a => Notification -> List1 a
unpackPayload = fmap decodeEvent . ntfPayload
  where
    decodeEvent o = case fromJSON (Object o) of
      JSON.Success x -> x
      JSON.Error e -> error e

-----------------------------------------------------------------------------
-- Randomness

randomConnId :: MonadIO m => m ConnId
randomConnId = liftIO $ do
  r <- randomIO :: IO Word32
  return . ConnId $ C.pack $ show r

-----------------------------------------------------------------------------
-- Internals

-- | Start a client thread in 'Async' that opens a web socket to a Cannon, wait
--   for the connection to register with Gundeck, and return the 'Async' thread.
run :: MonadIO m => Cannon -> UserId -> ConnId -> WS.ClientApp () -> m (Async ())
run (($ Http.defaultRequest) -> ca) uid cid app = liftIO $ do
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
    _ -> waitForRegistry numRetries >> return wsapp
  where
    caHost = C.unpack (Http.host ca)
    caPort = Http.port ca
    caPath = "/await" ++ C.unpack (Http.queryString ca)
    caOpts = WS.defaultConnectionOptions
    caHdrs = [("Z-User", toByteString' uid), ("Z-Connection", toByteString' cid)]
    numRetries = 30
    waitForRegistry 0 = throwIO $ RegistrationTimeout numRetries
    waitForRegistry (n :: Int) = do
      man <- newManager defaultManagerSettings
      let ca' =
            ca
              { method = "HEAD",
                path = "/i/presences/" <> toByteString' uid <> "/" <> toByteString' cid
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
