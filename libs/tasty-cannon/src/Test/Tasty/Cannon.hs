{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Test.Tasty.Cannon
    ( Cannon

      -- * WebSockets
    , WebSocket
    , connect
    , close
    , bracket
    , bracketN
      -- ** Random Connection IDs
    , connectR
    , bracketR
    , bracketR2
    , bracketR3
    , bracketRN

      -- * Awaiting & Asserting on Notifications
    , MatchTimeout (..)
    , MatchFailure (..)
    , await
    , awaitMatch
    , awaitMatchN
    , assertMatch
    , assertMatchN
    , assertSuccess

      -- * Unpacking Notifications
    , unpackPayload

      -- * Randomness
    , randomConnId

      -- * Re-exports
    , Timeout
    , TimeoutUnit (..)
    , (#)
    ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.Timeout
import Control.Exception (SomeAsyncException, asyncExceptionFromException, throwIO)
import Control.Monad (void, forever)
import Control.Monad.Catch hiding (bracket)
import Control.Monad.IO.Class
import Data.Aeson (decodeStrict', FromJSON, fromJSON, Value (..))
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Conversion
import Data.Foldable (mapM_)
import Data.Id
import Data.List1
import Data.Maybe
import Data.Timeout (Timeout, TimeoutUnit (..), (#))
import Data.Typeable
import Data.Word
import Gundeck.Types
import Prelude hiding (mapM_)
import System.Random (randomIO)
import Test.Tasty.HUnit

import qualified Control.Monad.Catch   as Catch
import qualified Data.Aeson            as JSON
import qualified Data.ByteString.Char8 as C
import qualified Network.HTTP.Client   as Http
import qualified Network.WebSockets    as WS

type Cannon = Http.Request -> Http.Request

-----------------------------------------------------------------------------
-- WebSockets

data WebSocket = WebSocket
    { wsChan       :: TChan Notification
    , wsCloseLatch :: MVar ()
    , wsAppThread  :: Async ()
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

bracket :: (MonadIO m, MonadMask m)
        => Cannon
        -> UserId
        -> ConnId
        -> (WebSocket -> m a)
        -> m a
bracket can uid cid = Catch.bracket (connect can uid cid) close

bracketN :: (MonadIO m, MonadMask m)
         => Cannon
         -> [(UserId, ConnId)]
         -> ([WebSocket] -> m a)
         -> m a
bracketN c us f = go [] us
  where
    go wss []         = f (reverse wss)
    go wss ((x,y):xs) = bracket c x y (\ws -> go (ws:wss) xs)

-- Random Connection IDs

connectR :: MonadIO m => Cannon -> UserId -> m WebSocket
connectR can uid = randomConnId >>= connect can uid

bracketR :: (MonadIO m, MonadMask m) => Cannon -> UserId -> (WebSocket -> m a) -> m a
bracketR can usr f = do
    cid <- randomConnId
    bracket can usr cid f

bracketR2 :: (MonadIO m, MonadMask m)
         => Cannon
         -> UserId -> UserId
         -> ((WebSocket, WebSocket) -> m a)
         -> m a
bracketR2 c u1 u2 f =
    bracketR c u1 $ \c1 ->
    bracketR c u2 $ \c2 ->
        f (c1, c2)

bracketR3 :: (MonadIO m, MonadMask m)
         => Cannon
         -> UserId -> UserId -> UserId
         -> ((WebSocket, WebSocket, WebSocket) -> m a)
         -> m a
bracketR3 c u1 u2 u3 f =
    bracketR c u1 $ \c1 ->
    bracketR c u2 $ \c2 ->
    bracketR c u3 $ \c3 ->
        f (c1, c2, c3)

bracketRN :: (MonadIO m, MonadMask m)
         => Cannon
         -> [UserId]
         -> ([WebSocket] -> m a)
         -> m a
bracketRN c us f = go [] us
  where
    go wss []     = f (reverse wss)
    go wss (x:xs) = bracketR c x (\ws -> go (ws:wss) xs)

-----------------------------------------------------------------------------
-- Awaiting & Asserting on Notifications

newtype MatchFailure = MatchFailure SomeException
    deriving (Typeable)

instance Exception MatchFailure

instance Show MatchFailure where
    show (MatchFailure ex) = case fromException ex of
        Just (HUnitFailure msg) -> msg
        Nothing                 -> show ex

newtype MatchTimeout = MatchTimeout
    { timeoutFailures :: [MatchFailure]
    } deriving (Typeable)

instance Exception MatchTimeout

instance Show MatchTimeout where
    showsPrec _ (MatchTimeout mfs) =
          showString "Timeout: No matching notification received.\n"
        . showFailures mfs
      where
        showFailures     [] = id
        showFailures (e:es) = showString "Match failure: "
                            . shows e
                            . showString "\n"
                            . showFailures es

await :: MonadIO m => Timeout -> WebSocket -> m (Maybe Notification)
await t = liftIO . timeout t . atomically . readTChan . wsChan

awaitMatch :: (MonadIO m, MonadCatch m)
           => Timeout
           -> WebSocket
           -> (Notification -> Assertion)
           -> m (Either MatchTimeout Notification)
awaitMatch t ws match = go [] []
  where
    go buf errs = do
        mn <- await t ws
        case mn of
            Just  n -> do
                liftIO (match n)
                refill buf
                return (Right n)
              `catchAll` \e -> case asyncExceptionFromException e of
                Just  x -> throwM (x :: SomeAsyncException)
                Nothing -> let e' = MatchFailure (SomeException e)
                           in go (n : buf) (e' : errs)
            Nothing -> do
                refill buf
                return (Left (MatchTimeout errs))

    refill = mapM_ (liftIO . atomically . writeTChan (wsChan ws))

assertMatch :: (MonadIO m, MonadCatch m)
            => Timeout
            -> WebSocket
            -> (Notification -> Assertion)
            -> m Notification
assertMatch t ws f = awaitMatch t ws f >>= assertSuccess

awaitMatchN :: MonadIO m
            => Timeout
            -> [WebSocket]
            -> (Notification -> Assertion)
            -> m [Either MatchTimeout Notification]
awaitMatchN t wss f = liftIO $ mapConcurrently (\ws -> awaitMatch t ws f) wss

assertMatchN :: (MonadIO m, MonadThrow m)
             => Timeout
             -> [WebSocket]
             -> (Notification -> Assertion)
             -> m [Notification]
assertMatchN t wss f = awaitMatchN t wss f >>= mapM assertSuccess

assertSuccess :: (MonadIO m, MonadThrow m) => Either MatchTimeout Notification -> m Notification
assertSuccess = either throwM return

-----------------------------------------------------------------------------
-- Unpacking Notifications

unpackPayload :: FromJSON a => Notification -> List1 a
unpackPayload = fmap decodeEvent . ntfPayload
  where
    decodeEvent o = case fromJSON (Object o) of
        JSON.Success x -> x
        JSON.Error   e -> error e

-----------------------------------------------------------------------------
-- Randomness

randomConnId :: MonadIO m => m ConnId
randomConnId = liftIO $ do
    r <- randomIO :: IO Word32
    return . ConnId $ C.pack $ show r

-----------------------------------------------------------------------------
-- Internals

run :: MonadIO m => Cannon -> UserId -> ConnId -> WS.ClientApp () -> m (Async ())
run (($ Http.defaultRequest) -> ca) uid cid app = liftIO $ do
    latch <- newEmptyMVar
    wsapp <- async $
        WS.runClientWith caHost caPort caPath caOpts caHdrs (\conn ->
            putMVar latch () >> app conn
        ) `onException` tryPutMVar latch ()
    takeMVar latch
    stat <- poll wsapp
    case stat of
        Just (Left ex) -> throwIO ex
        _              -> return wsapp
  where
    caHost = C.unpack (Http.host ca)
    caPort = Http.port ca
    caPath = "/await" ++ C.unpack (Http.queryString ca)
    caOpts = WS.defaultConnectionOptions
    caHdrs = [ ("Z-User", toByteString' uid), ("Z-Connection", toByteString' cid) ]

clientApp :: TChan Notification -> MVar () -> WS.ClientApp ()
clientApp nchan latch conn = do
    r <- async wsRead
    w <- async wsWrite
    void $ waitEitherCancel r w
  where
    wsRead = forever $ do
        bs <- WS.receiveData conn
        case decodeStrict' bs of
            Just  n -> atomically $ writeTChan nchan n
            Nothing -> putStrLn $ "Failed to decode notification: " ++ show bs

    wsWrite = forever $ do
        takeMVar latch
        WS.sendClose conn ("close" :: ByteString)
