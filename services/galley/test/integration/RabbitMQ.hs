module RabbitMQ where

import qualified System.Logger as Log
import Imports
import TestSetup
import Network.AMQP.Extended (RabbitMqHooks (RabbitMqHooks), openConnectionWithRetries)
import qualified Network.AMQP.Extended as AMQP
import Control.Lens ((^.), view)
import Data.Domain
import Galley.Options
import Galley.Run (publishRabbitMsg, readRabbitMq, ensureQueue)
import Test.Tasty.HUnit
import Data.Text (unpack, pack)
import System.Random

-- compete and timeout are from https://wiki.haskell.org/Timing_out_computations
compete :: [IO a] -> IO a
compete actions = do
    mvar <- newEmptyMVar
    tids <- mapM (\action -> forkIO $ action >>= putMVar mvar) actions
    result <- takeMVar mvar
    mapM_ killThread tids
    pure result

timeout :: Int -> IO a -> IO (Maybe a)
timeout usec action = compete [fmap Just action, threadDelay usec >> pure Nothing]

-- Test the round trip to rabbit from galley
-- and especially that we get out the domain we put in.`
rabbitPubSub :: TestM ()
rabbitPubSub = view (tsGConf . optRabbitmq)
  >>= maybe (pure ()) withRabbitOpts

-- Generate a simple random string
randomString :: MonadIO m => m String
randomString = getStdRandom $ \gen ->
  let (count, gen') = randomR (3, 10) gen
  -- I'm not happy with using replicate, but my hoogling didn't
  -- show anything closer to what I wanted.
  in foldr step ([], gen') $ replicate count ()
  where
    step _ (s, g) = let (c, g') = randomR ('a', 'z') g in (c:s, g')

randomDomain :: MonadIO m => m Domain
randomDomain = Domain <$> do
  a <- randomString
  b <- randomString
  pure $ pack $ a <> "." <> b

withRabbitOpts :: RabbitMqOpts -> TestM ()
withRabbitOpts rabbitOpts = do
  log' <- liftIO $ Log.new Log.defSettings
  -- If this IORef has a Just in it, that will contain
  -- an error message. When we successfully get our message
  -- from Rabbit we empty it
  ioref <- liftIO $ newIORef $ pure "Empty: Maybe we weren't able to talk to RabbitMQ yet"
  -- Make a random string to ensure we aren't seeing leftover
  -- messages from other tests
  dom <- randomDomain
  queue <- pack . ("galley-integration-testing-" <>) <$> randomString
  -- We don't care about the result from timeout, as
  -- the openConnect... function will loop forever. We
  -- just need to kill it in a timely manner
  liftIO $ void $ timeout timeout_us $
    openConnectionWithRetries log' host port vhost $
      RabbitMqHooks
        { AMQP.onConnectionClose = pure ()
        , AMQP.onChannelException = \_e -> pure ()
        , AMQP.onNewChannel = \chan -> do
          -- Run the tests here
          ensureQueue chan queue
          void $ publishRabbitMsg chan queue dom
          liftIO $ readRabbitMq chan queue log' $ \dom' -> do
            -- Append the random string
            if dom == dom'
            then do -- Happy path
              atomicWriteIORef ioref Nothing
            else do -- Sad path
              atomicWriteIORef ioref $ Just $ "expected \"" <> unpack (domainText dom) <> "\" but got \"" <> unpack (domainText dom') <> "\""
          -- Keep this around until we kill it.
          forever $ threadDelay 1_000_000
        }
  liftIO $ readIORef ioref >>= maybe
    (pure ())
    assertFailure
  where
    timeout_us = 5 * 1_000_000 -- seconds scale
    host = rabbitOpts ^. rabbitmqHost
    port = rabbitOpts ^. rabbitmqPort
    vhost = rabbitOpts ^. rabbitmqVHost