{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Wire.Util where

import Imports
import Control.Monad.Catch
import Test.Tasty.Options
import Data.Tagged
import Test.Tasty
import Test.Tasty.HUnit
import Options.Applicative
import Data.Aeson
import Util.Options
import Wire.BackgroundWorker.Env hiding (federatorInternal, galley)
import qualified Wire.BackgroundWorker.Env as E
import Wire.BackgroundWorker.Options hiding (federatorInternal, galley)
import Wire.BackgroundWorker.Util

data IntegrationConfig = IntegrationConfig
  { galley :: Endpoint
  , federatorInternal :: Endpoint
  }
  deriving (Show, Generic)
instance FromJSON IntegrationConfig

newtype ServiceConfigFile = ServiceConfigFile String
  deriving (Eq, Ord, Typeable)

instance IsOption ServiceConfigFile where
  defaultValue = ServiceConfigFile "/etc/wire/background-worker/conf/background-worker.yaml"
  parseValue = fmap ServiceConfigFile . safeRead
  optionName = pure "service-config"
  optionHelp = pure "Service config file to read from"
  optionCLParser =
    fmap ServiceConfigFile $
      strOption $
        ( short (untag (pure 's' :: Tagged ServiceConfigFile Char))
            <> long (untag (optionName :: Tagged ServiceConfigFile String))
            <> help (untag (optionHelp :: Tagged ServiceConfigFile String))
        )

newtype TestM a = TestM { runTestM :: ReaderT TestSetup IO a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader TestSetup,
      MonadIO,
      MonadCatch,
      MonadThrow,
      MonadMask,
      MonadUnliftIO,
      MonadFail
    )

data TestSetup = TestSetup
  { opts :: Opts
  , iConf :: IntegrationConfig
  }

test :: IO TestSetup -> TestName -> TestM a -> TestTree
test s n h = testCase n runTest
  where
    runTest :: Assertion
    runTest = do
      setup <- s
      void . flip runReaderT setup . runTestM $ h

natAppT :: AppT IO a -> TestM a
natAppT app =
  TestM $ do
    e <- ask
    e' <- liftIO $ setupToEnv e
    liftIO $ runReaderT (unAppT app) e'

setupToEnv :: TestSetup -> IO Env
setupToEnv setup = do
  e <- mkEnv $ setup.opts
  pure $ e
    { E.federatorInternal = federatorInternal $ iConf $ setup
    , E.galley = galley $ iConf $ setup
    }

runTestAppT :: MonadIO m => TestSetup -> AppT m a -> Int -> m a
runTestAppT setup app federatorPort = do
  env <- liftIO $ setupToEnv setup
  runReaderT (unAppT app) $ env
    { E.federatorInternal = (E.federatorInternal env) { _epPort  = fromIntegral federatorPort }
    }

data FakeEnvelope = FakeEnvelope
  { rejections :: IORef [Bool],
    acks :: IORef Int
  }

newFakeEnvelope :: IO FakeEnvelope
newFakeEnvelope =
  FakeEnvelope
    <$> newIORef []
    <*> newIORef 0

instance RabbitMQEnvelope FakeEnvelope where
  ack e = atomicModifyIORef' e.acks $ \a -> (a + 1, ())
  reject e requeueFlag = atomicModifyIORef' e.rejections $ \r -> (r <> [requeueFlag], ())