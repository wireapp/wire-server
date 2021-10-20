{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bilge.TestSession where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.State (StateT)
import qualified Control.Monad.State as ST
import Imports
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as WaiTest
import qualified Network.Wai.Test.Internal as WaiTest

newtype SessionT m a = SessionT {unSessionT :: ReaderT Wai.Application (StateT WaiTest.ClientState m) a}
  deriving newtype (Functor, Applicative, Monad, MonadThrow, MonadCatch, MonadMask, MonadIO, MonadFail)

instance MonadTrans SessionT where
  lift = SessionT . lift . lift

liftSession :: MonadIO m => WaiTest.Session a -> SessionT m a
liftSession session = SessionT $ do
  app <- ask
  clientState <- lift ST.get
  let resultInState = runReaderT session app
  let resultInIO = ST.evalStateT resultInState clientState
  liftIO resultInIO

runSessionT :: Monad m => SessionT m a -> Wai.Application -> m a
runSessionT session app = ST.evalStateT (runReaderT (unSessionT session) app) WaiTest.initState
