{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Brig.Effects.RPC.IO where

import Bilge (HttpT, MonadHttp, RequestId)
import Bilge.IO (Manager, runHttpT)
import Bilge.RPC
import Brig.Effects.RPC
import qualified Brig.RPC as RPC
import Control.Monad.Catch
import Imports
import Polysemy

interpretRpcToIO :: Members '[Final IO] r => Manager -> RequestId -> Sem (RPC ': r) a -> Sem r a
interpretRpcToIO mgr rid = interpret $ \case
  ServiceRequest txt f sm g ->
    embedFinal @IO $ viaHttpIO mgr rid $ RPC.serviceRequestImpl txt f sm g

viaHttpIO :: Manager -> RequestId -> HttpIO a -> IO a
viaHttpIO mgr rid = runHttpT mgr . flip runReaderT rid . runHttpIO

newtype HttpIO a = HttpIO
  { runHttpIO :: ReaderT RequestId (HttpT IO) a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadHttp,
      MonadIO,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadUnliftIO
    )

instance HasRequestId HttpIO where
  getRequestId = HttpIO ask
