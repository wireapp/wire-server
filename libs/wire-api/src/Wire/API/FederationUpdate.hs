module Wire.API.FederationUpdate
  ( syncFedDomainConfigs,
    SyncFedDomainConfigsCallback (..),
    emptySyncFedDomainConfigsCallback,
  )
where

import Control.Concurrent.Async
import Control.Exception
import qualified Control.Retry as R
import qualified Data.Set as Set
import Data.Text
import Imports
import Network.HTTP.Client
import Servant.Client
import qualified System.Logger as L
import Util.Options
import Wire.API.Routes.FederationDomainConfig
import qualified Wire.API.Routes.Internal.Brig as IAPI
import Wire.API.Routes.Named (namedClient)

-- | 'FedUpdateCallback' is not called if a new settings cannot be fetched, or if they are
-- equal to the old settings.
syncFedDomainConfigs :: Endpoint -> L.Logger -> SyncFedDomainConfigsCallback -> IO (IORef FederationDomainConfigs, Async ())
syncFedDomainConfigs (Endpoint h p) log' cb = do
  let baseUrl = BaseUrl Http (unpack h) (fromIntegral p) ""
  clientEnv <- newManager defaultManagerSettings <&> \mgr -> ClientEnv mgr baseUrl Nothing defaultMakeClientRequest
  ioref <- newIORef =<< initialize log' clientEnv
  updateDomainsThread <- async $ loop log' clientEnv cb ioref
  pure (ioref, updateDomainsThread)

-- | Initial function for getting the set of domains from brig, and an update interval
initialize :: L.Logger -> ClientEnv -> IO FederationDomainConfigs
initialize logger clientEnv =
  let -- keep trying every 3s for one minute
      policy :: R.RetryPolicy
      policy = R.constantDelay 3_081_003 <> R.limitRetries 20

      go :: IO (Maybe FederationDomainConfigs)
      go = do
        fetch clientEnv >>= \case
          Right s -> pure $ Just s
          Left e -> do
            L.log logger L.Info $
              L.msg (L.val "Failed to reach brig for federation setup, retrying...")
                L.~~ "error" L..= show e
            pure Nothing
   in R.retrying policy (const (pure . isNothing)) (const go) >>= \case
        Just c -> pure c
        Nothing -> throwIO $ ErrorCall "*** Failed to reach brig for federation setup, giving up!"

loop :: L.Logger -> ClientEnv -> SyncFedDomainConfigsCallback -> IORef FederationDomainConfigs -> IO ()
loop logger clientEnv (SyncFedDomainConfigsCallback callback) env = forever $
  catch go $ \(e :: SomeException) -> do
    -- log synchronous exceptions
    case fromException e of
      Just (SomeAsyncException _) -> pure ()
      Nothing ->
        L.log logger L.Error $
          L.msg (L.val "Federation domain sync thread died, restarting domain synchronization.")
            L.~~ "error" L..= displayException e
  where
    go = do
      fetch clientEnv >>= \case
        Left e ->
          L.log logger L.Info $
            L.msg (L.val "Could not retrieve an updated list of federation domains from Brig; I'll keep trying!")
              L.~~ "error" L..= displayException e
        Right new -> do
          old <- readIORef env
          unless (domainListsEqual old new) $ callback old new
          atomicWriteIORef env new
      delay <- updateInterval <$> readIORef env
      threadDelay (delay * 1_000_000)

    domainListsEqual o n =
      Set.fromList (domain <$> remotes o)
        == Set.fromList (domain <$> remotes n)

fetch :: ClientEnv -> IO (Either ClientError FederationDomainConfigs)
fetch = runClientM (namedClient @IAPI.API @"get-federation-remotes")

-- | The callback takes the previous and the new settings and runs a given action.
newtype SyncFedDomainConfigsCallback = SyncFedDomainConfigsCallback
  { fromFedUpdateCallback ::
      FederationDomainConfigs -> -- old value
      FederationDomainConfigs -> -- new value
      IO ()
  }

emptySyncFedDomainConfigsCallback :: SyncFedDomainConfigsCallback
emptySyncFedDomainConfigsCallback = SyncFedDomainConfigsCallback $ \_ _ -> pure ()
