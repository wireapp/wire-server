module Wire.API.FederationUpdate
  ( FedUpdateCallback,
    updateFedDomains,
  )
where

import Control.Concurrent.Async
import Control.Exception (ErrorCall (ErrorCall), throwIO)
import qualified Control.Retry as R
import qualified Data.Set as Set
import Data.Text (unpack)
import Imports
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.Client (BaseUrl (BaseUrl), ClientEnv (ClientEnv), ClientError, Scheme (Http), runClientM)
import Servant.Client.Internal.HttpClient (ClientM, defaultMakeClientRequest)
import qualified System.Logger as L
import Util.Options (Endpoint (..))
import Wire.API.Routes.FederationDomainConfig (FederationDomainConfig (domain), FederationDomainConfigs (remotes, updateInterval))
import qualified Wire.API.Routes.Internal.Brig as IAPI
import Wire.API.Routes.Named (namedClient)

getFedRemotes :: ClientM FederationDomainConfigs
getFedRemotes = namedClient @IAPI.API @"get-federation-remotes"

-- Initial function for getting the set of domains from brig, and an update interval
getAllowedDomainsInitial :: L.Logger -> ClientEnv -> IO FederationDomainConfigs
getAllowedDomainsInitial logger clientEnv =
  let -- keep trying every 3s for one minute
      policy :: R.RetryPolicy
      policy = R.constantDelay 3_081_003 <> R.limitRetries 20

      go :: IO (Maybe FederationDomainConfigs)
      go = do
        getAllowedDomains clientEnv >>= \case
          Right s -> pure $ Just s
          Left e -> do
            L.log logger L.Info $
              L.msg (L.val "Could not retrieve an initial list of federation domains from Brig.")
                L.~~ "error" L..= show e
            pure Nothing
   in R.retrying policy (const (pure . isNothing)) (const go) >>= \case
        Just c -> pure c
        Nothing -> throwIO $ ErrorCall "*** Failed to reach brig for federation setup, giving up!"

getAllowedDomains :: ClientEnv -> IO (Either ClientError FederationDomainConfigs)
getAllowedDomains = runClientM getFedRemotes

-- Old value -> new value -> action
type FedUpdateCallback = FederationDomainConfigs -> FederationDomainConfigs -> IO ()

-- The callback takes the previous and the new values of the federation domain configs
-- and runs a given action. This function is not called if a new config value cannot be fetched.
getAllowedDomainsLoop :: L.Logger -> ClientEnv -> FedUpdateCallback -> IORef FederationDomainConfigs -> IO ()
getAllowedDomainsLoop logger clientEnv callback env = forever $ do
  getAllowedDomains clientEnv >>= \case
    Left e ->
      L.log logger L.Info $
        L.msg (L.val "Could not retrieve an updated list of federation domains from Brig; I'll keep trying!")
          L.~~ "error" L..= show e
    Right new -> do
      old <- readIORef env
      unless (domainListsEqual old new) $ callback old new
      atomicWriteIORef env new
  delay <- updateInterval <$> readIORef env
  threadDelay (delay * 1_000_000)
  where
    domainListsEqual o n =
      Set.fromList (domain <$> remotes o)
        == Set.fromList (domain <$> remotes n)

updateFedDomains :: Endpoint -> L.Logger -> FedUpdateCallback -> IO (IORef FederationDomainConfigs, Async ())
updateFedDomains (Endpoint h p) log' cb = do
  clientEnv <- newManager defaultManagerSettings <&> \mgr -> ClientEnv mgr baseUrl Nothing defaultMakeClientRequest
  ioref <- newIORef =<< getAllowedDomainsInitial log' clientEnv
  updateDomainsThread <- async $ getAllowedDomainsLoop log' clientEnv cb ioref
  pure (ioref, updateDomainsThread)
  where
    baseUrl = BaseUrl Http (unpack h) (fromIntegral p) ""
