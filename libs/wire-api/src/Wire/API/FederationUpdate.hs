module Wire.API.FederationUpdate where

import Imports
import Wire.API.Routes.FederationDomainConfig (FederationDomainConfigs (updateInterval))
import qualified Wire.API.Routes.Internal.Brig as IAPI
import Servant.Client (runClientM, ClientError, ClientEnv)
import Wire.API.Routes.Named (namedClient)
import Servant.Client.Internal.HttpClient (ClientM)

getFedRemotes :: ClientM FederationDomainConfigs
getFedRemotes = namedClient @IAPI.API @"get-federation-remotes"

-- Initial function for getting the set of domains from brig, and an update interval
getAllowedDomainsInitial :: ClientEnv -> IO FederationDomainConfigs
getAllowedDomainsInitial clientEnv =
  let oneSec = 1000000 -- microsends
      go :: IO FederationDomainConfigs
      go = do
        getAllowedDomains clientEnv >>= \case
          Right s -> pure s
          Left e -> do
            print $ "Could not retrieve the latest list of federation domains from Brig: " <> show e -- TODO: log error or critical!
            threadDelay oneSec
            go
  in go

getAllowedDomains :: ClientEnv -> IO (Either ClientError FederationDomainConfigs)
getAllowedDomains = runClientM getFedRemotes

type FedUpdateCallback = FederationDomainConfigs -> FederationDomainConfigs -> IO ()

-- The callback takes the previous and the new values of the federation domain configs
-- and runs a given action. This function is not called if a new config value cannot be fetched.
getAllowedDomainsLoop :: ClientEnv -> IORef FederationDomainConfigs -> FedUpdateCallback -> IO ()
getAllowedDomainsLoop clientEnv env callback = forever $ do
  getAllowedDomains clientEnv >>= \case
    Left e -> print $ "Could not retrieve the latest list of federation domains from Brig: " <> show e -- TODO: log error or critical!
    Right cfg -> do
      old <- readIORef env
      callback old cfg
      atomicWriteIORef env cfg
  delay <- updateInterval <$> readIORef env
  threadDelay delay

-- A version where the callback isn't needed. Most of the services don't care about
-- when the list changes, just that they have the new list and can use it as-is
getAllowedDomainsLoop' :: ClientEnv -> IORef FederationDomainConfigs -> IO ()
getAllowedDomainsLoop' c r = getAllowedDomainsLoop c r $ \_ _ -> pure ()