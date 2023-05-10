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

getAllowedDomainsLoop :: ClientEnv -> IORef FederationDomainConfigs -> IO ()
getAllowedDomainsLoop clientEnv env = forever $ do
  getAllowedDomains clientEnv >>= \case
    Left e -> print $ "Could not retrieve the latest list of federation domains from Brig: " <> show e -- TODO: log error or critical!
    Right cfg -> atomicWriteIORef env cfg
  delay <- updateInterval <$> readIORef env
  threadDelay delay