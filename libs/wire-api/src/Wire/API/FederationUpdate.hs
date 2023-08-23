module Wire.API.FederationUpdate
  ( getFederationDomainConfigs,
    deleteFederationRemoteGalley,
  )
where

import Data.Domain
import Imports
import Servant.Client (ClientEnv, ClientError, runClientM)
import Wire.API.Routes.FederationDomainConfig
import Wire.API.Routes.Internal.Brig qualified as IAPI
import Wire.API.Routes.Named (namedClient)

getFederationDomainConfigs :: ClientEnv -> IO (Either ClientError FederationDomainConfigs)
getFederationDomainConfigs = runClientM $ namedClient @IAPI.API @"get-federation-remotes"

deleteFederationRemoteGalley :: Domain -> ClientEnv -> IO (Either ClientError ())
deleteFederationRemoteGalley dom = runClientM $ namedClient @IAPI.API @"delete-federation-remote-from-galley" dom
