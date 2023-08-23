module Wire.API.FederationUpdate
  ( getFederationDomainConfigs,
    deleteFederationRemoteGalley,
  )
where

import Control.Concurrent.Async
import Control.Exception
import Control.Retry qualified as R
import Data.Domain
import Data.Set qualified as Set
import Data.Text
import Data.Typeable (cast)
import Imports
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.Client (BaseUrl (BaseUrl), ClientEnv (ClientEnv), ClientError, ClientM, Scheme (Http), runClientM)
import Servant.Client.Internal.HttpClient (defaultMakeClientRequest)
import System.Logger qualified as L
import Util.Options
import Wire.API.Routes.FederationDomainConfig
import Wire.API.Routes.Internal.Brig qualified as IAPI
import Wire.API.Routes.Named (namedClient)

getFederationDomainConfigs :: ClientEnv -> IO (Either ClientError FederationDomainConfigs)
getFederationDomainConfigs = runClientM (namedClient @IAPI.API @"get-federation-remotes")

deleteFederationRemoteGalley :: Domain -> ClientEnv -> IO (Either ClientError ())
deleteFederationRemoteGalley dom = runClientM $ namedClient @IAPI.API @"delete-federation-remote-from-galley" dom
