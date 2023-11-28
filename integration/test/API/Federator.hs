module API.Federator where

import Data.Function
import GHC.Stack
import qualified Network.HTTP.Client as HTTP
import Testlib.Prelude

getMetrics ::
  (HasCallStack, MakesValue domain) =>
  domain ->
  (ServiceMap -> HostPort) ->
  App Response
getMetrics domain service = do
  req <- rawBaseRequestF domain service "i/metrics"
  submit "GET" req

rawBaseRequestF :: (HasCallStack, MakesValue domain) => domain -> (ServiceMap -> HostPort) -> String -> App HTTP.Request
rawBaseRequestF domain getService path = do
  domainV <- objDomain domain
  serviceMap <- getServiceMap domainV

  liftIO . HTTP.parseRequest $
    let HostPort h p = getService serviceMap
     in "http://" <> h <> ":" <> show p <> ("/" <> joinHttpPath (splitHttpPath path))
