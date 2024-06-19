module Testlib.Ports where

import Testlib.Types hiding (port)
import Prelude

data PortNamespace
  = NginzSSL
  | NginzHttp2
  | FederatorExternal
  | ServiceInternal Service

port :: (Num a) => PortNamespace -> BackendName -> a
port NginzSSL bn = mkPort 8443 bn
port NginzHttp2 bn = mkPort 8099 bn
port FederatorExternal bn = mkPort 8098 bn
port (ServiceInternal BackgroundWorker) bn = mkPort 8089 bn
port (ServiceInternal Brig) bn = mkPort 8082 bn
port (ServiceInternal Cannon) bn = mkPort 8083 bn
port (ServiceInternal Cargohold) bn = mkPort 8084 bn
port (ServiceInternal FederatorInternal) bn = mkPort 8097 bn
port (ServiceInternal Galley) bn = mkPort 8085 bn
port (ServiceInternal Gundeck) bn = mkPort 8086 bn
port (ServiceInternal Nginz) bn = mkPort 8080 bn
port (ServiceInternal Spar) bn = mkPort 8088 bn
port (ServiceInternal Stern) bn = mkPort 8091 bn

portForDyn :: (Num a) => PortNamespace -> Int -> a
portForDyn ns i = port ns (DynamicBackend i)

mkPort :: (Num a) => Int -> BackendName -> a
mkPort basePort bn =
  let i = case bn of
        BackendA -> 0
        BackendB -> 1
        (DynamicBackend k) -> 1 + k
   in fromIntegral basePort + (fromIntegral i) * 1000

internalServicePorts :: (Num a) => BackendName -> Service -> a
internalServicePorts backend service = port (ServiceInternal service) backend
