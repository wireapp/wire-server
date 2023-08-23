module Testlib.Ports where

import Testlib.Service qualified as Service
import Prelude

data PortNamespace
  = NginzSSL
  | FederatorExternal
  | ServiceInternal Service.Service

port :: Num a => PortNamespace -> Service.BackendName -> a
port NginzSSL bn = mkPort 8443 bn
port FederatorExternal bn = mkPort 8098 bn
port (ServiceInternal Service.BackgroundWorker) bn = mkPort 8089 bn
port (ServiceInternal Service.Brig) bn = mkPort 8082 bn
port (ServiceInternal Service.Cannon) bn = mkPort 8083 bn
port (ServiceInternal Service.Cargohold) bn = mkPort 8084 bn
port (ServiceInternal Service.FederatorInternal) bn = mkPort 8097 bn
port (ServiceInternal Service.Galley) bn = mkPort 8085 bn
port (ServiceInternal Service.Gundeck) bn = mkPort 8086 bn
port (ServiceInternal Service.Nginz) bn = mkPort 8080 bn
port (ServiceInternal Service.Spar) bn = mkPort 8088 bn
port (ServiceInternal Service.Stern) bn = mkPort 8091 bn

portForDyn :: Num a => PortNamespace -> Int -> a
portForDyn ns i = port ns (Service.DynamicBackend i)

mkPort :: Num a => Int -> Service.BackendName -> a
mkPort basePort bn =
  let i = case bn of
        Service.BackendA -> 0
        Service.BackendB -> 1
        (Service.DynamicBackend k) -> 1 + k
   in fromIntegral basePort + (fromIntegral i) * 1000

internalServicePorts :: Num a => Service.BackendName -> Service.Service -> a
internalServicePorts backend service = port (ServiceInternal service) backend
