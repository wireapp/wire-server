module Testlib.Ports where

import Testlib.Types
import Prelude

data PortNamespace
  = NginzSSL
  | NginzHttp2
  | FederatorExternal
  | ServiceInternal Service
  deriving (Show, Eq)

servicePort :: (Num a) => PortNamespace -> BackendName -> a
servicePort NginzSSL bn = mkPort 8443 bn
servicePort NginzHttp2 bn = mkPort 8099 bn
servicePort FederatorExternal bn = mkPort 8098 bn
servicePort (ServiceInternal BackgroundWorker) bn = mkPort 8089 bn
servicePort (ServiceInternal Brig) bn = mkPort 8082 bn
servicePort (ServiceInternal Cannon) bn = mkPort 8083 bn
servicePort (ServiceInternal Cargohold) bn = mkPort 8084 bn
servicePort (ServiceInternal FederatorInternal) bn = mkPort 8097 bn
servicePort (ServiceInternal Galley) bn = mkPort 8085 bn
servicePort (ServiceInternal Gundeck) bn = mkPort 8086 bn
servicePort (ServiceInternal Nginz) bn = mkPort 8080 bn
servicePort (ServiceInternal WireProxy) bn = mkPort 8087 bn
servicePort (ServiceInternal Spar) bn = mkPort 8088 bn
servicePort (ServiceInternal Stern) bn = mkPort 8091 bn
servicePort (ServiceInternal WireServerEnterprise) bn = mkPort 8079 bn

portForDyn :: (Num a) => PortNamespace -> Int -> a
portForDyn ns i = servicePort ns (DynamicBackend i)

mkPort :: (Num a) => Int -> BackendName -> a
mkPort basePort bn =
  let i = case bn of
        BackendA -> 0
        BackendB -> 1
        (DynamicBackend k) -> 1 + k
   in fromIntegral basePort + (fromIntegral i) * 1000

internalServicePorts :: (Num a) => BackendName -> Service -> a
internalServicePorts backend service = servicePort (ServiceInternal service) backend
