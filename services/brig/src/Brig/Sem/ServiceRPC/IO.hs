module Brig.Sem.ServiceRPC.IO where

import Brig.Sem.ServiceRPC

import Brig.Sem.RPC
import Polysemy
import Imports
import Bilge (Request)
import qualified Data.Text.Lazy as LT


interpretServiceRpcToRpc
  :: forall service r a
   . Member RPC r
     => LT.Text
     -> Request -> Sem (ServiceRPC service ': r) a -> Sem r a
interpretServiceRpcToRpc lt r = interpret $ \case
  Request sm f -> serviceRequest lt r sm f
