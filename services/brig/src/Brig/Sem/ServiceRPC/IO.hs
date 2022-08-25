module Brig.Sem.ServiceRPC.IO where

import Bilge (Request)
import Brig.Sem.RPC
import Brig.Sem.ServiceRPC
import qualified Data.Text.Lazy as LT
import Imports
import Polysemy

interpretServiceRpcToRpc ::
  forall service r a.
  Member RPC r =>
  LT.Text ->
  Request ->
  Sem (ServiceRPC service ': r) a ->
  Sem r a
interpretServiceRpcToRpc lt r = interpret $ \case
  Request sm f -> serviceRequest lt r sm f
