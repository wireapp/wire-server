module Brig.Effects.ServiceRPC.IO where

import Bilge (Request)
import Brig.Effects.RPC
import Brig.Effects.ServiceRPC
import Data.Text.Lazy qualified as LT
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
