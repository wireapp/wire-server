module Spar.Sem.Reporter where

import Imports
import qualified Network.Wai as Wai
import Network.Wai.Utilities.Error (Error)
import Polysemy

data Reporter m a where
  Report :: Maybe Wai.Request -> Error -> Reporter m ()

-- TODO(sandy): Inline this definition --- no TH
makeSem ''Reporter
