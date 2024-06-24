module Wire.MockInterpreters.Now where

import Data.Time
import Imports
import Polysemy
import Wire.Sem.Now

-- TODO: unify with minibackend
interpretNowConst ::
  UTCTime ->
  Sem (Now : r) a ->
  Sem r a
interpretNowConst time = interpret \case
  Wire.Sem.Now.Get -> pure time
