module Wire.MockInterpreters.Now where

import Data.Time
import Imports
import Polysemy
import Polysemy.State
import Wire.Sem.Now

interpretNowConst ::
  UTCTime ->
  Sem (Now : r) a ->
  Sem r a
interpretNowConst time = interpret \case
  Wire.Sem.Now.Get -> pure time

interpretNowAsState :: (Member (State UTCTime) r) => InterpreterFor Now r
interpretNowAsState =
  interpret $ \case
    Wire.Sem.Now.Get -> Polysemy.State.get

defaultTime :: UTCTime
defaultTime = UTCTime (ModifiedJulianDay 0) 0

passTime :: (Member (State UTCTime) r) => NominalDiffTime -> Sem r ()
passTime t = modify (addUTCTime t)
