module Wire.UserEvents.Null where

import Imports
import Polysemy
import Wire.UserEvents

nullUserEventsInterpreter :: InterpreterFor UserEvents r
nullUserEventsInterpreter = interpret \case
  GenerateUserEvent {} -> pure ()
