module Wire.UserEvents.Interpreter where

import Imports
import Polysemy
import Wire.UserEvents

interpretUserEvents :: InterpreterFor UserEvents r
interpretUserEvents = interpret $ \case
  -- TODO
  OnUserEvent _ _ _ -> pure ()
