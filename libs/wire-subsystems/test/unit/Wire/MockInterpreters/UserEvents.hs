module Wire.MockInterpreters.UserEvents where

import Data.Id
import Imports
import Polysemy
import Polysemy.State
import Wire.API.UserEvent
import Wire.UserEvents

data MiniEvent = MkMiniEvent
  { userId :: UserId,
    event :: UserEvent
  }
  deriving stock (Eq, Show)

miniEventInterpreter ::
  (Member (State [MiniEvent]) r) =>
  InterpreterFor UserEvents r
miniEventInterpreter = interpret \case
  GenerateUserEvent uid _mconn e -> modify (MkMiniEvent uid e :)
