module Wire.MockInterpreters.Events where

import Data.Id
import Imports
import Polysemy
import Polysemy.State
import Wire.API.UserEvent
import Wire.Events

data MiniEvent = MkMiniEvent
  { userId :: UserId,
    mConnId :: Maybe ConnId,
    event :: Event
  }
  deriving stock (Eq, Show)

miniEventInterpreter ::
  (Member (State [MiniEvent]) r) =>
  InterpreterFor Events r
miniEventInterpreter = interpret \case
  GenerateUserEvent uid mconn e -> modify (MkMiniEvent uid mconn (UserEvent e) :)
  GeneratePropertyEvent uid mconn e -> modify (MkMiniEvent uid (Just mconn) (PropertyEvent e) :)
