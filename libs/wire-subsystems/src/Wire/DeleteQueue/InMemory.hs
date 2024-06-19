module Wire.DeleteQueue.InMemory where

import Imports
import Polysemy
import Polysemy.State
import Wire.DeleteQueue
import Wire.InternalEvent

inMemoryDeleteQueueInterpreter :: (Member (State [InternalNotification]) r) => InterpreterFor DeleteQueue r
inMemoryDeleteQueueInterpreter = interpret $ \case
  EnqueueUserDeletion uid -> modify (\l -> DeleteUser uid : l)
  EnqueueClientDeletion cid uid mConnId -> modify (\l -> DeleteClient cid uid mConnId : l)
  EnqueueServiceDeletion pid sid -> modify (\l -> DeleteService pid sid : l)
