module Wire.DeleteQueue.Interpreter where

import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input

runDeleteQueue InterpreterFor DeleteQueue
runDeleteQueue = interpret $ \case
  EnqueueUserDeletion userId -> enqueueUserDeletionImp userId
  EnqueueClientDeletion clientId userId mConnId -> enqueueClientDeletionImp clientId userId mConnId
  EnqueueServiceDeletion providerId serviceId -> enqueueServiceDeletionImp providerId serviceId


enqueueUserDeletionImp = _

enqueueClientDeletionImp = _

enqueueServiceDeletionImp = _
