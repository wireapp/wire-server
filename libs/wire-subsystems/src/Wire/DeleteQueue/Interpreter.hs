module Wire.DeleteQueue.Interpreter where

import Imports
import Polysemy
import Wire.DeleteQueue

runDeleteQueue :: InterpreterFor DeleteQueue r
runDeleteQueue = interpret $ \case
  EnqueueUserDeletion userId -> enqueueUserDeletionImp userId
  EnqueueClientDeletion clientId userId mConnId -> enqueueClientDeletionImp clientId userId mConnId
  EnqueueServiceDeletion providerId serviceId -> enqueueServiceDeletionImp providerId serviceId

enqueueUserDeletionImp :: a
enqueueUserDeletionImp = undefined

enqueueClientDeletionImp :: a
enqueueClientDeletionImp = undefined

enqueueServiceDeletionImp :: a
enqueueServiceDeletionImp = undefined
