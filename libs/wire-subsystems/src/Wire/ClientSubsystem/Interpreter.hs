module Wire.ClientSubsystem.Interpreter (runClientSubsystem) where

import Data.Id
import Data.Time.Clock
import Imports
import Polysemy
import Wire.ClientStore (ClientStore)
import Wire.ClientStore qualified as ClientStore
import Wire.ClientSubsystem

runClientSubsystem :: (Member ClientStore r) => InterpreterFor ClientSubsystem r
runClientSubsystem = interpret $ \case
  InternalGetActivityTimestamps uid -> internalGetActivityTimestampsImpl uid

internalGetActivityTimestampsImpl :: (Member ClientStore r) => UserId -> Sem r [Maybe UTCTime]
internalGetActivityTimestampsImpl = ClientStore.getActivityTimestamps
