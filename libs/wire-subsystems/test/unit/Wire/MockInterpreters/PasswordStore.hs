module Wire.MockInterpreters.PasswordStore where

import Data.Id
import Data.Map qualified as Map
import Imports
import Polysemy
import Polysemy.State
import Wire.API.Password
import Wire.PasswordStore

runInMemoryPasswordStoreInterpreter :: InterpreterFor PasswordStore r
runInMemoryPasswordStoreInterpreter = evalState (mempty :: Map UserId Password) . inMemoryPasswordStoreInterpreter . raiseUnder

inMemoryPasswordStoreInterpreter :: (Member (State (Map UserId Password)) r) => InterpreterFor PasswordStore r
inMemoryPasswordStoreInterpreter = interpret $ \case
  UpsertHashedPassword uid password -> modify $ Map.insert uid password
  LookupHashedPassword uid -> gets $ Map.lookup uid
  LookupHashedProviderPassword _uid -> error ("Implement as needed" :: String)
