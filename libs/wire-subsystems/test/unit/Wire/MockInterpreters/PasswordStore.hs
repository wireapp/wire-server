module Wire.MockInterpreters.PasswordStore where

import Data.Id
import Data.Map qualified as Map
import Imports
import Polysemy
import Polysemy.State
import Wire.API.Password
import Wire.PasswordStore

inMemoryPasswordStoreInterpreter :: (Member (State (Map UserId Password)) r) => InterpreterFor PasswordStore r
inMemoryPasswordStoreInterpreter = interpret $ \case
  UpsertHashedPassword uid password -> modify $ Map.insert uid password
  LookupHashedPassword uid -> gets $ Map.lookup uid
