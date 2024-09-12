module Wire.MockInterpreters.ActivationCodeStore where

import Data.Id
import Data.Map
import Imports
import Polysemy
import Polysemy.State
import Wire.API.User.Activation
import Wire.ActivationCodeStore (ActivationCodeStore (..))
import Wire.UserKeyStore

inMemoryActivationCodeStoreInterpreter :: (Member (State (Map EmailKey (Maybe UserId, ActivationCode))) r) => InterpreterFor ActivationCodeStore r
inMemoryActivationCodeStoreInterpreter = interpret \case LookupActivationCode ek -> gets (!? ek)
