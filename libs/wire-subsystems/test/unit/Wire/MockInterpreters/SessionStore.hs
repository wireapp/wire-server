module Wire.MockInterpreters.SessionStore where

import Data.Id
import Data.Map qualified as Map
import Imports
import Polysemy
import Polysemy.State
import Wire.API.User.Auth
import Wire.SessionStore

runInMemorySessionStore :: InterpreterFor SessionStore r
runInMemorySessionStore =
  evalState (mempty :: Map UserId [Cookie ()])
    . inMemorySessionStoreInterpreter
    . raiseUnder

inMemorySessionStoreInterpreter :: (Member (State (Map UserId [Cookie ()])) r) => InterpreterFor SessionStore r
inMemorySessionStoreInterpreter = interpret $ \case
  InsertCookie uid cookie _ttl -> modify $ Map.insertWith (<>) uid [cookie]
  ListCookies uid -> gets (Map.findWithDefault [] uid)
  DeleteAllCookies uid -> modify $ Map.delete uid
  DeleteCookies uid cc -> (error "implement on demand") uid cc
  LookupCookie uid time cid -> (error "implement on demand") uid time cid
