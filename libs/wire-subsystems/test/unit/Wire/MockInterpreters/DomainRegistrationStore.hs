module Wire.MockInterpreters.DomainRegistrationStore where

import Imports
import Polysemy
import Polysemy.State
import Wire.DomainRegistrationStore

inMemoryDomainRegistrationStoreInterpreter :: (Member (State [StoredDomainRegistration]) r) => InterpreterFor DomainRegistrationStore r
inMemoryDomainRegistrationStoreInterpreter = interpret $ \case
  Upsert dr -> modify ((dr :) . filter ((/= domain dr) . domain))
  Lookup d -> gets (find ((== d) . domain))
  Delete d -> modify (filter ((/= d) . domain))
