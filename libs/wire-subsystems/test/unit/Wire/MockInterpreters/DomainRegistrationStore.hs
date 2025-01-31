module Wire.MockInterpreters.DomainRegistrationStore where

import Imports
import Polysemy
import Polysemy.State
import Wire.DomainRegistrationStore

inMemoryDomainRegistrationStoreInterpreter :: (Member (State [StoredDomainRegistration]) r) => InterpreterFor DomainRegistrationStore r
inMemoryDomainRegistrationStoreInterpreter = interpret $ \case
  UpsertInternal dr -> modify ((dr :) . filter ((/= domain dr) . domain))
  LookupInternal d -> gets (find ((== d) . domain))
  DeleteInternal d -> modify (filter ((/= d) . domain))
  LookupByTeamInternal tid -> gets (filter ((== Just tid) . authorizedTeam))
