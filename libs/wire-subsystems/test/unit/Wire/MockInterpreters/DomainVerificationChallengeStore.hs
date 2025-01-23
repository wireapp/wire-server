module Wire.MockInterpreters.DomainVerificationChallengeStore where

import Imports
import Polysemy
import Polysemy.State
import Wire.DomainVerificationChallengeStore

inMemoryDomainVerificationChallengeStoreInterpreter ::
  (Member (State [StoredDomainVerificationChallenge]) r) =>
  InterpreterFor DomainVerificationChallengeStore r
inMemoryDomainVerificationChallengeStoreInterpreter = interpret $ \case
  Insert dr -> modify (dr :)
  Lookup i -> gets (find ((== i) . challengeId))
  Delete i -> modify (filter ((/= i) . challengeId))
