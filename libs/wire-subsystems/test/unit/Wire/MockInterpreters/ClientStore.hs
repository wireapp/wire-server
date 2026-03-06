module Wire.MockInterpreters.ClientStore where

import Imports
import Polysemy
import Wire.ClientStore

runInMemoryClientStoreInterpreter :: InterpreterFor ClientStore r
runInMemoryClientStoreInterpreter = interpret $ \case
  Upsert {} -> error "not implemented: Upsert"
  Delete {} -> error "not implemented: Delete"
  UpdateLabel {} -> error "not implemented: UpdateLabel"
  UpdateCapabilities {} -> error "not implemented: UpdateCapabilities"
  UpdateLastActive {} -> error "not implemented: UpdateLastActive"
  LookupClient {} -> error "not implemented: LookupClient"
  LookupClients {} -> error "not implemented: LookupClients"
  LookupClientIds {} -> error "not implemented: LookupClientIds"
  LookupClientIdsBulk {} -> error "not implemented: LookupClientIdsBulk"
  LookupClientsBulk {} -> error "not implemented: LookupClientsBulk"
  LookupPubClientsBulk {} -> error "not implemented: LookupPubClientsBulk"
  LookupPrekeyIds {} -> error "not implemented: LookupPrekeyIds"
  GetActivityTimestamps {} -> pure []
  UpdatePrekeys {} -> error "not implemented: UpdatePrekeys"
  ClaimPrekey {} -> error "not implemented: ClaimPrekey"
  AddMLSPublicKeys {} -> error "not implemented: AddMLSPublicKeys"
  LookupMLSPublicKey {} -> error "not implemented: LookupMLSPublicKey"
