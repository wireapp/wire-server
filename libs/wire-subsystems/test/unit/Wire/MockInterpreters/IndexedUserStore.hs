module Wire.MockInterpreters.IndexedUserStore where

import Imports
import Polysemy
import Wire.API.Team.Size
import Wire.IndexedUserStore

inMemoryIndexedUserStoreInterpreter :: InterpreterFor IndexedUserStore r
inMemoryIndexedUserStoreInterpreter =
  interpret $ \case
    Upsert {} -> pure ()
    UpdateTeamSearchVisibilityInbound {} -> pure ()
    BulkUpsert {} -> pure ()
    DoesIndexExist -> pure True
    SearchUsers {} -> error "IndexedUserStore: unimplemented in memory interpreter"
    PaginateTeamMembers {} -> error "IndexedUserStore: unimplemented in memory interpreter"
    GetTeamSize {} -> pure $ TeamSize 1
