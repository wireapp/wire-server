module Wire.MockInterpreters.IndexedUserStore where

import Imports
import Polysemy
import Wire.IndexedUserStore

inMemoryIndexedUserStoreInterpreter :: InterpreterFor IndexedUserStore r
inMemoryIndexedUserStoreInterpreter =
  interpret $ \case
    Upsert {} -> error "IndexedUserStore: unimplemented in memory interpreter"
    UpdateTeamSearchVisibilityInbound {} -> error "IndexedUserStore: unimplemented in memory interpreter"
    BulkUpsert {} -> error "IndexedUserStore: unimplemented in memory interpreter"
    DoesIndexExist -> error "IndexedUserStore: unimplemented in memory interpreter"
    SearchUsers {} -> error "IndexedUserStore: unimplemented in memory interpreter"
    PaginateTeamMembers {} -> error "IndexedUserStore: unimplemented in memory interpreter"
