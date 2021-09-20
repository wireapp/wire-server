module Spar.Sem.ScimUserTimesStore.Cassandra where

import Polysemy
import Spar.Sem.ScimUserTimesStore
import Imports
import Cassandra (MonadClient)
import qualified Spar.Data as Data


scimUserTimesStoreToCassandra :: forall m r a. (MonadClient m, Member (Embed m) r) => Sem (ScimUserTimesStore ': r) a -> Sem r a
scimUserTimesStoreToCassandra = interpret $ embed @m . \case
  Write wm -> Data.writeScimUserTimes wm
  Read uid -> Data.readScimUserTimes uid
  Delete uid -> Data.deleteScimUserTimes uid

