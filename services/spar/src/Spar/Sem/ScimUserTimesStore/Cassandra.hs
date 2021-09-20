module Spar.Sem.ScimUserTimesStore.Cassandra where

import Cassandra (MonadClient)
import Imports
import Polysemy
import qualified Spar.Data as Data
import Spar.Sem.ScimUserTimesStore

scimUserTimesStoreToCassandra :: forall m r a. (MonadClient m, Member (Embed m) r) => Sem (ScimUserTimesStore ': r) a -> Sem r a
scimUserTimesStoreToCassandra =
  interpret $
    embed @m . \case
      Write wm -> Data.writeScimUserTimes wm
      Read uid -> Data.readScimUserTimes uid
      Delete uid -> Data.deleteScimUserTimes uid
