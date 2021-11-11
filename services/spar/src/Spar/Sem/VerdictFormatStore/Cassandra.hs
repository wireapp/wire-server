module Spar.Sem.VerdictFormatStore.Cassandra where

import Cassandra
import Imports hiding (MonadReader (..), Reader)
import Polysemy
import qualified Spar.Data as Data
import Spar.Sem.VerdictFormatStore

verdictFormatStoreToCassandra ::
  forall m r a.
  (MonadClient m, Member (Embed m) r) =>
  Sem (VerdictFormatStore ': r) a ->
  Sem r a
verdictFormatStoreToCassandra = interpret $ \case
  Store ndt itla vf -> embed @m $ Data.storeVerdictFormat ndt itla vf
  Get itla -> embed @m $ Data.getVerdictFormat itla

