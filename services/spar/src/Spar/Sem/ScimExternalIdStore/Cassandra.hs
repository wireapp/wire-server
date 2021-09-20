module Spar.Sem.ScimExternalIdStore.Cassandra where

import Cassandra
import Imports
import Polysemy
-- import qualified Spar.Data as Data
import Spar.Sem.ScimExternalIdStore

scimExternalIdStoreToCassandra ::
  forall m r a.
  (MonadClient m, Member (Embed m) r) =>
  Sem (ScimExternalIdStore ': r) a ->
  Sem r a
scimExternalIdStoreToCassandra =
  interpret $ undefined

-- embed . \case
--   Insert ur uid -> Data.insertSAMLUser ur uid
--   Get ur -> Data.getSAMLUser ur
--   GetAnyByIssuer is -> Data.getSAMLAnyUserByIssuer is
--   GetSomeByIssuer is -> Data.getSAMLSomeUsersByIssuer is
--   DeleteByIssuer is -> Data.deleteSAMLUsersByIssuer is
--   Delete uid ur -> Data.deleteSAMLUser uid ur
