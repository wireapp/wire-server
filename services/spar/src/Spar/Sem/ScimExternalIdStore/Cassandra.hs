module Spar.Sem.ScimExternalIdStore.Cassandra where

import Cassandra
import Imports
import Polysemy
import qualified Spar.Data as Data
import Spar.Sem.ScimExternalIdStore

scimExternalIdStoreToCassandra ::
  forall m r a.
  (MonadClient m, Member (Embed m) r) =>
  Sem (ScimExternalIdStore ': r) a ->
  Sem r a
scimExternalIdStoreToCassandra =
  interpret $
    embed @m . \case
      Insert tid em uid -> Data.insertScimExternalId tid em uid
      Lookup tid em -> Data.lookupScimExternalId tid em
      Delete tid em -> Data.deleteScimExternalId tid em
