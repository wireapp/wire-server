module Spar.Sem.IdPRawMetadataStore.Cassandra where

import Cassandra
import Imports
import Polysemy
import qualified Spar.Data as Data
import Spar.Sem.IdPRawMetadataStore

idpRawMetadataStoreToCassandra ::
  forall m r a.
  (MonadClient m, Member (Embed m) r) =>
  Sem (IdPRawMetadataStore ': r) a ->
  Sem r a
idpRawMetadataStoreToCassandra =
  interpret $
    embed @m . \case
      Store i t -> Data.storeIdPRawMetadata i t
      Get i -> Data.getIdPRawMetadata i
      Delete i -> Data.deleteIdPRawMetadata i

