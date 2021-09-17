module Spar.Sem.IdP.Cassandra where

import Cassandra
import Imports
import Polysemy
import qualified Spar.Data as Data
import Spar.Sem.IdP

idPToCassandra ::
  forall m r a.
  (MonadClient m, Member (Embed m) r) =>
  Sem (IdP ': r) a ->
  Sem r a
idPToCassandra =
  interpret $
    embed @m . \case
      StoreConfig iw -> Data.storeIdPConfig iw
      GetConfig i -> Data.getIdPConfig i
      GetIdByIssuerWithoutTeam i -> Data.getIdPIdByIssuerWithoutTeam i
      GetIdByIssuerWithTeam i t -> Data.getIdPIdByIssuerWithTeam i t
      GetConfigsByTeam itlt -> Data.getIdPConfigsByTeam itlt
      DeleteConfig i i11 itlt -> Data.deleteIdPConfig i i11 itlt
      SetReplacedBy r r11 -> Data.setReplacedBy r r11
      ClearReplacedBy r -> Data.clearReplacedBy r
      StoreRawMetadata i t -> Data.storeIdPRawMetadata i t
      GetRawMetadata i -> Data.getIdPRawMetadata i
      DeleteRawMetadata i -> Data.deleteIdPRawMetadata i
