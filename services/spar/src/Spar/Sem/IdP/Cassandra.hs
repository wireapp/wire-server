module Spar.Sem.IdP.Cassandra where

import Cassandra
import Control.Lens ((^.))
import Imports
import Polysemy
import qualified SAML2.WebSSO.Types as SAML
import qualified Spar.Data as Data
import Spar.Sem.IdP
import Wire.API.User.IdentityProvider (wiTeam)

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
      DeleteConfig idp ->
        let idpid = idp ^. SAML.idpId
            issuer = idp ^. SAML.idpMetadata . SAML.edIssuer
            team = idp ^. SAML.idpExtraInfo . wiTeam
         in Data.deleteIdPConfig idpid issuer team
      SetReplacedBy r r11 -> Data.setReplacedBy r r11
      ClearReplacedBy r -> Data.clearReplacedBy r
