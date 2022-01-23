-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

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
