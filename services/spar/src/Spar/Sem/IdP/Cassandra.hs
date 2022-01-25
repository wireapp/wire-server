{-# LANGUAGE RecordWildCards #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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
import Control.Monad.Except
import Data.Id
import qualified Data.List.NonEmpty as NL
import Data.X509 (SignedCertificate)
import Imports
import Polysemy
import qualified SAML2.WebSSO as SAML
import Spar.Data.Instances ()
import Spar.Sem.IdP (GetIdPResult (..), IdPConfigStore (..), Replaced (..), Replacing (..))
import URI.ByteString
import Wire.API.User.IdentityProvider

idPToCassandra ::
  forall m r a.
  (MonadClient m, Member (Embed m) r) =>
  Sem (IdPConfigStore ': r) a ->
  Sem r a
idPToCassandra =
  interpret $
    embed @m . \case
      StoreConfig iw -> storeIdPConfig iw
      GetConfig i -> getIdPConfig i
      GetIdByIssuerWithoutTeam i -> getIdPIdByIssuerWithoutTeam i
      GetIdByIssuerWithTeam i t -> getIdPIdByIssuerWithTeam i t
      GetConfigsByTeam itlt -> getIdPConfigsByTeam itlt
      DeleteConfig idp ->
        let idpid = idp ^. SAML.idpId
            issuer = idp ^. SAML.idpMetadata . SAML.edIssuer
            team = idp ^. SAML.idpExtraInfo . wiTeam
         in deleteIdPConfig idpid issuer team
      SetReplacedBy r r11 -> setReplacedBy r r11
      ClearReplacedBy r -> clearReplacedBy r

type IdPConfigRow = (SAML.IdPId, SAML.Issuer, URI, SignedCertificate, [SignedCertificate], TeamId, Maybe WireIdPAPIVersion, [SAML.Issuer], Maybe SAML.IdPId)

-- FUTUREWORK: should be called 'insertIdPConfig' for consistency.
-- FUTUREWORK: enforce that wiReplacedby is Nothing, or throw an error.  there is no
-- legitimate reason to store an IdP that has already been replaced.  and for updating an old
-- one, call 'markReplacedIdP'.
storeIdPConfig ::
  (HasCallStack, MonadClient m) =>
  IdP ->
  m ()
storeIdPConfig idp = retry x5 . batch $ do
  setType BatchLogged
  setConsistency LocalQuorum
  addPrepQuery
    ins
    ( idp ^. SAML.idpId,
      idp ^. SAML.idpMetadata . SAML.edIssuer,
      idp ^. SAML.idpMetadata . SAML.edRequestURI,
      NL.head (idp ^. SAML.idpMetadata . SAML.edCertAuthnResponse),
      NL.tail (idp ^. SAML.idpMetadata . SAML.edCertAuthnResponse),
      -- (the 'List1' is split up into head and tail to make migration from one-element-only easier.)
      idp ^. SAML.idpExtraInfo . wiTeam,
      idp ^. SAML.idpExtraInfo . wiApiVersion,
      idp ^. SAML.idpExtraInfo . wiOldIssuers,
      idp ^. SAML.idpExtraInfo . wiReplacedBy
    )
  addPrepQuery
    byIssuer
    ( idp ^. SAML.idpMetadata . SAML.edIssuer,
      idp ^. SAML.idpExtraInfo . wiTeam,
      idp ^. SAML.idpId
    )
  addPrepQuery
    byTeam
    ( idp ^. SAML.idpId,
      idp ^. SAML.idpExtraInfo . wiTeam
    )
  where
    ins :: PrepQuery W IdPConfigRow ()
    ins = "INSERT INTO idp (idp, issuer, request_uri, public_key, extra_public_keys, team, api_version, old_issuers, replaced_by) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"

    -- FUTUREWORK: migrate `spar.issuer_idp` away, `spar.issuer_idp_v2` is enough.
    byIssuer :: PrepQuery W (SAML.Issuer, TeamId, SAML.IdPId) ()
    byIssuer = "INSERT INTO issuer_idp_v2 (issuer, team, idp) VALUES (?, ?, ?)"

    byTeam :: PrepQuery W (SAML.IdPId, TeamId) ()
    byTeam = "INSERT INTO team_idp (idp, team) VALUES (?, ?)"

getIdPConfig ::
  forall m.
  (HasCallStack, MonadClient m) =>
  SAML.IdPId ->
  m (Maybe IdP)
getIdPConfig idpid =
  traverse toIdp =<< retry x1 (query1 sel $ params LocalQuorum (Identity idpid))
  where
    toIdp :: IdPConfigRow -> m IdP
    toIdp
      ( _idpId,
        -- metadata
        _edIssuer,
        _edRequestURI,
        certsHead,
        certsTail,
        -- extras
        teamId,
        apiVersion,
        oldIssuers,
        replacedBy
        ) = do
        let _edCertAuthnResponse = certsHead NL.:| certsTail
            _idpMetadata = SAML.IdPMetadata {..}
            _idpExtraInfo = WireIdP teamId apiVersion oldIssuers replacedBy
        pure $ SAML.IdPConfig {..}
    sel :: PrepQuery R (Identity SAML.IdPId) IdPConfigRow
    sel = "SELECT idp, issuer, request_uri, public_key, extra_public_keys, team, api_version, old_issuers, replaced_by FROM idp WHERE idp = ?"

-- | Find 'IdPId' without team.  Search both `issuer_idp_v2` and `issuer_idp`; in the former,
-- make sure the result is unique (no two IdPs for two different teams).
getIdPIdByIssuerWithoutTeam ::
  (HasCallStack, MonadClient m) =>
  SAML.Issuer ->
  m (GetIdPResult SAML.IdPId)
getIdPIdByIssuerWithoutTeam issuer = do
  (runIdentity <$$> retry x1 (query selv2 $ params LocalQuorum (Identity issuer))) >>= \case
    [] ->
      (runIdentity <$$> retry x1 (query1 sel $ params LocalQuorum (Identity issuer))) >>= \case
        Just idpid -> pure $ GetIdPFound idpid
        Nothing -> pure GetIdPNotFound
    [idpid] ->
      pure $ GetIdPFound idpid
    idpids@(_ : _ : _) ->
      pure $ GetIdPNonUnique idpids
  where
    sel :: PrepQuery R (Identity SAML.Issuer) (Identity SAML.IdPId)
    sel = "SELECT idp FROM issuer_idp WHERE issuer = ?"

    selv2 :: PrepQuery R (Identity SAML.Issuer) (Identity SAML.IdPId)
    selv2 = "SELECT idp FROM issuer_idp_v2 WHERE issuer = ?"

getIdPIdByIssuerWithTeam ::
  (HasCallStack, MonadClient m) =>
  SAML.Issuer ->
  TeamId ->
  m (Maybe SAML.IdPId)
getIdPIdByIssuerWithTeam issuer tid = do
  runIdentity <$$> retry x1 (query1 sel $ params LocalQuorum (issuer, tid))
  where
    sel :: PrepQuery R (SAML.Issuer, TeamId) (Identity SAML.IdPId)
    sel = "SELECT idp FROM issuer_idp_v2 WHERE issuer = ? and team = ?"

getIdPConfigsByTeam ::
  (HasCallStack, MonadClient m) =>
  TeamId ->
  m [IdP]
getIdPConfigsByTeam team = do
  idpids <- runIdentity <$$> retry x1 (query sel $ params LocalQuorum (Identity team))
  catMaybes <$> mapM getIdPConfig idpids
  where
    sel :: PrepQuery R (Identity TeamId) (Identity SAML.IdPId)
    sel = "SELECT idp FROM team_idp WHERE team = ?"

deleteIdPConfig ::
  (HasCallStack, MonadClient m) =>
  SAML.IdPId ->
  SAML.Issuer ->
  TeamId ->
  m ()
deleteIdPConfig idp issuer team = retry x5 . batch $ do
  setType BatchLogged
  setConsistency LocalQuorum
  addPrepQuery delDefaultIdp (Identity idp)
  addPrepQuery delIdp (Identity idp)
  addPrepQuery delIssuerIdp (Identity issuer)
  addPrepQuery delIssuerIdpV2 (Identity issuer)
  addPrepQuery delTeamIdp (team, idp)
  where
    delDefaultIdp :: PrepQuery W (Identity SAML.IdPId) ()
    delDefaultIdp = "DELETE FROM default_idp WHERE partition_key_always_default = 'default' AND idp = ?"

    delIdp :: PrepQuery W (Identity SAML.IdPId) ()
    delIdp = "DELETE FROM idp WHERE idp = ?"

    delIssuerIdp :: PrepQuery W (Identity SAML.Issuer) ()
    delIssuerIdp = "DELETE FROM issuer_idp WHERE issuer = ?"

    delIssuerIdpV2 :: PrepQuery W (Identity SAML.Issuer) ()
    delIssuerIdpV2 = "DELETE FROM issuer_idp_v2 WHERE issuer = ?"

    delTeamIdp :: PrepQuery W (TeamId, SAML.IdPId) ()
    delTeamIdp = "DELETE FROM team_idp WHERE team = ? and idp = ?"

-- | See also: test case @"{set,clear}ReplacedBy"@ in integration tests ("Test.Spar.DataSpec").
setReplacedBy ::
  (HasCallStack, MonadClient m) =>
  Replaced ->
  Replacing ->
  m ()
setReplacedBy (Replaced old) (Replacing new) = do
  retry x5 . write ins $ params LocalQuorum (new, old)
  where
    ins :: PrepQuery W (SAML.IdPId, SAML.IdPId) ()
    ins = "UPDATE idp SET replaced_by = ? WHERE idp = ?"

-- | See also: 'setReplacedBy'.
clearReplacedBy ::
  (HasCallStack, MonadClient m) =>
  Replaced ->
  m ()
clearReplacedBy (Replaced old) = do
  retry x5 . write ins $ params LocalQuorum (Identity old)
  where
    ins :: PrepQuery W (Identity SAML.IdPId) ()
    ins = "UPDATE idp SET replaced_by = null WHERE idp = ?"
