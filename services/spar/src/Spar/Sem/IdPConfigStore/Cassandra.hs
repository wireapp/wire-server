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

module Spar.Sem.IdPConfigStore.Cassandra
  ( idPToCassandra,
  )
where

import Cassandra
import Control.Lens ((^.))
import Control.Monad.Except
import Data.Id
import qualified Data.List.NonEmpty as NL
import Data.Text (pack)
import Data.X509 (SignedCertificate)
import Imports
import Polysemy
import Polysemy.Error (Error, throw)
import qualified SAML2.WebSSO as SAML
import Spar.Data.Instances ()
import Spar.Error
import Spar.Sem.IdPConfigStore (IdPConfigStore (..), Replaced (..), Replacing (..))
import URI.ByteString
import Wire.API.User.IdentityProvider

idPToCassandra ::
  forall m r a.
  (MonadClient m, Member (Embed m) r, Member (Error IdpDbError) r) =>
  Sem (IdPConfigStore ': r) a ->
  Sem r a
idPToCassandra =
  interpret $
    \case
      InsertConfig iw -> embed @m (runExceptT $ insertIdPConfig iw) >>= either throw pure
      NewHandle tid -> embed @m (runExceptT $ newIdPHandleForTeam tid) >>= either throw pure
      GetConfig i -> embed @m (runExceptT $ getIdPConfig i) >>= either throw pure
      GetIdPByIssuerV1 i -> embed @m (runExceptT $ getIdPByIssuerV1 i) >>= either throw pure
      GetIdPByIssuerV1Maybe i -> embed @m (runExceptT $ getIdPByIssuerV1May i) >>= either throw pure
      GetIdPByIssuerV2 i t -> embed @m (runExceptT $ getIdPByIssuerV2 i t) >>= either throw pure
      GetIdPByIssuerV2Maybe i t -> embed @m (runExceptT $ getIdPByIssuerV2May i t) >>= either throw pure
      GetConfigsByTeam itlt -> embed @m (runExceptT $ getIdPConfigsByTeam itlt) >>= either throw pure
      DeleteConfig idp ->
        let idpid = idp ^. SAML.idpId
            issuer = idp ^. SAML.idpMetadata . SAML.edIssuer
            team = idp ^. SAML.idpExtraInfo . wiTeam
         in embed @m $ deleteIdPConfig idpid issuer team
      SetReplacedBy r r11 -> embed @m $ setReplacedBy r r11
      ClearReplacedBy r -> embed @m $ clearReplacedBy r
      DeleteIssuer i t -> embed @m $ deleteIssuer i t

type IdPConfigRow = (SAML.IdPId, SAML.Issuer, URI, SignedCertificate, [SignedCertificate], TeamId, Maybe WireIdPAPIVersion, [SAML.Issuer], Maybe SAML.IdPId, Maybe Text)

insertIdPConfig ::
  forall m.
  (HasCallStack, MonadClient m, MonadError IdpDbError m) =>
  IdP ->
  m ()
insertIdPConfig idp = do
  ensureDoNotMixApiVersions
  retry x5 . batch $ do
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
        idp ^. SAML.idpExtraInfo . wiReplacedBy,
        Just (unIdPHandle $ idp ^. SAML.idpExtraInfo . wiHandle)
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
    ensureDoNotMixApiVersions :: m ()
    ensureDoNotMixApiVersions = do
      let thisVersion = fromMaybe defWireIdPAPIVersion $ idp ^. SAML.idpExtraInfo . wiApiVersion
          issuer = idp ^. SAML.idpMetadata . SAML.edIssuer

          failIfNot :: WireIdPAPIVersion -> IdP -> m ()
          failIfNot expectedVersion idp' = do
            let actualVersion = fromMaybe defWireIdPAPIVersion $ idp' ^. SAML.idpExtraInfo . wiApiVersion
            unless (actualVersion == expectedVersion) $
              throwError InsertIdPConfigCannotMixApiVersions

      getAllIdPsByIssuerUnsafe issuer >>= mapM_ (failIfNot thisVersion)

    ins :: PrepQuery W IdPConfigRow ()
    ins = "INSERT INTO idp (idp, issuer, request_uri, public_key, extra_public_keys, team, api_version, old_issuers, replaced_by) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"

    -- FUTUREWORK: migrate `spar.issuer_idp` away, `spar.issuer_idp_v2` is enough.
    byIssuer :: PrepQuery W (SAML.Issuer, TeamId, SAML.IdPId) ()
    byIssuer = "INSERT INTO issuer_idp_v2 (issuer, team, idp) VALUES (?, ?, ?)"

    byTeam :: PrepQuery W (SAML.IdPId, TeamId) ()
    byTeam = "INSERT INTO team_idp (idp, team) VALUES (?, ?)"

newIdPHandleForTeam ::
  forall m.
  (HasCallStack, MonadClient m, MonadError IdpDbError m) =>
  TeamId ->
  m IdPHandle
newIdPHandleForTeam tid = do
  idpIds <- getIdPIdsByTeam tid
  handles <- mapMaybe runIdentity <$> retry x1 (query sel (params LocalQuorum (Identity idpIds)))
  pure $ IdPHandle $ newUniqueHandle 1 handles
  where
    sel :: PrepQuery R (Identity [SAML.IdPId]) (Identity (Maybe Text))
    sel = "SELECT handle FROM idp WHERE idp IN ?"

newUniqueHandle :: Int -> [Text] -> Text
newUniqueHandle n handles =
  let handle = "IdP " <> pack (show n)
   in if handle `elem` handles
        then newUniqueHandle (n + 1) handles
        else handle

getIdPConfig ::
  forall m.
  (HasCallStack, MonadClient m, MonadError IdpDbError m) =>
  SAML.IdPId ->
  m IdP
getIdPConfig idpid = do
  mbidp <- traverse toIdp =<< retry x1 (query1 sel $ params LocalQuorum (Identity idpid))
  maybe (throwError IdpNotFound) pure mbidp
  where
    mkHandle :: SAML.IdPId -> Maybe Text -> IdPHandle
    mkHandle idpid' = maybe (IdPHandle (pack (take 6 (show idpid')))) IdPHandle

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
        replacedBy,
        mHandle
        ) = do
        let _edCertAuthnResponse = certsHead NL.:| certsTail
            _idpMetadata = SAML.IdPMetadata {..}
            _idpExtraInfo = WireIdP teamId apiVersion oldIssuers replacedBy (mkHandle _idpId mHandle)
        pure $ SAML.IdPConfig {..}

    sel :: PrepQuery R (Identity SAML.IdPId) IdPConfigRow
    sel = "SELECT idp, issuer, request_uri, public_key, extra_public_keys, team, api_version, old_issuers, replaced_by, handle FROM idp WHERE idp = ?"

-- | Get all idps with a given issuer, no matter what team or what idp version.
getAllIdPsByIssuerUnsafe ::
  forall m.
  (HasCallStack, MonadClient m, MonadError IdpDbError m) =>
  SAML.Issuer ->
  m [IdP]
getAllIdPsByIssuerUnsafe issuer = do
  v1Results <- runIdentity <$$> retry x1 (query selV1 $ params LocalQuorum (Identity issuer))
  v2Results <- runIdentity <$$> retry x1 (query selV2 $ params LocalQuorum (Identity issuer))
  mapM getIdPConfig $ v1Results <> v2Results
  where
    selV1 :: PrepQuery R (Identity SAML.Issuer) (Identity SAML.IdPId)
    selV1 = "SELECT idp FROM issuer_idp WHERE issuer = ?"

    selV2 :: PrepQuery R (Identity SAML.Issuer) (Identity SAML.IdPId)
    selV2 = "SELECT idp FROM issuer_idp_v2 WHERE issuer = ?"

-- | Find 'IdPId' without team.
getIdPByIssuerV1 ::
  forall m.
  (HasCallStack, MonadClient m, MonadError IdpDbError m) =>
  SAML.Issuer ->
  m IdP
getIdPByIssuerV1 issuer = getIdPByIssuerV1May issuer >>= maybe (throwError IdpNotFound) pure

getIdPByIssuerV1May ::
  forall m.
  (HasCallStack, MonadClient m, MonadError IdpDbError m) =>
  SAML.Issuer ->
  m (Maybe IdP)
getIdPByIssuerV1May issuer = do
  let v1Results = runIdentity <$$> retry x1 (query selV1 $ params LocalQuorum (Identity issuer))
  let v2Results = runIdentity <$$> retry x1 (query selV2 $ params LocalQuorum (Identity issuer))
  v2Results >>= \case
    [] ->
      v1Results >>= \case
        [] -> pure Nothing
        [idpId] -> do
          -- this table can only contain V1 idps.
          Just <$> getIdPConfig idpId -- 'Nothing' is an internal database consistency issue (dangling IdPId)
        _ : _ : _ -> throwError IdpNonUnique
    [idpId] -> do
      idp <- getIdPConfig idpId
      doNotMixApiVersions WireIdPAPIV1 idp
      pure $ Just idp -- 'Nothing' is an internal database consistency issue (dangling IdPId)
    _ : _ : _ ->
      throwError IdpNonUnique
  where
    selV1 :: PrepQuery R (Identity SAML.Issuer) (Identity SAML.IdPId)
    selV1 = "SELECT idp FROM issuer_idp WHERE issuer = ?"

    selV2 :: PrepQuery R (Identity SAML.Issuer) (Identity SAML.IdPId)
    selV2 = "SELECT idp FROM issuer_idp_v2 WHERE issuer = ?"

getIdPByIssuerV2 ::
  forall m.
  (HasCallStack, MonadClient m, MonadError IdpDbError m) =>
  SAML.Issuer ->
  TeamId ->
  m IdP
getIdPByIssuerV2 issuer tid = getIdPByIssuerV2May issuer tid >>= maybe (throwError IdpNotFound) pure

getIdPByIssuerV2May ::
  forall m.
  (HasCallStack, MonadClient m, MonadError IdpDbError m) =>
  SAML.Issuer ->
  TeamId ->
  m (Maybe IdP)
getIdPByIssuerV2May issuer tid = do
  let v1Results = runIdentity <$$> retry x1 (query selV1 $ params LocalQuorum (Identity issuer))
  let v2Results = runIdentity <$$> retry x1 (query selV2 $ params LocalQuorum (issuer, tid))
  v2Results >>= \case
    [] ->
      v1Results >>= \case
        [] -> pure Nothing
        [_idpId] -> do
          -- this table can only contain V1 idps.
          throwError AttemptToGetV1IssuerViaV2API
        _ : _ : _ -> throwError IdpNonUnique
    [idpId] -> do
      idp <- getIdPConfig idpId
      doNotMixApiVersions WireIdPAPIV2 idp
      pure $ Just idp -- 'Nothing' is an internal database consistency issue (dangling IdPId)
    _ : _ : _ ->
      throwError IdpNonUnique
  where
    selV1 :: PrepQuery R (Identity SAML.Issuer) (Identity SAML.IdPId)
    selV1 = "SELECT idp FROM issuer_idp WHERE issuer = ?"

    selV2 :: PrepQuery R (SAML.Issuer, TeamId) (Identity SAML.IdPId)
    selV2 = "SELECT idp FROM issuer_idp_v2 WHERE issuer = ? and team = ?"

doNotMixApiVersions ::
  forall m.
  (HasCallStack, MonadError IdpDbError m) =>
  WireIdPAPIVersion ->
  IdP ->
  m ()
doNotMixApiVersions expectVersion idp = do
  let actualVersion = fromMaybe defWireIdPAPIVersion (idp ^. SAML.idpExtraInfo . wiApiVersion)
  unless (actualVersion == expectVersion) $ do
    throwError $ case expectVersion of
      WireIdPAPIV1 -> AttemptToGetV1IssuerViaV2API
      WireIdPAPIV2 -> AttemptToGetV2IssuerViaV1API

getIdPConfigsByTeam ::
  (HasCallStack, MonadClient m, MonadError IdpDbError m) =>
  TeamId ->
  m [IdP]
getIdPConfigsByTeam team =
  getIdPIdsByTeam team >>= mapM getIdPConfig

getIdPIdsByTeam ::
  (HasCallStack, MonadClient m, MonadError IdpDbError m) =>
  TeamId ->
  m [SAML.IdPId]
getIdPIdsByTeam team = do
  runIdentity <$$> retry x1 (query sel $ params LocalQuorum (Identity team))
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
  addPrepQuery delIssuerIdpV2 (issuer, team)
  addPrepQuery delTeamIdp (team, idp)
  where
    delDefaultIdp :: PrepQuery W (Identity SAML.IdPId) ()
    delDefaultIdp = "DELETE FROM default_idp WHERE partition_key_always_default = 'default' AND idp = ?"

    delIdp :: PrepQuery W (Identity SAML.IdPId) ()
    delIdp = "DELETE FROM idp WHERE idp = ?"

    delIssuerIdp :: PrepQuery W (Identity SAML.Issuer) ()
    delIssuerIdp = "DELETE FROM issuer_idp WHERE issuer = ?"

    delIssuerIdpV2 :: PrepQuery W (SAML.Issuer, TeamId) ()
    delIssuerIdpV2 = "DELETE FROM issuer_idp_v2 WHERE issuer = ? AND team = ?"

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

-- | If the IdP is 'WireIdPAPIV1', it must be deleted globally, if it is 'WireIdPAPIV2', it
-- must be deleted inside one team.  'V1' can be either in the old table without team index,
-- or in the new one, so we delete both.
deleteIssuer :: (HasCallStack, MonadClient m) => SAML.Issuer -> Maybe TeamId -> m ()
deleteIssuer issuer = \case
  Just tid -> do
    retry x5 $ write delV2 (params LocalQuorum (issuer, tid))
  Nothing -> do
    retry x5 $ write delV1 (params LocalQuorum (Identity issuer))
    retry x5 $ write delV1' (params LocalQuorum (Identity issuer))
  where
    delV1 :: PrepQuery W (Identity SAML.Issuer) ()
    delV1 = "DELETE FROM issuer_idp WHERE issuer = ?"

    delV1' :: PrepQuery W (Identity SAML.Issuer) ()
    delV1' = "DELETE FROM issuer_idp_v2 WHERE issuer = ?"

    delV2 :: PrepQuery W (SAML.Issuer, TeamId) ()
    delV2 = "DELETE FROM issuer_idp_v2 WHERE issuer = ? AND team = ?"
