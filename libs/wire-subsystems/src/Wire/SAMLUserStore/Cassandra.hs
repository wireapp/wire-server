{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.SAMLUserStore.Cassandra
  ( samlUserStoreToCassandra,
    getSAMLUserRaw,
    lookupSAMLUserLegacy,
    insertSAMLUser,
    deleteSAMLUserByIssuer,
    deleteSAMLUser,
  )
where

import Cassandra as Cas
import Data.Bifunctor (first)
import Data.Id
import Imports
import Polysemy
import SAML2.WebSSO qualified as SAML
import {- instance Cql NormalizedUNameID -} Wire.API.User.Saml (NormalizedUNameID, normalizeQualifiedNameId)
import {- instance Cql SAML.Issuer -} Wire.IdPConfigStore.Orphans ()
import Wire.SAMLUserStore

samlUserStoreToCassandra ::
  forall m r a.
  (MonadClient m, Member (Embed m) r) =>
  Sem (SAMLUserStore ': r) a ->
  Sem r a
samlUserStoreToCassandra =
  interpret $
    embed
      . \case
        Insert ur uid -> insertSAMLUser ur uid
        Get ur -> getSAMLUser ur
        DeleteByIssuer is -> deleteSAMLUserByIssuer is
        Delete uid ur -> deleteSAMLUser uid ur
        GetAllByIssuer is -> getAllSAMLUsersByIssuer is

-- | Replaces `getSAML{Some,Any}UsersByIssuer`.
-- Since we currently do not have a team id stored together with the SAML user in user_v2
-- we must get all and filter manually by asking brig for the team id when deleting an IdP.
-- FUTUREWORK: to migrate to a new table that contains the team id.
getAllSAMLUsersByIssuer :: (HasCallStack, MonadClient m) => SAML.Issuer -> m [(SAML.UserRef, UserId)]
getAllSAMLUsersByIssuer issuer = do
  page <- retry x1 (paginate selByIssuer (paramsP LocalQuorum (Identity issuer) size))
  drainPage issuer page
  where
    size = 200

    selByIssuer :: PrepQuery R (Identity SAML.Issuer) (SAML.NameID, UserId)
    selByIssuer = "SELECT sso_id, uid FROM user_v2 WHERE issuer = ?"

    drainPage iss pg =
      let items = (Data.Bifunctor.first (SAML.UserRef iss)) <$> Cas.result pg
       in (items <>) <$> if Cas.hasMore pg then Cas.liftClient (Cas.nextPage pg) >>= drainPage iss else pure []

-- | Add new user.  If user with this 'SAML.UserId' exists, overwrite it.
insertSAMLUser :: (HasCallStack, MonadClient m) => SAML.UserRef -> UserId -> m ()
insertSAMLUser (SAML.UserRef tenant subject) uid = retry x5 . write ins $ params LocalQuorum (tenant, normalizeQualifiedNameId subject, subject, uid)
  where
    ins :: PrepQuery W (SAML.Issuer, NormalizedUNameID, SAML.NameID, UserId) ()
    ins = "INSERT INTO user_v2 (issuer, normalized_uname_id, sso_id, uid) VALUES (?, ?, ?, ?)"

-- | Lookup a brig 'UserId' by IdP issuer and NameID.
--
-- NB: It is not allowed for two distinct wire users from two different teams to have the same
-- 'UserRef'.  RATIONALE: this allows us to implement 'getSAMLUser' without adding 'TeamId' to
-- 'UserRef' (which in turn would break the (admittedly leaky) abstarctions of saml2-web-sso).
getSAMLUser :: (HasCallStack, MonadClient m) => SAML.UserRef -> m (Maybe UserId)
getSAMLUser uref = do
  mbUid <- getSAMLUserRaw uref
  case mbUid of
    Nothing -> migrateLegacy uref
    Just uid -> pure $ Just uid
  where
    migrateLegacy :: (HasCallStack, MonadClient m) => SAML.UserRef -> m (Maybe UserId)
    migrateLegacy uref' = do
      mbUid <- lookupSAMLUserLegacy uref'
      for mbUid $ \uid -> do
        insertSAMLUser uref' uid
        pure uid

-- | Raw lookup in @user_v2@ without the legacy migrate-on-read side effect.
getSAMLUserRaw :: (HasCallStack, MonadClient m) => SAML.UserRef -> m (Maybe UserId)
getSAMLUserRaw (SAML.UserRef tenant subject) =
  runIdentity
    <$$> (retry x1 . query1 sel $ params LocalQuorum (tenant, normalizeQualifiedNameId subject))
  where
    sel :: PrepQuery R (SAML.Issuer, NormalizedUNameID) (Identity UserId)
    sel = "SELECT uid FROM user_v2 WHERE issuer = ? AND normalized_uname_id = ?"

-- | Lookup in the legacy @user@ table (raw @sso_id@ key, no normalization).
lookupSAMLUserLegacy :: (HasCallStack, MonadClient m) => SAML.UserRef -> m (Maybe UserId)
lookupSAMLUserLegacy (SAML.UserRef tenant subject) =
  runIdentity
    <$$> (retry x1 . query1 sel $ params LocalQuorum (tenant, subject))
  where
    sel :: PrepQuery R (SAML.Issuer, SAML.NameID) (Identity UserId)
    sel = "SELECT uid FROM user WHERE issuer = ? AND sso_id = ?"

deleteSAMLUserByIssuer :: (HasCallStack, MonadClient m) => SAML.Issuer -> m ()
deleteSAMLUserByIssuer issuer = retry x5 . write del $ params LocalQuorum (Identity issuer)
  where
    del :: PrepQuery W (Identity SAML.Issuer) ()
    del = "DELETE FROM user_v2 WHERE issuer = ?"

deleteSAMLUser :: (HasCallStack, MonadClient m) => UserId -> SAML.UserRef -> m ()
deleteSAMLUser uid uref = do
  muidUref <- getSAMLUser uref
  for_ muidUref $ \uidUref ->
    when (uidUref == uid) $ do
      deleteSAMLUserLegacy uref
      deleteSAMLUserNew uref
  where
    deleteSAMLUserNew :: (HasCallStack, MonadClient m) => SAML.UserRef -> m ()
    deleteSAMLUserNew (SAML.UserRef tenant subject) = retry x5 . write del $ params LocalQuorum (tenant, normalizeQualifiedNameId subject)
      where
        del :: PrepQuery W (SAML.Issuer, NormalizedUNameID) ()
        del = "DELETE FROM user_v2 WHERE issuer = ? AND normalized_uname_id = ?"
    deleteSAMLUserLegacy :: (HasCallStack, MonadClient m) => SAML.UserRef -> m ()
    deleteSAMLUserLegacy (SAML.UserRef tenant subject) = retry x5 . write del $ params LocalQuorum (tenant, subject)
      where
        del :: PrepQuery W (SAML.Issuer, SAML.NameID) ()
        del = "DELETE FROM user WHERE issuer = ? AND sso_id = ?"
