{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

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

module Spar.Sem.SAMLUserStore.Cassandra
  ( samlUserStoreToCassandra,
  )
where

import Cassandra as Cas
import Control.Lens
import Control.Monad.Except
import Data.Id
import Imports
import Polysemy
import qualified SAML2.WebSSO as SAML
import qualified Spar.Data as Data
import Spar.Data.Instances ()
import Spar.Sem.SAMLUserStore

samlUserStoreToCassandra ::
  forall m r a.
  (MonadClient m, Member (Embed m) r) =>
  Sem (SAMLUserStore ': r) a ->
  Sem r a
samlUserStoreToCassandra =
  interpret $
    embed . \case
      Insert ur uid -> insertSAMLUser ur uid
      Get ur -> getSAMLUser ur
      GetAnyByIssuer is -> getSAMLAnyUserByIssuer is
      GetSomeByIssuer is -> getSAMLSomeUsersByIssuer is
      DeleteByIssuer is -> deleteSAMLUsersByIssuer is
      Delete uid ur -> deleteSAMLUser uid ur

-- | Add new user.  If user with this 'SAML.UserId' exists, overwrite it.
insertSAMLUser :: (HasCallStack, MonadClient m) => SAML.UserRef -> UserId -> m ()
insertSAMLUser (SAML.UserRef tenant subject) uid = retry x5 . write ins $ params LocalQuorum (tenant, Data.normalizeQualifiedNameId subject, subject, uid)
  where
    ins :: PrepQuery W (SAML.Issuer, Data.NormalizedUNameID, SAML.NameID, UserId) ()
    ins = "INSERT INTO user_v2 (issuer, normalized_uname_id, sso_id, uid) VALUES (?, ?, ?, ?)"

-- | Sometimes we only need to know if it's none or more, so this function returns the first one.
getSAMLAnyUserByIssuer :: (HasCallStack, MonadClient m) => SAML.Issuer -> m (Maybe UserId)
getSAMLAnyUserByIssuer issuer =
  runIdentity
    <$$> (retry x1 . query1 sel $ params LocalQuorum (Identity issuer))
  where
    sel :: PrepQuery R (Identity SAML.Issuer) (Identity UserId)
    sel = "SELECT uid FROM user_v2 WHERE issuer = ? LIMIT 1"

-- | Sometimes (eg., for IdP deletion), we can start anywhere with deleting all users in an
-- IdP, and if we don't get all users we just try again when we're done with these.
getSAMLSomeUsersByIssuer :: (HasCallStack, MonadClient m) => SAML.Issuer -> m [(SAML.UserRef, UserId)]
getSAMLSomeUsersByIssuer issuer =
  (_1 %~ SAML.UserRef issuer)
    <$$> (retry x1 . query sel $ params LocalQuorum (Identity issuer))
  where
    sel :: PrepQuery R (Identity SAML.Issuer) (SAML.NameID, UserId)
    sel = "SELECT sso_id, uid FROM user_v2 WHERE issuer = ? LIMIT 2000"

-- | Lookup a brig 'UserId' by IdP issuer and NameID.
--
-- NB: It is not allowed for two distinct wire users from two different teams to have the same
-- 'UserRef'.  RATIONALE: this allows us to implement 'getSAMLUser' without adding 'TeamId' to
-- 'UserRef' (which in turn would break the (admittedly leaky) abstarctions of saml2-web-sso).
getSAMLUser :: (HasCallStack, MonadClient m) => SAML.UserRef -> m (Maybe UserId)
getSAMLUser uref = do
  mbUid <- getSAMLUserNew uref
  case mbUid of
    Nothing -> migrateLegacy uref
    Just uid -> pure $ Just uid
  where
    getSAMLUserNew :: (HasCallStack, MonadClient m) => SAML.UserRef -> m (Maybe UserId)
    getSAMLUserNew (SAML.UserRef tenant subject) =
      runIdentity
        <$$> (retry x1 . query1 sel $ params LocalQuorum (tenant, Data.normalizeQualifiedNameId subject))
      where
        sel :: PrepQuery R (SAML.Issuer, Data.NormalizedUNameID) (Identity UserId)
        sel = "SELECT uid FROM user_v2 WHERE issuer = ? AND normalized_uname_id = ?"

    migrateLegacy :: (HasCallStack, MonadClient m) => SAML.UserRef -> m (Maybe UserId)
    migrateLegacy uref' = do
      mbUid <- getSAMLUserLegacy uref'
      for mbUid $ \uid -> do
        insertSAMLUser uref' uid
        pure uid

    getSAMLUserLegacy :: (HasCallStack, MonadClient m) => SAML.UserRef -> m (Maybe UserId)
    getSAMLUserLegacy (SAML.UserRef tenant subject) =
      runIdentity
        <$$> (retry x1 . query1 sel $ params LocalQuorum (tenant, subject))
      where
        sel :: PrepQuery R (SAML.Issuer, SAML.NameID) (Identity UserId)
        sel = "SELECT uid FROM user WHERE issuer = ? AND sso_id = ?"

deleteSAMLUsersByIssuer :: (HasCallStack, MonadClient m) => SAML.Issuer -> m ()
deleteSAMLUsersByIssuer issuer = retry x5 . write del $ params LocalQuorum (Identity issuer)
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
    deleteSAMLUserNew (SAML.UserRef tenant subject) = retry x5 . write del $ params LocalQuorum (tenant, Data.normalizeQualifiedNameId subject)
      where
        del :: PrepQuery W (SAML.Issuer, Data.NormalizedUNameID) ()
        del = "DELETE FROM user_v2 WHERE issuer = ? AND normalized_uname_id = ?"
    deleteSAMLUserLegacy :: (HasCallStack, MonadClient m) => SAML.UserRef -> m ()
    deleteSAMLUserLegacy (SAML.UserRef tenant subject) = retry x5 . write del $ params LocalQuorum (tenant, subject)
      where
        del :: PrepQuery W (SAML.Issuer, SAML.NameID) ()
        del = "DELETE FROM user WHERE issuer = ? AND sso_id = ?"
