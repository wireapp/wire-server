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

module Brig.Provider.DB where

import Cassandra as C
import Data.Id
import Data.Misc
import Imports
import Wire.API.Password as Password
import Wire.API.Provider
import Wire.API.User
import Wire.UserKeyStore

--------------------------------------------------------------------------------
-- Providers

insertAccount ::
  (MonadClient m) =>
  Name ->
  Password ->
  HttpsUrl ->
  Text ->
  m ProviderId
insertAccount name pass url descr = do
  pid <- randomId
  retry x5 $ write cql $ params LocalQuorum (pid, name, pass, url, descr)
  pure pid
  where
    cql :: PrepQuery W (ProviderId, Name, Password, HttpsUrl, Text) ()
    cql = "INSERT INTO provider (id, name, password, url, descr) VALUES (?, ?, ?, ?, ?)"

updateAccountProfile ::
  (MonadClient m) =>
  ProviderId ->
  Maybe Name ->
  Maybe HttpsUrl ->
  Maybe Text ->
  m ()
updateAccountProfile p name url descr = retry x5 . batch $ do
  setType BatchUnLogged
  setConsistency LocalQuorum
  for_ name $ \x -> addPrepQuery cqlName (x, p)
  for_ url $ \x -> addPrepQuery cqlUrl (x, p)
  for_ descr $ \x -> addPrepQuery cqlDescr (x, p)
  where
    cqlName :: PrepQuery W (Name, ProviderId) ()
    cqlName = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE provider SET name = ? WHERE id = ?"
    cqlUrl :: PrepQuery W (HttpsUrl, ProviderId) ()
    cqlUrl = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE provider SET url = ? WHERE id = ?"
    cqlDescr :: PrepQuery W (Text, ProviderId) ()
    cqlDescr = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE provider SET descr = ? WHERE id = ?"

-- | Lookup the raw account data of a (possibly unverified) provider.
lookupAccountData ::
  (MonadClient m) =>
  ProviderId ->
  m (Maybe (Name, Maybe EmailAddress, HttpsUrl, Text))
lookupAccountData p = retry x1 $ query1 cql $ params LocalQuorum (Identity p)
  where
    cql :: PrepQuery R (Identity ProviderId) (Name, Maybe EmailAddress, HttpsUrl, Text)
    cql = "SELECT name, email, url, descr FROM provider WHERE id = ?"

lookupAccount ::
  (MonadClient m) =>
  ProviderId ->
  m (Maybe Provider)
lookupAccount p = (>>= mk) <$> lookupAccountData p
  where
    mk :: (Name, Maybe EmailAddress, HttpsUrl, Text) -> Maybe Provider
    mk (_, Nothing, _, _) = Nothing
    mk (n, Just e, u, d) = Just $! Provider p n e u d

lookupAccountProfile ::
  (MonadClient m) =>
  ProviderId ->
  m (Maybe ProviderProfile)
lookupAccountProfile p = fmap ProviderProfile <$> lookupAccount p

deleteAccount ::
  (MonadClient m) =>
  ProviderId ->
  m ()
deleteAccount pid = retry x5 $ write cql $ params LocalQuorum (Identity pid)
  where
    cql :: PrepQuery W (Identity ProviderId) ()
    cql = "DELETE FROM provider WHERE id = ?"

updateAccountPassword ::
  (MonadClient m) =>
  ProviderId ->
  Password ->
  m ()
updateAccountPassword pid pwd = do
  retry x5 $ write cql $ params LocalQuorum (pwd, pid)
  where
    cql :: PrepQuery W (Password, ProviderId) ()
    cql = "UPDATE provider SET password = ? where id = ?"

--------------------------------------------------------------------------------
-- Unique (Natural) Keys

insertKey ::
  (MonadClient m) =>
  ProviderId ->
  Maybe EmailKey ->
  EmailKey ->
  m ()
insertKey p old new = retry x5 . batch $ do
  setConsistency LocalQuorum
  setType BatchLogged
  for_ old $ \old' -> addPrepQuery cqlKeyDelete (Identity (emailKeyUniq old'))
  addPrepQuery cqlKeyInsert (emailKeyUniq new, p)
  addPrepQuery cqlEmail (emailKeyOrig new, p)
  where
    cqlKeyInsert :: PrepQuery W (Text, ProviderId) ()
    cqlKeyInsert = "INSERT INTO provider_keys (key, provider) VALUES (?, ?)"

    cqlKeyDelete :: PrepQuery W (Identity Text) ()
    cqlKeyDelete = "DELETE FROM provider_keys WHERE key = ?"

    cqlEmail :: PrepQuery W (EmailAddress, ProviderId) ()
    cqlEmail = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE provider SET email = ? WHERE id = ?"

lookupKey ::
  (MonadClient m) =>
  EmailKey ->
  m (Maybe ProviderId)
lookupKey k =
  fmap (fmap runIdentity) $
    retry x1 $
      query1 cql $
        params LocalQuorum (Identity (emailKeyUniq k))
  where
    cql :: PrepQuery R (Identity Text) (Identity ProviderId)
    cql = "SELECT provider FROM provider_keys WHERE key = ?"

deleteKey :: (MonadClient m) => EmailKey -> m ()
deleteKey k = retry x5 $ write cql $ params LocalQuorum (Identity (emailKeyUniq k))
  where
    cql :: PrepQuery W (Identity Text) ()
    cql = "DELETE FROM provider_keys WHERE key = ?"

--------------------------------------------------------------------------------
