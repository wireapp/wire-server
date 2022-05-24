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
{-# LANGUAGE RecordWildCards #-}

module Brig.Sem.UserQuery.Cassandra (userQueryToCassandra) where

import Brig.Data.Instances ()
import Brig.Password
import Brig.Sem.UserQuery
import Brig.Types
import Brig.Types.Intra
import Cassandra
import Control.Lens (view, (^.))
import Data.Id
import Imports
import Polysemy
import Wire.API.Provider.Service

userQueryToCassandra ::
  forall m r a.
  (MonadClient m, Member (Embed m) r) =>
  Sem (UserQuery ': r) a ->
  Sem r a
userQueryToCassandra =
  interpret $
    embed @m . \case
      GetId uid -> runIdentity <$$> retry x1 (query1 idSelect (params LocalQuorum (Identity uid)))
      GetUsers uids -> retry x1 (query usersSelect (params LocalQuorum (Identity uids)))
      GetName uid -> runIdentity <$$> retry x1 (query1 nameSelect (params LocalQuorum (Identity uid)))
      GetLocale uid -> retry x1 (query1 localeSelect (params LocalQuorum (Identity uid)))
      GetAuthentication uid -> retry x1 (query1 authSelect (params LocalQuorum (Identity uid)))
      GetPassword uid -> (runIdentity =<<) <$> retry x1 (query1 passwordSelect (params LocalQuorum (Identity uid)))
      GetActivated uid -> (== Just (Identity True)) <$> retry x1 (query1 activatedSelect (params LocalQuorum (Identity uid)))
      GetAccountStatus uid -> (runIdentity =<<) <$> retry x1 (query1 statusSelect (params LocalQuorum (Identity uid)))
      GetAccountStatuses uids -> retry x1 (query accountStateSelectAll (params LocalQuorum (Identity uids)))
      GetTeam uid -> (runIdentity =<<) <$> retry x1 (query1 teamSelect (params LocalQuorum (Identity uid)))
      GetAccounts uids -> retry x1 (query accountsSelect (params LocalQuorum (Identity uids)))
      InsertAccount ua mConvTeam mPass act -> accountInsert ua mConvTeam mPass act
      UpdateUser uid update -> userUpdate uid update

--------------------------------------------------------------------------------
-- Queries

idSelect :: PrepQuery R (Identity UserId) (Identity UserId)
idSelect = "SELECT id FROM user WHERE id = ?"

usersSelect :: PrepQuery R (Identity [UserId]) UserRow
usersSelect =
  "SELECT id, name, picture, email, phone, sso_id, accent_id, assets, \
  \activated, status, expires, language, country, provider, service, \
  \handle, team, managed_by \
  \FROM user where id IN ?"

nameSelect :: PrepQuery R (Identity UserId) (Identity Name)
nameSelect = "SELECT name FROM user WHERE id = ?"

localeSelect :: PrepQuery R (Identity UserId) (Maybe Language, Maybe Country)
localeSelect = "SELECT language, country FROM user WHERE id = ?"

authSelect :: PrepQuery R (Identity UserId) (Maybe Password, Maybe AccountStatus)
authSelect = "SELECT password, status FROM user WHERE id = ?"

passwordSelect :: PrepQuery R (Identity UserId) (Identity (Maybe Password))
passwordSelect = "SELECT password FROM user WHERE id = ?"

activatedSelect :: PrepQuery R (Identity UserId) (Identity Bool)
activatedSelect = "SELECT activated FROM user WHERE id = ?"

statusSelect :: PrepQuery R (Identity UserId) (Identity (Maybe AccountStatus))
statusSelect = "SELECT status FROM user WHERE id = ?"

accountStateSelectAll :: PrepQuery R (Identity [UserId]) (UserId, Bool, Maybe AccountStatus)
accountStateSelectAll = "SELECT id, activated, status FROM user WHERE id IN ?"

teamSelect :: PrepQuery R (Identity UserId) (Identity (Maybe TeamId))
teamSelect = "SELECT team FROM user WHERE id = ?"

accountsSelect :: PrepQuery R (Identity [UserId]) AccountRow
accountsSelect =
  "SELECT id, name, picture, email, phone, sso_id, accent_id, assets, \
  \activated, status, expires, language, country, provider, \
  \service, handle, team, managed_by \
  \FROM user WHERE id IN ?"

accountInsert ::
  MonadClient m =>
  UserAccount ->
  -- | If a bot: conversation and team
  --   (if a team conversation)
  Maybe (ConvId, Maybe TeamId) ->
  Maybe Password ->
  -- | Whether the user is activated
  Bool ->
  m ()
accountInsert (UserAccount u status) mbConv password activated = retry x5 . batch $ do
  setType BatchLogged
  setConsistency LocalQuorum
  let Locale l c = userLocale u
  addPrepQuery
    userInsert
    ( userId u,
      userDisplayName u,
      userPict u,
      userAssets u,
      userEmail u,
      userPhone u,
      userSSOId u,
      userAccentId u,
      password,
      activated,
      status,
      userExpire u,
      l,
      c,
      view serviceRefProvider <$> userService u,
      view serviceRefId <$> userService u,
      userHandle u,
      userTeam u,
      userManagedBy u
    )
  for_ ((,) <$> userService u <*> mbConv) $ \(sref, (cid, mbTid)) -> do
    let pid = sref ^. serviceRefProvider
        sid = sref ^. serviceRefId
    addPrepQuery cqlServiceUser (pid, sid, BotId (userId u), cid, mbTid)
    for_ mbTid $ \tid ->
      addPrepQuery cqlServiceTeam (pid, sid, BotId (userId u), cid, tid)
  where
    cqlServiceUser :: PrepQuery W (ProviderId, ServiceId, BotId, ConvId, Maybe TeamId) ()
    cqlServiceUser =
      "INSERT INTO service_user (provider, service, user, conv, team) \
      \VALUES (?, ?, ?, ?, ?)"
    cqlServiceTeam :: PrepQuery W (ProviderId, ServiceId, BotId, ConvId, TeamId) ()
    cqlServiceTeam =
      "INSERT INTO service_team (provider, service, user, conv, team) \
      \VALUES (?, ?, ?, ?, ?)"
    userInsert :: PrepQuery W UserRowInsert ()
    userInsert =
      "INSERT INTO user (id, name, picture, assets, email, phone, sso_id, \
      \accent_id, password, activated, status, expires, language, \
      \country, provider, service, handle, team, managed_by) \
      \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

userUpdate :: MonadClient m => UserId -> UserUpdate -> m ()
userUpdate u UserUpdate {..} = retry x5 . batch $ do
  setType BatchLogged
  setConsistency LocalQuorum
  for_ uupName $ \n -> addPrepQuery userDisplayNameUpdate (n, u)
  for_ uupPict $ \p -> addPrepQuery userPictUpdate (p, u)
  for_ uupAssets $ \a -> addPrepQuery userAssetsUpdate (a, u)
  for_ uupAccentId $ \c -> addPrepQuery userAccentIdUpdate (c, u)
  where
    userDisplayNameUpdate :: PrepQuery W (Name, UserId) ()
    userDisplayNameUpdate = "UPDATE user SET name = ? WHERE id = ?"

    userPictUpdate :: PrepQuery W (Pict, UserId) ()
    userPictUpdate = "UPDATE user SET picture = ? WHERE id = ?"

    userAssetsUpdate :: PrepQuery W ([Asset], UserId) ()
    userAssetsUpdate = "UPDATE user SET assets = ? WHERE id = ?"

    userAccentIdUpdate :: PrepQuery W (ColourId, UserId) ()
    userAccentIdUpdate = "UPDATE user SET accent_id = ? WHERE id = ?"
