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
{-# OPTIONS_GHC -Wno-orphans #-}

module Brig.Sem.AccountStore.Cassandra
  ( accountStoreToCassandra,
  )
where

import Brig.App
import Brig.Data.Instances ()
import Brig.Options
import Brig.Password
import Brig.Sem.AccountStore
import Brig.Types.Intra
import qualified Brig.ZAuth as ZAuth
import Cassandra
import Control.Lens (view, (^.))
import Data.Handle
import Data.Id
import Data.Json.Util
import Data.Qualified
import Data.Range
import Data.Time.Clock
import Data.UUID.V4
import Imports
import Polysemy
import Wire.API.Provider.Service
import Wire.API.User

accountStoreToCassandra ::
  forall m r a.
  (MonadClient m, MonadReader Env m, Member (Embed m) r) =>
  -- (MonadClient m, Member (Embed m) r) =>
  Sem (AccountStore ': r) a ->
  Sem r a
accountStoreToCassandra =
  interpret $ \case
    NewAccount u inv tid mbHandle -> embed @m $ new u inv tid mbHandle
    InsertAccount account mbConv password activated ->
      embed @m $ insert account mbConv password activated
    LookupAuth uid -> embed @m $ lookupAuthQuery uid

-- | Preconditions:
--
-- 1. @newUserUUID u == Just inv || isNothing (newUserUUID u)@.
-- 2. If @isJust@, @mbHandle@ must be claimed by user with id @inv@.
--
-- Condition (2.) is essential for maintaining handle uniqueness.  It is guaranteed by the
-- fact that we're setting getting @mbHandle@ from table @"user"@, and when/if it was added
-- there, it was claimed properly.
new :: (MonadReader Env m, MonadIO m) => NewUser -> Maybe InvitationId -> Maybe TeamId -> Maybe Handle -> m (UserAccount, Maybe Password)
new u inv tid mbHandle = do
  defLoc <- setDefaultUserLocale <$> view settings
  domain <- viewFederationDomain
  uid <-
    Id <$> do
      case (inv, newUserUUID u) of
        (Just (toUUID -> uuid), _) -> pure uuid
        (_, Just uuid) -> pure uuid
        (Nothing, Nothing) -> liftIO nextRandom
  passwd <- maybe (return Nothing) (fmap Just . liftIO . mkSafePassword) pass
  expiry <- case status of
    Ephemeral -> do
      -- Ephemeral users' expiry time is in expires_in (default sessionTokenTimeout) seconds
      e <- view zauthEnv
      let ZAuth.SessionTokenTimeout defTTL = e ^. ZAuth.settings . ZAuth.sessionTokenTimeout
          ttl = maybe defTTL fromRange (newUserExpiresIn u)
      now <- liftIO =<< view currentTime
      return . Just . toUTCTimeMillis $ addUTCTime (fromIntegral ttl) now
    _ -> return Nothing
  return (UserAccount (user uid domain (locale defLoc) expiry) status, passwd)
  where
    ident = newUserIdentity u
    pass = newUserPassword u
    name = newUserDisplayName u
    pict = fromMaybe noPict (newUserPict u)
    assets = newUserAssets u
    status =
      if isNewUserEphemeral u
        then Ephemeral
        else Active
    colour = fromMaybe defaultAccentId (newUserAccentId u)
    locale defLoc = fromMaybe defLoc (newUserLocale u)
    managedBy = fromMaybe defaultManagedBy (newUserManagedBy u)
    user uid domain l e = User uid (Qualified uid domain) ident name pict assets colour False l Nothing mbHandle e tid managedBy

insert ::
  MonadClient m =>
  UserAccount ->
  -- | If a bot: conversation and team
  --   (if a team conversation)
  Maybe (ConvId, Maybe TeamId) ->
  Maybe Password ->
  -- | Whether the user is activated
  Bool ->
  m ()
insert (UserAccount u status) mbConv password activated = retry x5 . batch $ do
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

type Activated = Bool

type UserRowInsert =
  ( UserId,
    Name,
    Pict,
    [Asset],
    Maybe Email,
    Maybe Phone,
    Maybe UserSSOId,
    ColourId,
    Maybe Password,
    Activated,
    AccountStatus,
    Maybe UTCTimeMillis,
    Language,
    Maybe Country,
    Maybe ProviderId,
    Maybe ServiceId,
    Maybe Handle,
    Maybe TeamId,
    ManagedBy
  )

deriving instance Show UserRowInsert

lookupAuthQuery :: MonadClient m => UserId -> m (Maybe (Maybe Password, AccountStatus))
lookupAuthQuery u = fmap f <$> retry x1 (query1 authSelect (params LocalQuorum (Identity u)))
  where
    f (pw, st) = (pw, fromMaybe Active st)
    authSelect :: PrepQuery R (Identity UserId) (Maybe Password, Maybe AccountStatus)
    authSelect = "SELECT password, status FROM user WHERE id = ?"
