{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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

-- for Show UserRowInsert

-- TODO: Move to Brig.User.Account.DB
module Brig.Data.User
  ( AuthError (..),
    ReAuthError (..),
    newAccount,
    newAccountInviteViaScim,
    insertAccount,
    authenticate,
    reauthenticate,
    filterActive,
    isActivated,
    isSamlUser,

    -- * Lookups
    lookupAccount,
    lookupAccounts,
    lookupUser,
    lookupUsers,
    getName,
    lookupLocale,
    lookupPassword,
    lookupStatus,
    lookupRichInfo,
    lookupRichInfoMultiUsers,
    lookupUserTeam,
    lookupServiceUsers,
    lookupServiceUsersForTeam,
    lookupFeatureConferenceCalling,
    userExists,

    -- * Updates
    updateUser,
    updateEmail,
    updateEmailUnvalidated,
    updatePhone,
    updateSSOId,
    updateManagedBy,
    activateUser,
    deactivateUser,
    updateLocale,
    updatePassword,
    updateStatus,
    updateHandle,
    updateRichInfo,
    updateFeatureConferenceCalling,

    -- * Deletions
    deleteEmail,
    deleteEmailUnvalidated,
    deletePhone,
    deleteServiceUser,
  )
where

import Brig.App (Env, currentTime, settings, viewFederationDomain, zauthEnv)
import Brig.Data.Instances ()
import Brig.Effects.UserQuery
  ( AuthError (..),
    ReAuthError (..),
    UserQuery,
    activateUser,
    deleteEmailUnvalidated,
    deleteServiceUser,
    getAuthentication,
    getId,
    getLocale,
    getName,
    getUsers,
    insertAccount,
    isActivated,
    lookupAccount,
    lookupAccounts,
    updateEmail,
    updateHandle,
    updatePhone,
    updateStatus,
  )
import Brig.Options
import Brig.Password
import Brig.Types.Intra
import Brig.Types.User (HavePendingInvitations (NoPendingInvitations, WithPendingInvitations))
import qualified Brig.ZAuth as ZAuth
import Cassandra
import Control.Error
import Control.Lens hiding (from)
import Data.Conduit (ConduitM)
import Data.Domain
import Data.Handle (Handle)
import Data.Id
import Data.Json.Util (UTCTimeMillis, toUTCTimeMillis)
import Data.Misc (PlainTextPassword (..))
import Data.Qualified
import Data.Range (fromRange)
import Data.Time (addUTCTime)
import Data.UUID.V4
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Wire.API.Provider.Service
import qualified Wire.API.Team.Feature as ApiFt
import Wire.API.User
import Wire.API.User.RichInfo

-- | Preconditions:
--
-- 1. @newUserUUID u == Just inv || isNothing (newUserUUID u)@.
-- 2. If @isJust@, @mbHandle@ must be claimed by user with id @inv@.
--
-- Condition (2.) is essential for maintaining handle uniqueness.  It is guaranteed by the
-- fact that we're setting getting @mbHandle@ from table @"user"@, and when/if it was added
-- there, it was claimed properly.
newAccount :: (MonadClient m, MonadReader Env m) => NewUser -> Maybe InvitationId -> Maybe TeamId -> Maybe Handle -> m (UserAccount, Maybe Password)
newAccount u inv tid mbHandle = do
  defLoc <- setDefaultUserLocale <$> view settings
  domain <- viewFederationDomain
  uid <-
    Id <$> do
      case (inv, newUserUUID u) of
        (Just (toUUID -> uuid), _) -> pure uuid
        (_, Just uuid) -> pure uuid
        (Nothing, Nothing) -> liftIO nextRandom
  passwd <- maybe (pure Nothing) (fmap Just . liftIO . mkSafePassword) pass
  expiry <- case status of
    Ephemeral -> do
      -- Ephemeral users' expiry time is in expires_in (default sessionTokenTimeout) seconds
      e <- view zauthEnv
      let ZAuth.SessionTokenTimeout defTTL = e ^. ZAuth.settings . ZAuth.sessionTokenTimeout
          ttl = maybe defTTL fromRange (newUserExpiresIn u)
      now <- liftIO =<< view currentTime
      pure . Just . toUTCTimeMillis $ addUTCTime (fromIntegral ttl) now
    _ -> pure Nothing
  pure (UserAccount (user uid domain (locale defLoc) expiry) status, passwd)
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

newAccountInviteViaScim :: (MonadClient m, MonadReader Env m) => UserId -> TeamId -> Maybe Locale -> Name -> Email -> m UserAccount
newAccountInviteViaScim uid tid locale name email = do
  defLoc <- setDefaultUserLocale <$> view settings
  let loc = fromMaybe defLoc locale
  domain <- viewFederationDomain
  pure (UserAccount (user domain loc) PendingInvitation)
  where
    user domain loc =
      User
        uid
        (Qualified uid domain)
        (Just $ EmailIdentity email)
        name
        (Pict [])
        []
        defaultAccentId
        False
        loc
        Nothing
        Nothing
        Nothing
        (Just tid)
        ManagedByScim

-- | Mandatory password authentication.
authenticate ::
  Members
    '[ Error AuthError,
       UserQuery
     ]
    r =>
  UserId ->
  PlainTextPassword ->
  Sem r ()
authenticate u pw =
  lookupAuth u >>= \case
    Nothing -> throw AuthInvalidUser
    Just (_, Deleted) -> throw AuthInvalidUser
    Just (_, Suspended) -> throw AuthSuspended
    Just (_, Ephemeral) -> throw AuthEphemeral
    Just (_, PendingInvitation) -> throw AuthPendingInvitation
    Just (Nothing, _) -> throw AuthInvalidCredentials
    Just (Just pw', Active) ->
      unless (verifyPassword pw pw') $
        throw AuthInvalidCredentials

-- | Password reauthentication. If the account has a password, reauthentication
-- is mandatory. If the account has no password, or is an SSO user, and no password is given,
-- reauthentication is a no-op.
reauthenticate ::
  Members
    '[ Error ReAuthError,
       Input (Local ()),
       UserQuery
     ]
    r =>
  Locale ->
  UserId ->
  Maybe PlainTextPassword ->
  Sem r ()
reauthenticate locale u pw =
  lookupAuth u >>= \case
    Nothing -> throw (ReAuthError AuthInvalidUser)
    Just (_, Deleted) -> throw (ReAuthError AuthInvalidUser)
    Just (_, Suspended) -> throw (ReAuthError AuthSuspended)
    Just (_, PendingInvitation) -> throw (ReAuthError AuthPendingInvitation)
    Just (Nothing, _) -> for_ pw $ const (throw $ ReAuthError AuthInvalidCredentials)
    Just (Just pw', Active) -> maybeReAuth pw'
    Just (Just pw', Ephemeral) -> maybeReAuth pw'
  where
    maybeReAuth pw' = case pw of
      Nothing -> unlessM (isSamlUser locale u) $ throw ReAuthMissingPassword
      Just p ->
        unless (verifyPassword p pw') $
          throw (ReAuthError AuthInvalidCredentials)

isSamlUser ::
  Members '[Input (Local ()), UserQuery] r =>
  Locale ->
  UserId ->
  Sem r Bool
isSamlUser locale uid = do
  account <- lookupAccount locale uid
  case userIdentity . accountUser =<< account of
    Just (SSOIdentity (UserSSOId _) _ _) -> pure True
    _ -> pure False

updateLocale :: MonadClient m => UserId -> Locale -> m ()
updateLocale u (Locale l c) = write userLocaleUpdate (params LocalQuorum (l, c, u))

updateUser :: MonadClient m => UserId -> UserUpdate -> m ()
updateUser u UserUpdate {..} = retry x5 . batch $ do
  setType BatchLogged
  setConsistency LocalQuorum
  for_ uupName $ \n -> addPrepQuery userDisplayNameUpdate (n, u)
  for_ uupPict $ \p -> addPrepQuery userPictUpdate (p, u)
  for_ uupAssets $ \a -> addPrepQuery userAssetsUpdate (a, u)
  for_ uupAccentId $ \c -> addPrepQuery userAccentIdUpdate (c, u)

updateEmailUnvalidated :: MonadClient m => UserId -> Email -> m ()
updateEmailUnvalidated u e = retry x5 $ write userEmailUnvalidatedUpdate (params LocalQuorum (e, u))

updateSSOId :: MonadClient m => UserId -> Maybe UserSSOId -> m Bool
updateSSOId u ssoid = do
  mteamid <- lookupUserTeam u
  case mteamid of
    Just _ -> do
      retry x5 $ write userSSOIdUpdate (params LocalQuorum (ssoid, u))
      pure True
    Nothing -> pure False

updateManagedBy :: MonadClient m => UserId -> ManagedBy -> m ()
updateManagedBy u h = retry x5 $ write userManagedByUpdate (params LocalQuorum (h, u))

-- updateHandle :: MonadClient m => UserId -> Handle -> m ()
-- updateHandle u h = retry x5 $ write userHandleUpdate (params LocalQuorum (h, u))

updatePassword :: MonadClient m => UserId -> PlainTextPassword -> m ()
updatePassword u t = do
  p <- liftIO $ mkSafePassword t
  retry x5 $ write userPasswordUpdate (params LocalQuorum (p, u))

updateRichInfo :: MonadClient m => UserId -> RichInfoAssocList -> m ()
updateRichInfo u ri = retry x5 $ write userRichInfoUpdate (params LocalQuorum (ri, u))

updateFeatureConferenceCalling :: MonadClient m => UserId -> Maybe (ApiFt.WithStatusNoLock ApiFt.ConferenceCallingConfig) -> m (Maybe (ApiFt.WithStatusNoLock ApiFt.ConferenceCallingConfig))
updateFeatureConferenceCalling uid mbStatus = do
  let flag = ApiFt.wssStatus <$> mbStatus
  retry x5 $ write update (params LocalQuorum (flag, uid))
  pure mbStatus
  where
    update :: PrepQuery W (Maybe ApiFt.FeatureStatus, UserId) ()
    update = fromString "update user set feature_conference_calling = ? where id = ?"

deleteEmail :: MonadClient m => UserId -> m ()
deleteEmail u = retry x5 $ write userEmailDelete (params LocalQuorum (Identity u))

deletePhone :: MonadClient m => UserId -> m ()
deletePhone u = retry x5 $ write userPhoneDelete (params LocalQuorum (Identity u))

userExists :: Member UserQuery r => UserId -> Sem r Bool
userExists uid = isJust <$> getId uid

filterActive :: MonadClient m => [UserId] -> m [UserId]
filterActive us =
  map (view _1) . filter isActiveUser
    <$> retry x1 (query accountStateSelectAll (params LocalQuorum (Identity us)))
  where
    isActiveUser :: (UserId, Bool, Maybe AccountStatus) -> Bool
    isActiveUser (_, True, Just Active) = True
    isActiveUser _ = False

lookupUser ::
  Member UserQuery r =>
  Local x ->
  Locale ->
  HavePendingInvitations ->
  UserId ->
  Sem r (Maybe User)
lookupUser loc locale hpi u = listToMaybe <$> lookupUsers loc locale hpi [u]

deactivateUser :: MonadClient m => UserId -> m ()
deactivateUser u =
  retry x5 $ write userDeactivatedUpdate (params LocalQuorum (Identity u))

lookupLocale ::
  Member UserQuery r =>
  Locale ->
  UserId ->
  Sem r (Maybe Locale)
lookupLocale defLoc u = fmap (toLocale defLoc) <$> getLocale u

lookupPassword :: MonadClient m => UserId -> m (Maybe Password)
lookupPassword u =
  (runIdentity =<<)
    <$> retry x1 (query1 passwordSelect (params LocalQuorum (Identity u)))

lookupStatus :: MonadClient m => UserId -> m (Maybe AccountStatus)
lookupStatus u =
  (runIdentity =<<)
    <$> retry x1 (query1 statusSelect (params LocalQuorum (Identity u)))

lookupRichInfo :: MonadClient m => UserId -> m (Maybe RichInfoAssocList)
lookupRichInfo u =
  fmap runIdentity
    <$> retry x1 (query1 richInfoSelect (params LocalQuorum (Identity u)))

-- | Returned rich infos are in the same order as users
lookupRichInfoMultiUsers :: MonadClient m => [UserId] -> m [(UserId, RichInfo)]
lookupRichInfoMultiUsers users = do
  mapMaybe (\(uid, mbRi) -> (uid,) . RichInfo <$> mbRi)
    <$> retry x1 (query richInfoSelectMulti (params LocalQuorum (Identity users)))

-- | Lookup user (no matter what status) and return 'TeamId'.  Safe to use for authorization:
-- suspended / deleted / ... users can't login, so no harm done if we authorize them *after*
-- successful login.
lookupUserTeam :: MonadClient m => UserId -> m (Maybe TeamId)
lookupUserTeam u =
  (runIdentity =<<)
    <$> retry x1 (query1 teamSelect (params LocalQuorum (Identity u)))

lookupAuth ::
  Member UserQuery r =>
  UserId ->
  Sem r (Maybe (Maybe Password, AccountStatus))
lookupAuth u = fmap f <$> getAuthentication u
  where
    f (pw, st) = (pw, fromMaybe Active st)

-- | Return users with given IDs.
--
-- Skips nonexistent users. /Does not/ skip users who have been deleted.
lookupUsers ::
  Member UserQuery r =>
  Local x ->
  Locale ->
  HavePendingInvitations ->
  [UserId] ->
  Sem r [User]
lookupUsers loc locale hpi usrs =
  toUsers (tDomain loc) locale hpi <$> getUsers usrs

-- | NB: might return a lot of users, and therefore we do streaming here (page-by-page).
lookupServiceUsers ::
  MonadClient m =>
  ProviderId ->
  ServiceId ->
  ConduitM () [(BotId, ConvId, Maybe TeamId)] m ()
lookupServiceUsers pid sid =
  paginateC cql (paramsP LocalQuorum (pid, sid) 100) x1
  where
    cql :: PrepQuery R (ProviderId, ServiceId) (BotId, ConvId, Maybe TeamId)
    cql =
      "SELECT user, conv, team FROM service_user \
      \WHERE provider = ? AND service = ?"

lookupServiceUsersForTeam ::
  MonadClient m =>
  ProviderId ->
  ServiceId ->
  TeamId ->
  ConduitM () [(BotId, ConvId)] m ()
lookupServiceUsersForTeam pid sid tid =
  paginateC cql (paramsP LocalQuorum (pid, sid, tid) 100) x1
  where
    cql :: PrepQuery R (ProviderId, ServiceId, TeamId) (BotId, ConvId)
    cql =
      "SELECT user, conv FROM service_team \
      \WHERE provider = ? AND service = ? AND team = ?"

lookupFeatureConferenceCalling :: MonadClient m => UserId -> m (Maybe (ApiFt.WithStatusNoLock ApiFt.ConferenceCallingConfig))
lookupFeatureConferenceCalling uid = do
  let q = query1 select (params LocalQuorum (Identity uid))
  mStatusValue <- (>>= runIdentity) <$> retry x1 q
  case mStatusValue of
    Nothing -> pure Nothing
    Just status -> pure $ Just $ ApiFt.defFeatureStatusNoLock {ApiFt.wssStatus = status}
  where
    select :: PrepQuery R (Identity UserId) (Identity (Maybe ApiFt.FeatureStatus))
    select = fromString "select feature_conference_calling from user where id = ?"

-------------------------------------------------------------------------------
-- Queries

type Activated = Bool

type UserRow =
  ( UserId,
    Name,
    Maybe Pict,
    Maybe Email,
    Maybe Phone,
    Maybe UserSSOId,
    ColourId,
    Maybe [Asset],
    Activated,
    Maybe AccountStatus,
    Maybe UTCTimeMillis,
    Maybe Language,
    Maybe Country,
    Maybe ProviderId,
    Maybe ServiceId,
    Maybe Handle,
    Maybe TeamId,
    Maybe ManagedBy
  )

passwordSelect :: PrepQuery R (Identity UserId) (Identity (Maybe Password))
passwordSelect = "SELECT password FROM user WHERE id = ?"

accountStateSelectAll :: PrepQuery R (Identity [UserId]) (UserId, Bool, Maybe AccountStatus)
accountStateSelectAll = "SELECT id, activated, status FROM user WHERE id IN ?"

statusSelect :: PrepQuery R (Identity UserId) (Identity (Maybe AccountStatus))
statusSelect = "SELECT status FROM user WHERE id = ?"

richInfoSelect :: PrepQuery R (Identity UserId) (Identity RichInfoAssocList)
richInfoSelect = "SELECT json FROM rich_info WHERE user = ?"

richInfoSelectMulti :: PrepQuery R (Identity [UserId]) (UserId, Maybe RichInfoAssocList)
richInfoSelectMulti = "SELECT user, json FROM rich_info WHERE user in ?"

teamSelect :: PrepQuery R (Identity UserId) (Identity (Maybe TeamId))
teamSelect = "SELECT team FROM user WHERE id = ?"

-- userInsert :: PrepQuery W UserRowInsert ()
-- userInsert =
--   "INSERT INTO user (id, name, picture, assets, email, phone, sso_id, \
--   \accent_id, password, activated, status, expires, language, \
--   \country, provider, service, handle, team, managed_by) \
--   \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

userDisplayNameUpdate :: PrepQuery W (Name, UserId) ()
userDisplayNameUpdate = "UPDATE user SET name = ? WHERE id = ?"

userPictUpdate :: PrepQuery W (Pict, UserId) ()
userPictUpdate = "UPDATE user SET picture = ? WHERE id = ?"

userAssetsUpdate :: PrepQuery W ([Asset], UserId) ()
userAssetsUpdate = "UPDATE user SET assets = ? WHERE id = ?"

userAccentIdUpdate :: PrepQuery W (ColourId, UserId) ()
userAccentIdUpdate = "UPDATE user SET accent_id = ? WHERE id = ?"

userEmailUnvalidatedUpdate :: PrepQuery W (Email, UserId) ()
userEmailUnvalidatedUpdate = "UPDATE user SET email_unvalidated = ? WHERE id = ?"

userSSOIdUpdate :: PrepQuery W (Maybe UserSSOId, UserId) ()
userSSOIdUpdate = "UPDATE user SET sso_id = ? WHERE id = ?"

userManagedByUpdate :: PrepQuery W (ManagedBy, UserId) ()
userManagedByUpdate = "UPDATE user SET managed_by = ? WHERE id = ?"

userPasswordUpdate :: PrepQuery W (Password, UserId) ()
userPasswordUpdate = "UPDATE user SET password = ? WHERE id = ?"

userDeactivatedUpdate :: PrepQuery W (Identity UserId) ()
userDeactivatedUpdate = "UPDATE user SET activated = false WHERE id = ?"

userLocaleUpdate :: PrepQuery W (Language, Maybe Country, UserId) ()
userLocaleUpdate = "UPDATE user SET language = ?, country = ? WHERE id = ?"

userEmailDelete :: PrepQuery W (Identity UserId) ()
userEmailDelete = "UPDATE user SET email = null WHERE id = ?"

userPhoneDelete :: PrepQuery W (Identity UserId) ()
userPhoneDelete = "UPDATE user SET phone = null WHERE id = ?"

userRichInfoUpdate :: PrepQuery W (RichInfoAssocList, UserId) ()
userRichInfoUpdate = "UPDATE rich_info SET json = ? WHERE user = ?"

-------------------------------------------------------------------------------
-- Conversions

toUsers :: Domain -> Locale -> HavePendingInvitations -> [UserRow] -> [User]
toUsers domain defaultLocale havePendingInvitations = fmap mk . filter fp
  where
    fp :: UserRow -> Bool
    fp =
      case havePendingInvitations of
        WithPendingInvitations -> const True
        NoPendingInvitations ->
          ( \( _uid,
               _name,
               _pict,
               _email,
               _phone,
               _ssoid,
               _accent,
               _assets,
               _activated,
               status,
               _expires,
               _lan,
               _con,
               _pid,
               _sid,
               _handle,
               _tid,
               _managed_by
               ) -> status /= Just PendingInvitation
          )

    mk :: UserRow -> User
    mk
      ( uid,
        name,
        pict,
        email,
        phone,
        ssoid,
        accent,
        assets,
        activated,
        status,
        expires,
        lan,
        con,
        pid,
        sid,
        handle,
        tid,
        managed_by
        ) =
        let ident = toIdentity activated email phone ssoid
            deleted = Just Deleted == status
            expiration = if status == Just Ephemeral then expires else Nothing
            loc = toLocale defaultLocale (lan, con)
            svc = newServiceRef <$> sid <*> pid
         in User
              uid
              (Qualified uid domain)
              ident
              name
              (fromMaybe noPict pict)
              (fromMaybe [] assets)
              accent
              deleted
              loc
              svc
              handle
              expiration
              tid
              (fromMaybe ManagedByWire managed_by)

toLocale :: Locale -> (Maybe Language, Maybe Country) -> Locale
toLocale _ (Just l, c) = Locale l c
toLocale l _ = l

-- | Construct a 'UserIdentity'.
--
-- If the user is not activated, 'toIdentity' will return 'Nothing' as a precaution, because
-- elsewhere we rely on the fact that a non-empty 'UserIdentity' means that the user is
-- activated.
--
-- The reason it's just a "precaution" is that we /also/ have an invariant that having an
-- email or phone in the database means the user has to be activated.
toIdentity ::
  -- | Whether the user is activated
  Bool ->
  Maybe Email ->
  Maybe Phone ->
  Maybe UserSSOId ->
  Maybe UserIdentity
toIdentity True (Just e) (Just p) Nothing = Just $! FullIdentity e p
toIdentity True (Just e) Nothing Nothing = Just $! EmailIdentity e
toIdentity True Nothing (Just p) Nothing = Just $! PhoneIdentity p
toIdentity True email phone (Just ssoid) = Just $! SSOIdentity ssoid email phone
toIdentity True Nothing Nothing Nothing = Nothing
toIdentity False _ _ _ = Nothing
