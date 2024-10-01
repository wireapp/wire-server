{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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
    isSamlUser,

    -- * Lookups
    lookupUser,
    lookupUsers,
    lookupName,
    lookupRichInfo,
    lookupRichInfoMultiUsers,
    lookupUserTeam,
    lookupServiceUsers,
    lookupServiceUsersForTeam,
    lookupFeatureConferenceCalling,
    userExists,

    -- * Updates
    updateEmail,
    updateEmailUnvalidated,
    updateSSOId,
    updateManagedBy,
    activateUser,
    deactivateUser,
    updateStatus,
    updateRichInfo,
    updateFeatureConferenceCalling,

    -- * Deletions
    deleteEmail,
    deleteEmailUnvalidated,
    deleteServiceUser,
  )
where

import Brig.App
import Brig.Options
import Brig.Types.Intra
import Brig.ZAuth qualified as ZAuth
import Cassandra hiding (Set)
import Control.Error
import Control.Lens hiding (from)
import Data.Conduit (ConduitM)
import Data.Domain
import Data.Handle (Handle)
import Data.HavePendingInvitations
import Data.Id
import Data.Json.Util (UTCTimeMillis, toUTCTimeMillis)
import Data.Misc
import Data.Qualified
import Data.Range (fromRange)
import Data.Time (addUTCTime)
import Data.UUID.V4
import Imports
import Polysemy
import Wire.API.Password
import Wire.API.Provider.Service
import Wire.API.Team.Feature
import Wire.API.User
import Wire.API.User.RichInfo
import Wire.PasswordStore

-- | Authentication errors.
data AuthError
  = AuthInvalidUser
  | AuthInvalidCredentials
  | AuthSuspended
  | AuthEphemeral
  | AuthPendingInvitation

-- | Re-authentication errors.
data ReAuthError
  = ReAuthError !AuthError
  | ReAuthMissingPassword
  | ReAuthCodeVerificationRequired
  | ReAuthCodeVerificationNoPendingCode
  | ReAuthCodeVerificationNoEmail

-- | Preconditions:
--
-- 1. @newUserUUID u == Just inv || isNothing (newUserUUID u)@.
-- 2. If @isJust@, @mbHandle@ must be claimed by user with id @inv@.
--
-- Condition (2.) is essential for maintaining handle uniqueness.  It is guaranteed by the
-- fact that we're setting getting @mbHandle@ from table @"user"@, and when/if it was added
-- there, it was claimed properly.
newAccount ::
  (MonadClient m, MonadReader Env m) =>
  NewUser ->
  Maybe InvitationId ->
  Maybe TeamId ->
  Maybe Handle ->
  m (User, Maybe Password)
newAccount u inv tid mbHandle = do
  defLoc <- defaultUserLocale <$> asks (.settings)
  domain <- viewFederationDomain
  uid <-
    Id <$> do
      case (inv, newUserUUID u) of
        (Just (toUUID -> uuid), _) -> pure uuid
        (_, Just uuid) -> pure uuid
        (Nothing, Nothing) -> liftIO nextRandom
  passwd <- maybe (pure Nothing) (fmap Just . liftIO . mkSafePasswordScrypt) pass
  expiry <- case status of
    Ephemeral -> do
      -- Ephemeral users' expiry time is in expires_in (default sessionTokenTimeout) seconds
      e <- asks (.zauthEnv)
      let ZAuth.SessionTokenTimeout defTTL = e ^. ZAuth.settings . ZAuth.sessionTokenTimeout
          ttl = maybe defTTL fromRange (newUserExpiresIn u)
      now <- liftIO =<< asks (.currentTime)
      pure . Just . toUTCTimeMillis $ addUTCTime (fromIntegral ttl) now
    _ -> pure Nothing
  pure (user uid domain (locale defLoc) expiry, passwd)
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
    prots = fromMaybe defSupportedProtocols (newUserSupportedProtocols u)
    user uid domain l e = User (Qualified uid domain) ident Nothing name Nothing pict assets colour status l Nothing mbHandle e tid managedBy prots

newAccountInviteViaScim :: (MonadReader Env m) => UserId -> Text -> TeamId -> Maybe Locale -> Name -> EmailAddress -> m User
newAccountInviteViaScim uid externalId tid locale name email = do
  defLoc <- defaultUserLocale <$> asks (.settings)
  let loc = fromMaybe defLoc locale
  domain <- viewFederationDomain
  pure $
    User
      (Qualified uid domain)
      (Just $ SSOIdentity (UserScimExternalId externalId) (Just email))
      Nothing
      name
      Nothing
      (Pict [])
      []
      defaultAccentId
      PendingInvitation
      loc
      Nothing
      Nothing
      Nothing
      (Just tid)
      ManagedByScim
      defSupportedProtocols

-- | Mandatory password authentication.
authenticate :: forall r. (Member PasswordStore r) => UserId -> PlainTextPassword6 -> ExceptT AuthError (AppT r) ()
authenticate u pw =
  lift (wrapHttp $ lookupAuth u) >>= \case
    Nothing -> throwE AuthInvalidUser
    Just (_, Deleted) -> throwE AuthInvalidUser
    Just (_, Suspended) -> throwE AuthSuspended
    Just (_, Ephemeral) -> throwE AuthEphemeral
    Just (_, PendingInvitation) -> throwE AuthPendingInvitation
    Just (Nothing, _) -> throwE AuthInvalidCredentials
    Just (Just pw', Active) ->
      case verifyPasswordWithStatus pw pw' of
        (False, _) -> throwE AuthInvalidCredentials
        (True, PasswordStatusNeedsUpdate) -> do
          -- FUTUREWORK(elland): 6char pwd allowed for now
          -- throwE AuthStalePassword in the future
          for_ (plainTextPassword8 . fromPlainTextPassword $ pw) (lift . hashAndUpdatePwd u)
        (True, _) -> pure ()
  where
    hashAndUpdatePwd :: UserId -> PlainTextPassword8 -> AppT r ()
    hashAndUpdatePwd uid pwd = do
      hashed <- mkSafePasswordScrypt pwd
      liftSem $ upsertHashedPassword uid hashed

-- | Password reauthentication. If the account has a password, reauthentication
-- is mandatory. If the account has no password, or is an SSO user, and no password is given,
-- reauthentication is a no-op.
reauthenticate ::
  ( MonadClient m,
    MonadReader Env m
  ) =>
  UserId ->
  Maybe PlainTextPassword6 ->
  ExceptT ReAuthError m ()
reauthenticate u pw =
  lift (lookupAuth u) >>= \case
    Nothing -> throwE (ReAuthError AuthInvalidUser)
    Just (_, Deleted) -> throwE (ReAuthError AuthInvalidUser)
    Just (_, Suspended) -> throwE (ReAuthError AuthSuspended)
    Just (_, PendingInvitation) -> throwE (ReAuthError AuthPendingInvitation)
    Just (Nothing, _) -> for_ pw $ const (throwE $ ReAuthError AuthInvalidCredentials)
    Just (Just pw', Active) -> maybeReAuth pw'
    Just (Just pw', Ephemeral) -> maybeReAuth pw'
  where
    maybeReAuth pw' = case pw of
      Nothing -> do
        musr <- lookupUser NoPendingInvitations u
        unless (maybe False isSamlUser musr) $ throwE ReAuthMissingPassword
      Just p ->
        unless (verifyPassword p pw') $
          throwE (ReAuthError AuthInvalidCredentials)

isSamlUser :: User -> Bool
isSamlUser usr = do
  case usr.userIdentity of
    Just (SSOIdentity (UserSSOId _) _) -> True
    _ -> False

insertAccount ::
  (MonadClient m) =>
  User ->
  -- | If a bot: conversation and team
  --   (if a team conversation)
  Maybe (ConvId, Maybe TeamId) ->
  Maybe Password ->
  -- | Whether the user is activated
  Bool ->
  m ()
insertAccount u mbConv password activated = retry x5 . batch $ do
  setType BatchLogged
  setConsistency LocalQuorum
  let Locale l c = userLocale u
  addPrepQuery
    userInsert
    ( userId u,
      userDisplayName u,
      userTextStatus u,
      userPict u,
      userAssets u,
      userEmail u,
      userSSOId u,
      userAccentId u,
      password,
      activated,
      userStatus u,
      userExpire u,
      l,
      c,
      view serviceRefProvider <$> userService u,
      view serviceRefId <$> userService u,
      userHandle u,
      userTeam u,
      userManagedBy u,
      userSupportedProtocols u
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

updateEmail :: (MonadClient m) => UserId -> EmailAddress -> m ()
updateEmail u e = retry x5 $ write userEmailUpdate (params LocalQuorum (e, u))

updateEmailUnvalidated :: (MonadClient m) => UserId -> EmailAddress -> m ()
updateEmailUnvalidated u e = retry x5 $ write userEmailUnvalidatedUpdate (params LocalQuorum (e, u))

updateSSOId :: (MonadClient m) => UserId -> Maybe UserSSOId -> m Bool
updateSSOId u ssoid = do
  mteamid <- lookupUserTeam u
  case mteamid of
    Just _ -> do
      retry x5 $ write userSSOIdUpdate (params LocalQuorum (ssoid, u))
      pure True
    Nothing -> pure False

updateManagedBy :: (MonadClient m) => UserId -> ManagedBy -> m ()
updateManagedBy u h = retry x5 $ write userManagedByUpdate (params LocalQuorum (h, u))

updateRichInfo :: (MonadClient m) => UserId -> RichInfoAssocList -> m ()
updateRichInfo u ri = retry x5 $ write userRichInfoUpdate (params LocalQuorum (ri, u))

updateFeatureConferenceCalling :: (MonadClient m) => UserId -> Maybe FeatureStatus -> m ()
updateFeatureConferenceCalling uid mStatus =
  retry x5 $ write update (params LocalQuorum (mStatus, uid))
  where
    update :: PrepQuery W (Maybe FeatureStatus, UserId) ()
    update = fromString "update user set feature_conference_calling = ? where id = ?"

deleteEmail :: (MonadClient m) => UserId -> m ()
deleteEmail u = retry x5 $ write userEmailDelete (params LocalQuorum (Identity u))

deleteEmailUnvalidated :: (MonadClient m) => UserId -> m ()
deleteEmailUnvalidated u = retry x5 $ write userEmailUnvalidatedDelete (params LocalQuorum (Identity u))

deleteServiceUser :: (MonadClient m) => ProviderId -> ServiceId -> BotId -> m ()
deleteServiceUser pid sid bid = do
  lookupServiceUser pid sid bid >>= \case
    Nothing -> pure ()
    Just (_, mbTid) -> retry x5 . batch $ do
      setType BatchLogged
      setConsistency LocalQuorum
      addPrepQuery cql (pid, sid, bid)
      for_ mbTid $ \tid ->
        addPrepQuery cqlTeam (pid, sid, tid, bid)
  where
    cql :: PrepQuery W (ProviderId, ServiceId, BotId) ()
    cql =
      "DELETE FROM service_user \
      \WHERE provider = ? AND service = ? AND user = ?"
    cqlTeam :: PrepQuery W (ProviderId, ServiceId, TeamId, BotId) ()
    cqlTeam =
      "DELETE FROM service_team \
      \WHERE provider = ? AND service = ? AND team = ? AND user = ?"

updateStatus :: (MonadClient m) => UserId -> AccountStatus -> m ()
updateStatus u s =
  retry x5 $ write userStatusUpdate (params LocalQuorum (s, u))

userExists :: (MonadClient m) => UserId -> m Bool
userExists uid = isJust <$> retry x1 (query1 idSelect (params LocalQuorum (Identity uid)))

lookupUser :: (MonadClient m, MonadReader Env m) => HavePendingInvitations -> UserId -> m (Maybe User)
lookupUser hpi u = listToMaybe <$> lookupUsers hpi [u]

activateUser :: (MonadClient m) => UserId -> UserIdentity -> m ()
activateUser u ident = do
  let email = emailIdentity ident
  retry x5 $ write userActivatedUpdate (params LocalQuorum (email, u))

deactivateUser :: (MonadClient m) => UserId -> m ()
deactivateUser u =
  retry x5 $ write userDeactivatedUpdate (params LocalQuorum (Identity u))

lookupName :: (MonadClient m) => UserId -> m (Maybe Name)
lookupName u =
  fmap runIdentity
    <$> retry x1 (query1 nameSelect (params LocalQuorum (Identity u)))

lookupRichInfo :: (MonadClient m) => UserId -> m (Maybe RichInfoAssocList)
lookupRichInfo u =
  fmap runIdentity
    <$> retry x1 (query1 richInfoSelect (params LocalQuorum (Identity u)))

-- | Returned rich infos are in the same order as users
lookupRichInfoMultiUsers :: (MonadClient m) => [UserId] -> m [(UserId, RichInfo)]
lookupRichInfoMultiUsers users = do
  mapMaybe (\(uid, mbRi) -> (uid,) . RichInfo <$> mbRi)
    <$> retry x1 (query richInfoSelectMulti (params LocalQuorum (Identity users)))

-- | Lookup user (no matter what status) and return 'TeamId'.  Safe to use for authorization:
-- suspended / deleted / ... users can't login, so no harm done if we authorize them *after*
-- successful login.
lookupUserTeam :: (MonadClient m) => UserId -> m (Maybe TeamId)
lookupUserTeam u =
  (runIdentity =<<)
    <$> retry x1 (query1 teamSelect (params LocalQuorum (Identity u)))

lookupAuth :: (MonadClient m) => UserId -> m (Maybe (Maybe Password, AccountStatus))
lookupAuth u = fmap f <$> retry x1 (query1 authSelect (params LocalQuorum (Identity u)))
  where
    f (pw, st) = (pw, fromMaybe Active st)

-- | Return users with given IDs.
--
-- Skips nonexistent users. /Does not/ skip users who have been deleted.
lookupUsers :: (MonadClient m, MonadReader Env m) => HavePendingInvitations -> [UserId] -> m [User]
lookupUsers hpi usrs = do
  loc <- defaultUserLocale <$> asks (.settings)
  domain <- viewFederationDomain
  toUsers domain loc hpi <$> retry x1 (query usersSelect (params LocalQuorum (Identity usrs)))

lookupServiceUser :: (MonadClient m) => ProviderId -> ServiceId -> BotId -> m (Maybe (ConvId, Maybe TeamId))
lookupServiceUser pid sid bid = retry x1 (query1 cql (params LocalQuorum (pid, sid, bid)))
  where
    cql :: PrepQuery R (ProviderId, ServiceId, BotId) (ConvId, Maybe TeamId)
    cql =
      "SELECT conv, team FROM service_user \
      \WHERE provider = ? AND service = ? AND user = ?"

-- | NB: might return a lot of users, and therefore we do streaming here (page-by-page).
lookupServiceUsers ::
  (MonadClient m) =>
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
  (MonadClient m) =>
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

lookupFeatureConferenceCalling :: (MonadClient m) => UserId -> m (Maybe FeatureStatus)
lookupFeatureConferenceCalling uid = do
  let q = query1 select (params LocalQuorum (Identity uid))
  (>>= runIdentity) <$> retry x1 q
  where
    select :: PrepQuery R (Identity UserId) (Identity (Maybe FeatureStatus))
    select = fromString "select feature_conference_calling from user where id = ?"

-------------------------------------------------------------------------------
-- Queries

type Activated = Bool

-- UserRow is the same as AccountRow from the user subsystem.  when migrating this code there,
-- consider eliminating it instead.
type UserRow =
  ( UserId,
    Name,
    Maybe TextStatus,
    Maybe Pict,
    Maybe EmailAddress,
    Maybe EmailAddress,
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
    Maybe ManagedBy,
    Maybe (Set BaseProtocolTag)
  )

type UserRowInsert =
  ( UserId,
    Name,
    Maybe TextStatus,
    Pict,
    [Asset],
    Maybe EmailAddress,
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
    ManagedBy,
    Set BaseProtocolTag
  )

deriving instance Show UserRowInsert

usersSelect :: PrepQuery R (Identity [UserId]) UserRow
usersSelect =
  "SELECT id, name, text_status, picture, email, email_unvalidated, sso_id, accent_id, assets, \
  \activated, status, expires, language, country, provider, service, \
  \handle, team, managed_by, supported_protocols \
  \FROM user where id IN ?"

idSelect :: PrepQuery R (Identity UserId) (Identity UserId)
idSelect = "SELECT id FROM user WHERE id = ?"

nameSelect :: PrepQuery R (Identity UserId) (Identity Name)
nameSelect = "SELECT name FROM user WHERE id = ?"

authSelect :: PrepQuery R (Identity UserId) (Maybe Password, Maybe AccountStatus)
authSelect = "SELECT password, status FROM user WHERE id = ?"

richInfoSelect :: PrepQuery R (Identity UserId) (Identity RichInfoAssocList)
richInfoSelect = "SELECT json FROM rich_info WHERE user = ?"

richInfoSelectMulti :: PrepQuery R (Identity [UserId]) (UserId, Maybe RichInfoAssocList)
richInfoSelectMulti = "SELECT user, json FROM rich_info WHERE user in ?"

teamSelect :: PrepQuery R (Identity UserId) (Identity (Maybe TeamId))
teamSelect = "SELECT team FROM user WHERE id = ?"

userInsert :: PrepQuery W UserRowInsert ()
userInsert =
  "INSERT INTO user (id, name, text_status, picture, assets, email, sso_id, \
  \accent_id, password, activated, status, expires, language, \
  \country, provider, service, handle, team, managed_by, supported_protocols) \
  \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

userEmailUpdate :: PrepQuery W (EmailAddress, UserId) ()
userEmailUpdate = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE user SET email = ? WHERE id = ?"

userEmailUnvalidatedUpdate :: PrepQuery W (EmailAddress, UserId) ()
userEmailUnvalidatedUpdate = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE user SET email_unvalidated = ? WHERE id = ?"

userEmailUnvalidatedDelete :: PrepQuery W (Identity UserId) ()
userEmailUnvalidatedDelete = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE user SET email_unvalidated = null WHERE id = ?"

userSSOIdUpdate :: PrepQuery W (Maybe UserSSOId, UserId) ()
userSSOIdUpdate = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE user SET sso_id = ? WHERE id = ?"

userManagedByUpdate :: PrepQuery W (ManagedBy, UserId) ()
userManagedByUpdate = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE user SET managed_by = ? WHERE id = ?"

userStatusUpdate :: PrepQuery W (AccountStatus, UserId) ()
userStatusUpdate = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE user SET status = ? WHERE id = ?"

userDeactivatedUpdate :: PrepQuery W (Identity UserId) ()
userDeactivatedUpdate = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE user SET activated = false WHERE id = ?"

userActivatedUpdate :: PrepQuery W (Maybe EmailAddress, UserId) ()
userActivatedUpdate = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE user SET activated = true, email = ? WHERE id = ?"

userEmailDelete :: PrepQuery W (Identity UserId) ()
userEmailDelete = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE user SET email = null, write_time_bumper = 0 WHERE id = ?"

userRichInfoUpdate :: PrepQuery W (RichInfoAssocList, UserId) ()
userRichInfoUpdate = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE rich_info SET json = ? WHERE user = ?"

-------------------------------------------------------------------------------
-- Conversions

toUsers :: Domain -> Locale -> HavePendingInvitations -> [UserRow] -> [User]
toUsers domain defLocale havePendingInvitations = fmap mk . filter fp
  where
    fp :: UserRow -> Bool
    fp =
      case havePendingInvitations of
        WithPendingInvitations -> const True
        NoPendingInvitations ->
          ( \( _uid,
               _name,
               _textStatus,
               _pict,
               _email,
               _emailUnvalidated,
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
               _managed_by,
               _prots
               ) -> status /= Just PendingInvitation
          )

    mk :: UserRow -> User
    mk
      ( uid,
        name,
        textStatus,
        pict,
        email,
        emailUnvalidated,
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
        managed_by,
        prots
        ) =
        let ident = toIdentity activated email ssoid
            expiration = if status == Just Ephemeral then expires else Nothing
            loc = toLocaleWithDefault defLocale (lan, con)
            svc = newServiceRef <$> sid <*> pid
         in User
              (Qualified uid domain)
              ident
              emailUnvalidated
              name
              textStatus
              (fromMaybe noPict pict)
              (fromMaybe [] assets)
              accent
              (fromMaybe Active status)
              loc
              svc
              handle
              expiration
              tid
              (fromMaybe ManagedByWire managed_by)
              (fromMaybe defSupportedProtocols prots)

    toLocaleWithDefault :: Locale -> (Maybe Language, Maybe Country) -> Locale
    toLocaleWithDefault _ (Just l, c) = Locale l c
    toLocaleWithDefault l _ = l

-- | Construct a 'UserIdentity'.
--
-- If the user is not activated, 'toIdentity' will return 'Nothing' as a precaution, because
-- elsewhere we rely on the fact that a non-empty 'UserIdentity' means that the user is
-- activated.
--
-- The reason it's just a "precaution" is that we /also/ have an invariant that having an
-- email in the database means the user has to be activated.
toIdentity ::
  -- | Whether the user is activated
  Bool ->
  Maybe EmailAddress ->
  Maybe UserSSOId ->
  Maybe UserIdentity
toIdentity True (Just e) Nothing = Just $! EmailIdentity e
toIdentity True email (Just ssoid) = Just $! SSOIdentity ssoid email
toIdentity True Nothing Nothing = Nothing
toIdentity False _ _ = Nothing
