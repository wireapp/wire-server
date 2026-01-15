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

module Brig.Data.User
  ( -- * Creation
    newStoredUser,
    newStoredUserViaScim,

    -- * Lookups
    lookupName,
    lookupRichInfoMultiUsers,
    lookupUserTeam,
    lookupFeatureConferenceCalling,
    userExists,

    -- * Updates
    activateUser,
    deactivateUser,
  )
where

import Brig.App
import Brig.Options
import Cassandra hiding (Set)
import Control.Error
import Control.Lens hiding (from)
import Data.Handle (Handle)
import Data.Id
import Data.Json.Util (toUTCTimeMillis)
import Data.Range (fromRange)
import Data.Time (addUTCTime)
import Data.UUID.V4
import Imports
import Wire.API.Password
import Wire.API.Team.Feature
import Wire.API.User
import Wire.API.User.RichInfo
import Wire.AuthenticationSubsystem.Config
import Wire.StoredUser

-- | Preconditions:
--
-- 1. @newUserUUID u == Just inv || isNothing (newUserUUID u)@.
-- 2. If @isJust@, @mbHandle@ must be claimed by user with id @inv@.
--
-- Condition (2.) is essential for maintaining handle uniqueness.  It is guaranteed by the
-- fact that we're setting getting @mbHandle@ from table @"user"@, and when/if it was added
-- there, it was claimed properly.
newStoredUser ::
  NewUser Password ->
  Maybe InvitationId ->
  Maybe TeamId ->
  Maybe Handle ->
  AppT r NewStoredUser
newStoredUser u inv tid mbHandle = do
  defLoc <- defaultUserLocale <$> asks (.settings)
  uid <-
    Id <$> do
      case (inv, newUserUUID u) of
        (Just (toUUID -> uuid), _) -> pure uuid
        (_, Just uuid) -> pure uuid
        (Nothing, Nothing) -> liftIO nextRandom
  expiry <- case status of
    Ephemeral -> do
      -- Ephemeral users' expiry time is in expires_in (default sessionTokenTimeout) seconds
      e <- asks (.zauthEnv)
      let SessionTokenTimeout defTTL = e.settings.sessionTokenTimeout
          ttl = maybe defTTL fromRange (newUserExpiresIn u)
      now <- liftIO =<< asks (.currentTime)
      pure . Just . toUTCTimeMillis $ addUTCTime (fromIntegral ttl) now
    _ -> pure Nothing
  pure (user uid (locale defLoc) expiry u.newUserPassword)
  where
    ident = newUserIdentity u
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
    user uid l e mPassword =
      NewStoredUser
        { id = uid,
          email = ident >>= emailIdentity,
          ssoId = ident >>= ssoIdentity,
          name,
          textStatus = Nothing,
          pict,
          assets,
          accentId = colour,
          password = mPassword,
          activated = False,
          status,
          language = l.lLanguage,
          country = l.lCountry,
          providerId = Nothing,
          serviceId = Nothing,
          handle = mbHandle,
          expires = e,
          teamId = tid,
          managedBy = managedBy,
          supportedProtocols = prots,
          searchable = True
        }

newStoredUserViaScim :: (MonadReader Env m) => UserId -> Text -> TeamId -> Maybe Locale -> Name -> EmailAddress -> m NewStoredUser
newStoredUserViaScim uid externalId tid locale name email = do
  defLoc <- defaultUserLocale <$> asks (.settings)
  let loc = fromMaybe defLoc locale
  pure $
    NewStoredUser
      { id = uid,
        email = Just email,
        ssoId = Just (UserScimExternalId externalId),
        name,
        textStatus = Nothing,
        pict = Pict [],
        assets = [],
        accentId = defaultAccentId,
        password = Nothing,
        -- treating 'PendingActivation' as 'Active', but then 'Brig.Data.User.toIdentity'
        -- would not produce an identity, and so we won't have the email address to construct
        -- the SCIM user.
        activated = True,
        status = PendingInvitation,
        language = loc.lLanguage,
        country = loc.lCountry,
        providerId = Nothing,
        serviceId = Nothing,
        handle = Nothing,
        expires = Nothing,
        teamId = Just tid,
        managedBy = ManagedByScim,
        supportedProtocols = defSupportedProtocols,
        searchable = True
      }

userExists :: (MonadClient m) => UserId -> m Bool
userExists uid = isJust <$> retry x1 (query1 idSelect (params LocalQuorum (Identity uid)))

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

lookupFeatureConferenceCalling :: (MonadClient m) => UserId -> m (Maybe FeatureStatus)
lookupFeatureConferenceCalling uid = do
  let q = query1 select (params LocalQuorum (Identity uid))
  (>>= runIdentity) <$> retry x1 q
  where
    select :: PrepQuery R (Identity UserId) (Identity (Maybe FeatureStatus))
    select = fromString "select feature_conference_calling from user where id = ?"

-------------------------------------------------------------------------------
-- Queries

idSelect :: PrepQuery R (Identity UserId) (Identity UserId)
idSelect = "SELECT id FROM user WHERE id = ?"

nameSelect :: PrepQuery R (Identity UserId) (Identity Name)
nameSelect = "SELECT name FROM user WHERE id = ?"

richInfoSelectMulti :: PrepQuery R (Identity [UserId]) (UserId, Maybe RichInfoAssocList)
richInfoSelectMulti = "SELECT user, json FROM rich_info WHERE user in ?"

teamSelect :: PrepQuery R (Identity UserId) (Identity (Maybe TeamId))
teamSelect = "SELECT team FROM user WHERE id = ?"

userDeactivatedUpdate :: PrepQuery W (Identity UserId) ()
userDeactivatedUpdate = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE user SET activated = false WHERE id = ?"

userActivatedUpdate :: PrepQuery W (Maybe EmailAddress, UserId) ()
userActivatedUpdate = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE user SET activated = true, email = ? WHERE id = ?"
