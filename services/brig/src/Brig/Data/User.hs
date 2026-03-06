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
  )
where

import Brig.App
import Brig.Options
import Control.Error
import Data.Handle (Handle)
import Data.Id
import Data.Json.Util (toUTCTimeMillis)
import Data.Range (fromRange)
import Data.Time (addUTCTime)
import Data.UUID.V4
import Imports
import Wire.API.Password
import Wire.API.User
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
          userType = UserTypeRegular,
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
        userType = UserTypeRegular,
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
