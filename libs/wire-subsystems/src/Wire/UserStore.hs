{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.UserStore where

import Cassandra (GeneralPaginationState, PageWithState (..))
import Data.Default
import Data.Handle
import Data.Id
import Imports
import Polysemy
import Polysemy.Error
import Wire.API.Password
import Wire.API.Team.Feature (FeatureStatus)
import Wire.API.User
import Wire.API.User.RichInfo
import Wire.API.User.Search (SetSearchable)
import Wire.Arbitrary
import Wire.StoredUser
import Wire.UserStore.IndexUser

-- | Update of any "simple" attributes (ones that do not involve locking, like handle, or
-- validation protocols, like email).
--
-- | see 'UserProfileUpdate'.
data StoredUserUpdate = MkStoredUserUpdate
  { name :: Maybe Name,
    textStatus :: Maybe TextStatus,
    pict :: Maybe Pict,
    assets :: Maybe [Asset],
    accentId :: Maybe ColourId,
    locale :: Maybe Locale,
    supportedProtocols :: Maybe (Set BaseProtocolTag)
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via GenericUniform StoredUserUpdate

instance Default StoredUserUpdate where
  def = MkStoredUserUpdate Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | Update user handle (this involves several http requests for locking the required handle).
-- The old/previous handle (for deciding idempotency).
data StoredUserHandleUpdate = MkStoredUserHandleUpdate
  { old :: Maybe Handle,
    new :: Handle
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via GenericUniform StoredUserHandleUpdate

data StoredUserUpdateError = StoredUserUpdateHandleExists

-- | Effect containing database logic around 'StoredUser'.  (Example: claim handle lock is
-- database logic; validate handle is application logic.)
data UserStore m a where
  CreateUser :: NewStoredUser -> Maybe (ConvId, Maybe TeamId) -> UserStore m ()
  GetIndexUser :: UserId -> UserStore m (Maybe IndexUser)
  DoesUserExist :: UserId -> UserStore m Bool
  GetIndexUsersPaginated :: Int32 -> Maybe (GeneralPaginationState Void) -> UserStore m (PageWithState Void IndexUser)
  GetUsers :: [UserId] -> UserStore m [StoredUser]
  UpdateUser :: UserId -> StoredUserUpdate -> UserStore m ()
  UpdateEmail :: UserId -> EmailAddress -> UserStore m ()
  DeleteEmail :: UserId -> UserStore m ()
  UpdateEmailUnvalidated :: UserId -> EmailAddress -> UserStore m ()
  DeleteEmailUnvalidated :: UserId -> UserStore m ()
  UpdateUserHandleEither :: UserId -> StoredUserHandleUpdate -> UserStore m (Either StoredUserUpdateError ())
  UpdateSSOId :: UserId -> Maybe UserSSOId -> UserStore m Bool
  UpdateManagedBy :: UserId -> ManagedBy -> UserStore m ()
  UpdateAccountStatus :: UserId -> AccountStatus -> UserStore m ()
  ActivateUser :: UserId -> UserIdentity -> UserStore m ()
  DeactivateUser :: UserId -> UserStore m ()
  DeleteUser :: User -> UserStore m ()
  LookupName :: UserId -> UserStore m (Maybe Name)
  -- | This operation looks up a handle but is guaranteed to not give you stale locks.
  --   It is potentially slower and less resilient than 'GlimpseHandle'.
  LookupHandle :: Handle -> UserStore m (Maybe UserId)
  -- | The interpretation for 'LookupHandle' and 'GlimpseHandle'
  --   may differ in terms of how consistent they are.  If that
  --   matters for the interpretation, this operation may give you stale locks,
  --   but is faster and more resilient.
  GlimpseHandle :: Handle -> UserStore m (Maybe UserId)
  LookupStatus :: UserId -> UserStore m (Maybe AccountStatus)
  -- | Whether the account has been activated by verifying
  --   an email address or phone number.
  IsActivated :: UserId -> UserStore m Bool
  LookupLocale :: UserId -> UserStore m (Maybe (Maybe Language, Maybe Country))
  GetUserTeam :: UserId -> UserStore m (Maybe TeamId)
  -- GetUsersTeams :: [UserId] -> UserStore m (Maybe [TeamId])
  UpdateUserTeam :: UserId -> TeamId -> UserStore m ()
  GetRichInfo :: UserId -> UserStore m (Maybe RichInfoAssocList)
  LookupRichInfos :: [UserId] -> UserStore m [(UserId, RichInfo)]
  UpdateRichInfo :: UserId -> RichInfoAssocList -> UserStore m ()
  GetUserAuthenticationInfo :: UserId -> UserStore m (Maybe (Maybe Password, AccountStatus))
  SetUserSearchable :: UserId -> SetSearchable -> UserStore m ()
  UpdateFeatureConferenceCalling :: UserId -> Maybe FeatureStatus -> UserStore m ()
  LookupFeatureConferenceCalling :: UserId -> UserStore m (Maybe FeatureStatus)
  DeleteServiceUser :: ProviderId -> ServiceId -> BotId -> UserStore m ()
  LookupServiceUsers :: ProviderId -> ServiceId -> Maybe (GeneralPaginationState Void) -> UserStore m (PageWithState Void (BotId, ConvId, Maybe TeamId))
  LookupServiceUsersForTeam :: ProviderId -> ServiceId -> TeamId -> Maybe (GeneralPaginationState Void) -> UserStore m (PageWithState Void (BotId, ConvId))

makeSem ''UserStore

getUser :: (Member UserStore r) => UserId -> Sem r (Maybe StoredUser)
getUser uid = listToMaybe <$> getUsers [uid]

updateUserHandle ::
  (Member UserStore r, Member (Error StoredUserUpdateError) r) =>
  UserId ->
  StoredUserHandleUpdate ->
  Sem r ()
updateUserHandle uid update = either throw pure =<< updateUserHandleEither uid update
