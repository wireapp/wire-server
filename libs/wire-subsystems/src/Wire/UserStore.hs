{-# LANGUAGE TemplateHaskell #-}

module Wire.UserStore where

import Cassandra (PageWithState (..), PagingState)
import Data.Default
import Data.Handle
import Data.Id
import Data.Time.Clock
import Imports
import Polysemy
import Polysemy.Error
import Wire.API.Password
import Wire.API.User
import Wire.API.User.RichInfo
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
  GetIndexUser :: UserId -> UserStore m (Maybe IndexUser)
  GetIndexUsersPaginated :: Int32 -> Maybe PagingState -> UserStore m (PageWithState IndexUser)
  GetUsers :: [UserId] -> UserStore m [StoredUser]
  UpdateUser :: UserId -> StoredUserUpdate -> UserStore m ()
  UpdateUserHandleEither :: UserId -> StoredUserHandleUpdate -> UserStore m (Either StoredUserUpdateError ())
  DeleteUser :: User -> UserStore m ()
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
  UpdateUserTeam :: UserId -> TeamId -> UserStore m ()
  GetActivityTimestamps :: UserId -> UserStore m [Maybe UTCTime]
  GetRichInfo :: UserId -> UserStore m (Maybe RichInfoAssocList)
  GetUserAuthenticationInfo :: UserId -> UserStore m (Maybe (Maybe Password, AccountStatus))

makeSem ''UserStore

getUser :: (Member UserStore r) => UserId -> Sem r (Maybe StoredUser)
getUser uid = listToMaybe <$> getUsers [uid]

updateUserHandle ::
  (Member UserStore r, Member (Error StoredUserUpdateError) r) =>
  UserId ->
  StoredUserHandleUpdate ->
  Sem r ()
updateUserHandle uid update = either throw pure =<< updateUserHandleEither uid update
