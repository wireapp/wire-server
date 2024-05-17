{-# LANGUAGE TemplateHaskell #-}

module Wire.UserStore where

import Data.Default
import Data.Handle
import Data.Id
import Imports
import Polysemy
import Wire.API.User
import Wire.Arbitrary
import Wire.StoredUser

data AllowSCIMUpdates
  = AllowSCIMUpdates
  | ForbidSCIMUpdates
  deriving (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericUniform AllowSCIMUpdates

-- | Wrapper around an updated field which can potentially be managed by SCIM.
data ScimUpdate a = MkScimUpdate
  { -- | whether changes to SCIM-managed users should be allowed
    allowScim :: AllowSCIMUpdates,
    value :: a
  }
  deriving stock (Eq, Ord, Show)
  deriving (Functor, Foldable, Traversable)

forbidScimUpdate :: a -> ScimUpdate a
forbidScimUpdate = MkScimUpdate ForbidSCIMUpdates

allowScimUpdate :: a -> ScimUpdate a
allowScimUpdate = MkScimUpdate AllowSCIMUpdates

instance Arbitrary a => Arbitrary (ScimUpdate a) where
  arbitrary = MkScimUpdate <$> arbitrary <*> arbitrary

-- this is similar to `UserUpdate` in `Wire.API.User`, but supports updates to
-- all profile fields rather than just four.
data UserProfileUpdate = MkUserProfileUpdate
  { name :: Maybe (ScimUpdate Name),
    pict :: Maybe Pict,
    assets :: Maybe [Asset],
    accentId :: Maybe ColourId,
    locale :: Maybe Locale,
    handle :: Maybe (ScimUpdate Handle)
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via GenericUniform UserProfileUpdate

instance Default UserProfileUpdate where
  def =
    MkUserProfileUpdate
      { name = Nothing,
        pict = Nothing,
        assets = Nothing,
        accentId = Nothing,
        locale = Nothing,
        handle = Nothing
      }

data UserStore m a where
  GetUser :: UserId -> UserStore m (Maybe StoredUser)
  UpdateUser :: UserId -> UserProfileUpdate -> UserStore m ()
  -- | Claim a new handle for an existing 'User'.
  ClaimHandle :: UserId -> Maybe Handle -> Handle -> UserStore m Bool
  -- | Free a 'Handle', making it available to be claimed again.
  FreeHandle :: UserId -> Handle -> UserStore m ()
  -- | this operation looks up a handle but may not give you stale data
  --   it is potentially slower and less resilient than 'GlimpseHandle'
  LookupHandle :: Handle -> UserStore m (Maybe UserId)
  -- | the interpretation for 'LookupHandle' and 'GlimpseHandle'
  --   may differ in terms of how consistent they are, if that
  --   matters for the interpretation, this operation may give you stale data
  --   but is faster and more resilient
  GlimpseHandle :: Handle -> UserStore m (Maybe UserId)

makeSem ''UserStore
