{-# LANGUAGE TemplateHaskell #-}

module Wire.UserStore where

import Data.Default
import Data.Id
import Imports
import Polysemy
import Wire.API.User
import Wire.Arbitrary
import Wire.StoredUser

-- this is similar to `UserUpdate` in `Wire.API.User`, but supports updates to
-- all profile fields rather than just four.
data UserProfileUpdate = MkUserProfileUpdate
  { name :: Maybe Name,
    pict :: Maybe Pict,
    assets :: Maybe [Asset],
    accentId :: Maybe ColourId,
    locale :: Maybe Locale
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
        locale = Nothing
      }

data UserStore m a where
  GetUser :: UserId -> UserStore m (Maybe StoredUser)
  UpdateUser :: UserId -> UserProfileUpdate -> UserStore m ()

makeSem ''UserStore
