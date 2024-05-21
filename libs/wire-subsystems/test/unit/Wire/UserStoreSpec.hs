module Wire.UserStoreSpec (spec) where

import Imports
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Wire.API.User
import Wire.StoredUser

spec :: Spec
spec = do
  describe "mkUserFromStored" $ do
    prop "user identity" $ \domain defaultLocale storedUser ->
      let user = mkUserFromStored domain defaultLocale storedUser
       in if not storedUser.activated
            then user.userIdentity === Nothing
            else
              (emailIdentity =<< user.userIdentity) === storedUser.email
                .&&. (phoneIdentity =<< user.userIdentity) === storedUser.phone
                .&&. (ssoIdentity =<< user.userIdentity) === storedUser.ssoId

    prop "user deleted" $ \domain defaultLocale storedUser ->
      let user = mkUserFromStored domain defaultLocale storedUser
       in user.userDeleted === (storedUser.status == Just Deleted)

    prop "user expires" $ \domain defaultLocale storedUser ->
      let user = mkUserFromStored domain defaultLocale storedUser
       in if storedUser.status == Just Ephemeral
            then user.userExpire === storedUser.expires
            else user.userExpire === Nothing

    prop "user locale" $ \domain defaultLocale storedUser ->
      let user = mkUserFromStored domain defaultLocale storedUser
       in if (isJust storedUser.language)
            then user.userLocale === Locale (fromJust storedUser.language) storedUser.country
            else user.userLocale === defaultLocale
