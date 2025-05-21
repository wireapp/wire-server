module Wire.UserStoreSpec (spec) where

import Data.Default
import Imports
import Polysemy.State
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Wire.API.User
import Wire.MiniBackend
import Wire.StoredUser
import Wire.UserStore

spec :: Spec
spec = do
  describe "mkUserFromStored" $ do
    prop "user identity" $ \domain defaultLocale storedUser ->
      let user = mkUserFromStored domain defaultLocale storedUser
       in if not storedUser.activated
            then user.userIdentity === Nothing
            else
              (emailIdentity =<< user.userIdentity) === storedUser.email
                .&&. (ssoIdentity =<< user.userIdentity) === storedUser.ssoId

    prop "user deleted" $ \domain defaultLocale storedUser ->
      let user = mkUserFromStored domain defaultLocale storedUser
       in userDeleted user === (storedUser.status == Just Deleted)

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

  describe "UserStore effect" $ do
    prop "user self email deleted" $ \user1 user2' email2 config ->
      let user2 = user2' {email = Just email2}
          localBackend = def {users = [user1, user2]}
          result =
            runNoFederationStack localBackend Nothing config $ do
              deleteEmail (user1.id)
              gets users
       in result === [user1 {email = Nothing}, user2]
    prop "update unvalidated email" $ \user1 user2 email1 config ->
      let updatedUser1 = user1 {emailUnvalidated = Just email1}
          localBackend = def {users = [user1, user2]}
          result =
            runNoFederationStack localBackend Nothing config $ do
              updateEmailUnvalidated (user1.id) email1
              gets users
       in result === [updatedUser1, user2]
