module Wire.UserSubsystem.InterpreterSpec where

import Data.LegalHold (UserLegalHoldStatus (UserLegalHoldDisabled))
import Imports
import Polysemy
import Polysemy.Input
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Wire.API.Team.Member
import Wire.API.Team.Permission
import Wire.API.User
import Wire.GalleyAPIAccess
import Wire.UserStore
import Wire.UserSubsystem.Interpreter

spec :: Spec
spec = describe "UserSubsystem.Interpreter" do
  describe "getUserProfileImpl" do
    prop "returns Nothing when the user doesn't exist" $
      \viewer targetUserId visibility domain locale -> do
        let retrievedProfile =
              run
                . runInputConst (UserSubsystemConfig visibility domain locale)
                . staticUserStoreInterpreter []
                . emptyGalleyAPIAccess
                $ getLocalUserProfileImpl viewer targetUserId
        retrievedProfile `shouldBe` Nothing

    prop "gets a local user profile when the user exists" $
      \viewerId viewerTeamId targetUser visibility domain locale -> do
        let mUser = mkUserFromStored domain locale NoPendingInvitations targetUser
            teamMember = mkTeamMember viewerId fullPermissions Nothing UserLegalHoldDisabled
        isJust mUser ==> do
          let retrievedProfile =
                run
                  . runInputConst (UserSubsystemConfig visibility domain locale)
                  . staticUserStoreInterpreter [targetUser]
                  . fakeGalleyAPIAccess teamMember
                  $ getLocalUserProfileImpl viewerId targetUser.id_
          retrievedProfile
            `shouldBe` Just
              ( mkUserProfile
                  (fmap (const (viewerTeamId, teamMember)) visibility)
                  (fromJust mUser)
                  UserLegalHoldDisabled
              )

staticUserStoreInterpreter :: [StoredUser] -> InterpreterFor UserStore r
staticUserStoreInterpreter allUsers = interpret $ \case
  GetUser uid -> pure $ find (\user -> user.id_ == uid) allUsers

emptyGalleyAPIAccess :: InterpreterFor GalleyAPIAccess r
emptyGalleyAPIAccess = interpret $ \case
  _ -> error "uninterpreted effect"

fakeGalleyAPIAccess :: TeamMember -> InterpreterFor GalleyAPIAccess r
fakeGalleyAPIAccess member = interpret $ \case
  GetTeamMember _ _ -> pure (Just member)
  _ -> error "uninterpreted effect"
