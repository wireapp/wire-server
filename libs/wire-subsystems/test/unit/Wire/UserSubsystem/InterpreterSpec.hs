module Wire.UserSubsystem.InterpreterSpec where

import Data.LegalHold (UserLegalHoldStatus (UserLegalHoldDisabled))
import Imports
import Polysemy
import Test.Hspec
import Test.QuickCheck
import Wire.API.User
import Wire.UserStore
import Wire.UserSubsystem.Interpreter

spec :: Spec
spec = describe "UserSubsystem.Interpreter" do
  describe "getUserProfileImpl" do
    it "returns Nothing when the user doesn't exist" $ do
      viewer <- generate arbitrary
      targetUserId <- generate arbitrary
      let retrievedProfile =
            run $
              staticUserStoreInterpreter [] $
                getUserProfileImpl viewer targetUserId
      retrievedProfile `shouldBe` Nothing

    it "gets a local user profile as a non-connected user" do
      viewer <- generate arbitrary
      targetUser <- generate arbitrary
      let retrievedProfile = run $ staticUserStoreInterpreter [targetUser] $ getUserProfileImpl viewer targetUser.userQualifiedId
      retrievedProfile `shouldBe` Just (publicProfile targetUser UserLegalHoldDisabled)

    it "gets a local user profile as a connected user" do
      viewer <- generate arbitrary
      targetUser <- generate arbitrary
      -- TODO: Mock the connection
      let retrievedProfile = run $ staticUserStoreInterpreter [targetUser] $ getUserProfileImpl viewer targetUser.userQualifiedId
      retrievedProfile `shouldBe` Just (connectedProfile targetUser UserLegalHoldDisabled)

staticUserStoreInterpreter :: [User] -> InterpreterFor UserStore r
staticUserStoreInterpreter allUsers = interpret $ \case
  GetUser uid -> pure $ find (\user -> userId user == uid) allUsers
