{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Wire.AuthenticationSubsystem.InterpreterSpec (spec) where

import Data.Default
import Data.Map qualified as Map
import Data.Misc
import Imports
import Test.Hspec
import Test.QuickCheck
import Wire.API.Password
import Wire.API.User
import Wire.API.User.Password
import Wire.AuthenticationSubsystem
import Wire.MiniBackend
import Wire.StoredUser
import Wire.UserKeyStore
import Wire.UserSubsystem.Interpreter

spec :: Spec
spec = describe "AuthenticationSubsystem.Interpreter" do
  describe "password reset" do
    it "happy path" do
      config :: UserSubsystemConfig <- generate arbitrary
      email <- generate arbitrary
      NotPendingStoredUser userNoEmail <- generate arbitrary
      let user = userNoEmail {email = Just email, status = Just Active}
      let localBackend =
            def
              { users = [user],
                userKeys = Map.singleton (userEmailKey email) user.id
              }
          password = plainTextPassword8Unsafe "newsecret"
          (actualState, ()) =
            runNoFederationStackState localBackend Nothing config do
              (_, (_, code)) <- createPasswordResetCode (userEmailKey email)
              resetPassword (PasswordResetEmailIdentity email) code password
          Just actualPassword = Map.lookup user.id actualState.userPassword
      verifyPassword password actualPassword `shouldBe` True

    it "wrong password on init" do
      pending

    it "no code on complete" do
      pending

    it "wrong code on complete" do
      pending

    it "too many retries" do
      pending

    it "expired code" do
      pending

    it "wrong user" do
      pending
