{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Wire.AuthenticationSubsystem.InterpreterSpec (spec) where

import Data.Domain
import Data.Id
import Data.Qualified
import Data.Time
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.State
import Polysemy.TinyLog
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Wire.API.Allowlists (AllowlistEmailDomains (AllowlistEmailDomains), AllowlistPhonePrefixes)
import Wire.API.Password
import Wire.API.User
import Wire.API.User qualified as User
import Wire.API.User.Auth
import Wire.API.User.Password
import Wire.AuthenticationSubsystem
import Wire.AuthenticationSubsystem.Interpreter
import Wire.HashPassword
import Wire.MockInterpreters
import Wire.PasswordResetCodeStore
import Wire.PasswordStore
import Wire.Sem.Logger.TinyLog
import Wire.Sem.Now
import Wire.SessionStore
import Wire.UserKeyStore
import Wire.UserSubsystem

type AllEffects =
  [ HashPassword,
    Now,
    Input (Local ()),
    Input (Maybe AllowlistEmailDomains),
    Input (Maybe AllowlistPhonePrefixes),
    SessionStore,
    State (Map UserId [Cookie ()]),
    PasswordStore,
    State (Map UserId Password),
    PasswordResetCodeStore,
    State (Map PasswordResetKey (PRQueryData Identity)),
    TinyLog,
    UserSubsystem,
    Error AuthenticationSubsystemError
  ]

interpretDependencies :: Domain -> [UserAccount] -> Map UserId Password -> Maybe [Text] -> Sem AllEffects a -> Either AuthenticationSubsystemError a
interpretDependencies localDomain preexistingUsers preexistingPasswords mAllowedEmailDomains =
  run
    . runError
    . userSubsystemTestInterpreter preexistingUsers
    . discardTinyLogs
    . evalState mempty
    . inMemoryPasswordResetCodeStore
    . evalState preexistingPasswords
    . inMemoryPasswordStoreInterpreter
    . evalState mempty
    . inMemorySessionStoreInterpreter
    . runInputConst Nothing
    . runInputConst (AllowlistEmailDomains <$> mAllowedEmailDomains)
    . runInputConst (toLocalUnsafe localDomain ())
    . interpretNowConst (UTCTime (ModifiedJulianDay 0) 0)
    . staticHashPasswordInterpreter

spec :: Spec
spec = describe "AuthenticationSubsystem.Interpreter" do
  describe "password reset" do
    prop "happy path" $
      \email userNoEmail (cookiesWithTTL :: [(Cookie (), Maybe TTL)]) mPreviousPassword newPassword ->
        let user = userNoEmail {userIdentity = Just $ EmailIdentity email}
            uid = User.userId user
            localDomain = userNoEmail.userQualifiedId.qDomain
            Right (newPasswordHash, cookiesAfterReset) =
              interpretDependencies localDomain [UserAccount user Active] mempty Nothing
                . interpretAuthenticationSubsystem
                $ do
                  forM_ mPreviousPassword (hashPasswordArgon2id >=> upsertHashedPassword uid)
                  mapM_ (uncurry (insertCookie uid)) cookiesWithTTL

                  (_, (_, code)) <- createPasswordResetCode (userEmailKey email)
                  resetPassword (PasswordResetEmailIdentity email) code newPassword

                  (,) <$> lookupHashedPassword uid <*> listCookies uid
         in mPreviousPassword /= Just newPassword ==>
              (fmap (verifyPassword newPassword) newPasswordHash === Just True)
                .&&. (cookiesAfterReset === [])

    prop "reset code is not generated when email is not in allow list" $
      \email localDomain ->
        let createPasswordResetCodeResult =
              interpretDependencies localDomain [] mempty (Just ["example.com"])
                . interpretAuthenticationSubsystem
                $ createPasswordResetCode (userEmailKey email)
         in emailDomain email /= "exmaple.com" ==>
              createPasswordResetCodeResult === Left AuthenticationSubsystemAllowListError

    prop "reset code is generated when email is in allow list" $
      \email userNoEmail ->
        let user = userNoEmail {userIdentity = Just $ EmailIdentity email}
            localDomain = userNoEmail.userQualifiedId.qDomain
            createPasswordResetCodeResult =
              interpretDependencies localDomain [UserAccount user Active] mempty (Just [emailDomain email])
                . interpretAuthenticationSubsystem
                $ createPasswordResetCode (userEmailKey email)
         in counterexample ("expected Right, got: " <> show createPasswordResetCodeResult) $
              isRight createPasswordResetCodeResult

    prop "reset code is not generated for when user's status is not Active" $
      \email userNoEmail status ->
        let user = userNoEmail {userIdentity = Just $ EmailIdentity email}
            localDomain = userNoEmail.userQualifiedId.qDomain
            createPasswordResetCodeResult =
              interpretDependencies localDomain [UserAccount user status] mempty Nothing
                . interpretAuthenticationSubsystem
                $ createPasswordResetCode (userEmailKey email)
         in status /= Active ==>
              createPasswordResetCodeResult === Left AuthenticationSubsystemInvalidPasswordResetKey

    prop "reset code is not generated for when there is no user for the email" $
      \email localDomain ->
        let createPasswordResetCodeResult =
              interpretDependencies localDomain [] mempty Nothing
                . interpretAuthenticationSubsystem
                $ createPasswordResetCode (userEmailKey email)
         in createPasswordResetCodeResult === Left AuthenticationSubsystemInvalidPasswordResetKey

    prop "reset code is only generated once" $
      \email userNoEmail newPassword ->
        let user = userNoEmail {userIdentity = Just $ EmailIdentity email}
            uid = User.userId user
            localDomain = userNoEmail.userQualifiedId.qDomain
            Right (newPasswordHash, mCaughtException) =
              interpretDependencies localDomain [UserAccount user Active] mempty Nothing
                . interpretAuthenticationSubsystem
                $ do
                  (_, (_, code)) <- createPasswordResetCode (userEmailKey email)

                  mCaughtExc <- (const Nothing <$> createPasswordResetCode (userEmailKey email)) `catch` (pure . Just)

                  -- Reset passwrod still works with previously generated reset code
                  resetPassword (PasswordResetEmailIdentity email) code newPassword

                  (,mCaughtExc) <$> lookupHashedPassword uid
         in (fmap (verifyPassword newPassword) newPasswordHash === Just True)
              .&&. (mCaughtException === Just AuthenticationSubsystemPasswordResetInProgress)

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
