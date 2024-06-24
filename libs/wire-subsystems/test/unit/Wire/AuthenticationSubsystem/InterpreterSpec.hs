{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Wire.AuthenticationSubsystem.InterpreterSpec (spec) where

import Data.Domain
import Data.Id
import Data.Misc (PlainTextPassword8)
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
  [ Error AuthenticationSubsystemError,
    HashPassword,
    Now,
    State UTCTime,
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
    UserSubsystem
  ]

interpretDependencies :: Domain -> [UserAccount] -> Map UserId Password -> Maybe [Text] -> Sem AllEffects a -> Either AuthenticationSubsystemError a
interpretDependencies localDomain preexistingUsers preexistingPasswords mAllowedEmailDomains =
  run
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
    . evalState defaultTime
    . interpretNowAsState
    . staticHashPasswordInterpreter
    . runError

defaultTime :: UTCTime
defaultTime = UTCTime (ModifiedJulianDay 0) 0

spec :: Spec
spec = describe "AuthenticationSubsystem.Interpreter" do
  describe "password reset" do
    prop "password reset should work with the email being used as password reset key" $
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

    prop "password reset should work with the returned password reset key" $
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

                  (_, (passwordResetKey, code)) <- createPasswordResetCode (userEmailKey email)
                  resetPassword (PasswordResetIdentityKey passwordResetKey) code newPassword

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

                  mCaughtExc <- catchExpectedError $ createPasswordResetCode (userEmailKey email)

                  -- Reset passwrod still works with previously generated reset code
                  resetPassword (PasswordResetEmailIdentity email) code newPassword

                  (,mCaughtExc) <$> lookupHashedPassword uid
         in (fmap (verifyPassword newPassword) newPasswordHash === Just True)
              .&&. (mCaughtException === Just AuthenticationSubsystemPasswordResetInProgress)

    prop "reset code is not accepted after expiry" $
      \email userNoEmail oldPassword newPassword ->
        let user = userNoEmail {userIdentity = Just $ EmailIdentity email}
            uid = User.userId user
            localDomain = userNoEmail.userQualifiedId.qDomain
            Right (passwordInDB, resetPasswordResult) =
              interpretDependencies localDomain [UserAccount user Active] mempty Nothing
                . interpretAuthenticationSubsystem
                $ do
                  upsertHashedPassword uid =<< hashPasswordArgon2id oldPassword
                  (_, (_, code)) <- createPasswordResetCode (userEmailKey email)

                  passTime (passwordResetCodeTtl + 1)

                  mCaughtExc <- catchExpectedError $ resetPassword (PasswordResetEmailIdentity email) code newPassword
                  (,mCaughtExc) <$> lookupHashedPassword uid
         in resetPasswordResult === Just AuthenticationSubsystemInvalidPasswordResetCode
              .&&. verifyPasswordProp oldPassword passwordInDB

    prop "password reset is not allowed with arbitrary codes when no other codes exist" $
      \email userNoEmail resetCode oldPassword newPassword ->
        let user = userNoEmail {userIdentity = Just $ EmailIdentity email}
            uid = User.userId user
            localDomain = userNoEmail.userQualifiedId.qDomain
            Right (passwordInDB, resetPasswordResult) =
              interpretDependencies localDomain [UserAccount user Active] mempty Nothing
                . interpretAuthenticationSubsystem
                $ do
                  upsertHashedPassword uid =<< hashPasswordArgon2id oldPassword
                  mCaughtExc <- catchExpectedError $ resetPassword (PasswordResetEmailIdentity email) resetCode newPassword
                  (,mCaughtExc) <$> lookupHashedPassword uid
         in resetPasswordResult === Just AuthenticationSubsystemInvalidPasswordResetCode
              .&&. verifyPasswordProp oldPassword passwordInDB

    prop "password reset doesn't work if email is wrong" $
      \email wrongEmail userNoEmail resetCode oldPassword newPassword ->
        let user = userNoEmail {userIdentity = Just $ EmailIdentity email}
            uid = User.userId user
            localDomain = userNoEmail.userQualifiedId.qDomain
            Right (passwordInDB, resetPasswordResult) =
              interpretDependencies localDomain [UserAccount user Active] mempty Nothing
                . interpretAuthenticationSubsystem
                $ do
                  hashAndUpsertPassword uid oldPassword
                  mCaughtExc <- catchExpectedError $ resetPassword (PasswordResetEmailIdentity wrongEmail) resetCode newPassword
                  (,mCaughtExc) <$> lookupHashedPassword uid
         in email /= wrongEmail ==>
              resetPasswordResult === Just AuthenticationSubsystemInvalidPasswordResetKey
                .&&. verifyPasswordProp oldPassword passwordInDB

    prop "only 3 wrong password reset attempts are allowed" $
      \email userNoEmail arbitraryResetCode oldPassword newPassword (Upto4 wrongResetAttempts) ->
        let user = userNoEmail {userIdentity = Just $ EmailIdentity email}
            uid = User.userId user
            localDomain = userNoEmail.userQualifiedId.qDomain
            Right (passwordHashInDB, correctResetCode, wrongResetErrors, resetPassworedWithCorectCodeResult) =
              interpretDependencies localDomain [UserAccount user Active] mempty Nothing
                . interpretAuthenticationSubsystem
                $ do
                  upsertHashedPassword uid =<< hashPasswordArgon2id oldPassword
                  (_, (_, generatedResetCode)) <- createPasswordResetCode (userEmailKey email)

                  wrongResetErrs <-
                    replicateM wrongResetAttempts $
                      catchExpectedError $
                        resetPassword (PasswordResetEmailIdentity email) arbitraryResetCode newPassword

                  mFinalResetErr <- catchExpectedError $ resetPassword (PasswordResetEmailIdentity email) generatedResetCode newPassword
                  (,generatedResetCode,wrongResetErrs,mFinalResetErr) <$> lookupHashedPassword uid
            expectedFinalResetResult =
              if wrongResetAttempts >= 3
                then Just AuthenticationSubsystemInvalidPasswordResetCode
                else Nothing
            expectedFinalPassword =
              if wrongResetAttempts >= 3
                then oldPassword
                else newPassword
         in correctResetCode /= arbitraryResetCode ==>
              wrongResetErrors == replicate wrongResetAttempts (Just AuthenticationSubsystemInvalidPasswordResetCode)
                .&&. resetPassworedWithCorectCodeResult === expectedFinalResetResult
                .&&. verifyPasswordProp expectedFinalPassword passwordHashInDB

    it "wrong user" do
      pending

newtype Upto4 = Upto4 Int
  deriving newtype (Show, Eq)

instance Arbitrary Upto4 where
  arbitrary = Upto4 <$> elements [0 .. 4]

verifyPasswordProp :: PlainTextPassword8 -> Maybe Password -> Property
verifyPasswordProp plainTextPassword passwordHash =
  counterexample ("Password doesn't match, plainText=" <> show plainTextPassword <> ", passwordHasH=" <> show passwordHash) $
    fmap (verifyPassword plainTextPassword) passwordHash == Just True

hashAndUpsertPassword :: (Member PasswordStore r, Member HashPassword r) => UserId -> PlainTextPassword8 -> Sem r ()
hashAndUpsertPassword uid password =
  upsertHashedPassword uid =<< hashPasswordArgon2id password
