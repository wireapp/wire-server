{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Wire.AuthenticationSubsystem.InterpreterSpec (spec) where

import Data.Domain
import Data.Id
import Data.Misc (PlainTextPassword8)
import Data.Qualified
import Data.Text.Encoding (decodeUtf8)
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
import Wire.API.Allowlists (AllowlistEmailDomains (AllowlistEmailDomains))
import Wire.API.Password as Password
import Wire.API.User
import Wire.API.User qualified as User
import Wire.API.User.Auth
import Wire.API.User.Password
import Wire.AuthenticationSubsystem
import Wire.AuthenticationSubsystem.Interpreter
import Wire.EmailSubsystem
import Wire.HashPassword
import Wire.MockInterpreters
import Wire.PasswordResetCodeStore
import Wire.PasswordStore
import Wire.Sem.Logger.TinyLog
import Wire.Sem.Now (Now)
import Wire.SessionStore
import Wire.UserKeyStore

type AllEffects =
  [ AuthenticationSubsystem,
    Error AuthenticationSubsystemError,
    HashPassword,
    Now,
    State UTCTime,
    Input (Local ()),
    Input (Maybe AllowlistEmailDomains),
    SessionStore,
    State (Map UserId [Cookie ()]),
    PasswordStore,
    PasswordResetCodeStore,
    State (Map PasswordResetKey (PRQueryData Identity)),
    TinyLog,
    EmailSubsystem,
    State (Map EmailAddress [SentMail])
  ]

runAllEffects :: Domain -> [ExtendedUserAccount] -> Maybe [Text] -> Sem AllEffects a -> Either AuthenticationSubsystemError a
runAllEffects localDomain preexistingUsers mAllowedEmailDomains =
  run
    . evalState mempty
    . emailSubsystemInterpreter
    . discardTinyLogs
    . evalState mempty
    . inMemoryPasswordResetCodeStore
    . runInMemoryPasswordStoreInterpreter
    . evalState mempty
    . inMemorySessionStoreInterpreter
    . runInputConst (AllowlistEmailDomains <$> mAllowedEmailDomains)
    . runInputConst (toLocalUnsafe localDomain ())
    . evalState defaultTime
    . interpretNowAsState
    . staticHashPasswordInterpreter
    . runError
    . interpretAuthenticationSubsystem (userSubsystemTestInterpreter preexistingUsers)

spec :: Spec
spec = describe "AuthenticationSubsystem.Interpreter" do
  describe "password reset" do
    prop "password reset should work with the email being used as password reset key" $
      \email userNoEmail (cookiesWithTTL :: [(Cookie (), Maybe TTL)]) mPreviousPassword newPassword ->
        let user =
              userNoEmail
                { userIdentity = Just $ EmailIdentity email,
                  userStatus = Active
                }
            uid = User.userId user
            localDomain = userNoEmail.userQualifiedId.qDomain
            Right (newPasswordHash, cookiesAfterReset) =
              runAllEffects localDomain [ExtendedUserAccount user Nothing] Nothing $ do
                forM_ mPreviousPassword (hashPassword >=> upsertHashedPassword uid)
                mapM_ (uncurry (insertCookie uid)) cookiesWithTTL

                createPasswordResetCode (mkEmailKey email)
                (_, code) <- expect1ResetPasswordEmail email
                resetPassword (PasswordResetEmailIdentity email) code newPassword

                (,) <$> lookupHashedPassword uid <*> listCookies uid
         in mPreviousPassword /= Just newPassword ==>
              (fmap (Password.verifyPassword newPassword) newPasswordHash === Just True)
                .&&. (cookiesAfterReset === [])

    prop "password reset should work with the returned password reset key" $
      \email userNoEmail (cookiesWithTTL :: [(Cookie (), Maybe TTL)]) mPreviousPassword newPassword ->
        let user =
              userNoEmail
                { userIdentity = Just $ EmailIdentity email,
                  userStatus = Active
                }
            uid = User.userId user
            localDomain = userNoEmail.userQualifiedId.qDomain
            Right (newPasswordHash, cookiesAfterReset) =
              runAllEffects localDomain [ExtendedUserAccount user Nothing] Nothing $ do
                forM_ mPreviousPassword (hashPassword >=> upsertHashedPassword uid)
                mapM_ (uncurry (insertCookie uid)) cookiesWithTTL

                createPasswordResetCode (mkEmailKey email)
                (passwordResetKey, code) <- expect1ResetPasswordEmail email
                resetPassword (PasswordResetIdentityKey passwordResetKey) code newPassword

                (,) <$> lookupHashedPassword uid <*> listCookies uid
         in mPreviousPassword /= Just newPassword ==>
              (fmap (Password.verifyPassword newPassword) newPasswordHash === Just True)
                .&&. (cookiesAfterReset === [])

    prop "reset code is not generated when email is not in allow list" $
      \email localDomain ->
        let createPasswordResetCodeResult =
              runAllEffects localDomain [] (Just ["example.com"]) $
                createPasswordResetCode (mkEmailKey email)
                  <* expectNoEmailSent
         in domainPart email /= "example.com" ==>
              createPasswordResetCodeResult === Right ()

    prop "reset code is generated when email is in allow list" $
      \email userNoEmail ->
        let user =
              userNoEmail
                { userIdentity = Just $ EmailIdentity email,
                  userStatus = Active
                }
            localDomain = userNoEmail.userQualifiedId.qDomain
            createPasswordResetCodeResult =
              runAllEffects localDomain [ExtendedUserAccount user Nothing] (Just [decodeUtf8 $ domainPart email]) $
                createPasswordResetCode (mkEmailKey email)
         in counterexample ("expected Right, got: " <> show createPasswordResetCodeResult) $
              isRight createPasswordResetCodeResult

    prop "reset code is not generated for when user's status is not Active" $
      \email userNoEmail ->
        let user =
              userNoEmail
                { userIdentity = Just $ EmailIdentity email,
                  userStatus = Active
                }
            localDomain = userNoEmail.userQualifiedId.qDomain
            createPasswordResetCodeResult =
              runAllEffects localDomain [ExtendedUserAccount user Nothing] Nothing $
                createPasswordResetCode (mkEmailKey email)
                  <* expectNoEmailSent
         in createPasswordResetCodeResult === Right ()

    prop "reset code is not generated for when there is no user for the email" $
      \email localDomain ->
        let createPasswordResetCodeResult =
              runAllEffects localDomain [] Nothing $
                createPasswordResetCode (mkEmailKey email)
                  <* expectNoEmailSent
         in createPasswordResetCodeResult === Right ()

    prop "reset code is only generated once" $
      \email userNoEmail newPassword ->
        let user =
              userNoEmail
                { userIdentity = Just $ EmailIdentity email,
                  userStatus = Active
                }
            uid = User.userId user
            localDomain = userNoEmail.userQualifiedId.qDomain
            Right (newPasswordHash, mCaughtException) =
              runAllEffects localDomain [ExtendedUserAccount user Nothing] Nothing $ do
                createPasswordResetCode (mkEmailKey email)
                (_, code) <- expect1ResetPasswordEmail email

                mCaughtExc <- catchExpectedError $ createPasswordResetCode (mkEmailKey email)

                -- Reset password still works with previously generated reset code
                resetPassword (PasswordResetEmailIdentity email) code newPassword

                (,mCaughtExc) <$> lookupHashedPassword uid
         in (fmap (Password.verifyPassword newPassword) newPasswordHash === Just True)
              .&&. (mCaughtException === Nothing)

    prop "reset code is not accepted after expiry" $
      \email userNoEmail oldPassword newPassword ->
        let user =
              userNoEmail
                { userIdentity = Just $ EmailIdentity email,
                  userStatus = Active
                }
            uid = User.userId user
            localDomain = userNoEmail.userQualifiedId.qDomain
            Right (passwordInDB, resetPasswordResult) =
              runAllEffects localDomain [ExtendedUserAccount user Nothing] Nothing $ do
                upsertHashedPassword uid =<< hashPassword oldPassword
                createPasswordResetCode (mkEmailKey email)
                (_, code) <- expect1ResetPasswordEmail email

                passTime (passwordResetCodeTtl + 1)

                mCaughtExc <- catchExpectedError $ resetPassword (PasswordResetEmailIdentity email) code newPassword
                (,mCaughtExc) <$> lookupHashedPassword uid
         in resetPasswordResult === Just AuthenticationSubsystemInvalidPasswordResetCode
              .&&. verifyPasswordProp oldPassword passwordInDB

    prop "password reset is not allowed with arbitrary codes when no other codes exist" $
      \email userNoEmail resetCode oldPassword newPassword ->
        let user =
              userNoEmail
                { userIdentity = Just $ EmailIdentity email,
                  userStatus = Active
                }
            uid = User.userId user
            localDomain = userNoEmail.userQualifiedId.qDomain
            Right (passwordInDB, resetPasswordResult) =
              runAllEffects localDomain [ExtendedUserAccount user Nothing] Nothing $ do
                upsertHashedPassword uid =<< hashPassword oldPassword
                mCaughtExc <- catchExpectedError $ resetPassword (PasswordResetEmailIdentity email) resetCode newPassword
                (,mCaughtExc) <$> lookupHashedPassword uid
         in resetPasswordResult === Just AuthenticationSubsystemInvalidPasswordResetCode
              .&&. verifyPasswordProp oldPassword passwordInDB

    prop "password reset doesn't work if email is wrong" $
      \email wrongEmail userNoEmail resetCode oldPassword newPassword ->
        let user =
              userNoEmail
                { userIdentity = Just $ EmailIdentity email,
                  userStatus = Active
                }
            uid = User.userId user
            localDomain = userNoEmail.userQualifiedId.qDomain
            Right (passwordInDB, resetPasswordResult) =
              runAllEffects localDomain [ExtendedUserAccount user Nothing] Nothing $ do
                hashAndUpsertPassword uid oldPassword
                mCaughtExc <- catchExpectedError $ resetPassword (PasswordResetEmailIdentity wrongEmail) resetCode newPassword
                (,mCaughtExc) <$> lookupHashedPassword uid
         in email /= wrongEmail ==>
              resetPasswordResult === Just AuthenticationSubsystemInvalidPasswordResetKey
                .&&. verifyPasswordProp oldPassword passwordInDB

    prop "only 3 wrong password reset attempts are allowed" $
      \email userNoEmail arbitraryResetCode oldPassword newPassword (Upto4 wrongResetAttempts) ->
        let user =
              userNoEmail
                { userIdentity = Just $ EmailIdentity email,
                  userStatus = Active
                }
            uid = User.userId user
            localDomain = userNoEmail.userQualifiedId.qDomain
            Right (passwordHashInDB, correctResetCode, wrongResetErrors, resetPassworedWithCorectCodeResult) =
              runAllEffects localDomain [ExtendedUserAccount user Nothing] Nothing $ do
                upsertHashedPassword uid =<< hashPassword oldPassword
                createPasswordResetCode (mkEmailKey email)
                (_, generatedResetCode) <- expect1ResetPasswordEmail email

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

  describe "internalLookupPasswordResetCode" do
    prop "should find password reset code by email" $
      \email userNoEmail newPassword ->
        let user =
              userNoEmail
                { userIdentity = Just $ EmailIdentity email,
                  userStatus = Active
                }
            uid = User.userId user
            localDomain = userNoEmail.userQualifiedId.qDomain
            Right passwordHashInDB =
              runAllEffects localDomain [ExtendedUserAccount user Nothing] Nothing $ do
                void $ createPasswordResetCode (mkEmailKey email)
                mLookupRes <- internalLookupPasswordResetCode (mkEmailKey email)
                for_ mLookupRes $ \(_, code) -> resetPassword (PasswordResetEmailIdentity email) code newPassword
                lookupHashedPassword uid
         in verifyPasswordProp newPassword passwordHashInDB

newtype Upto4 = Upto4 Int
  deriving newtype (Show, Eq)

instance Arbitrary Upto4 where
  arbitrary = Upto4 <$> elements [0 .. 4]

verifyPasswordProp :: PlainTextPassword8 -> Maybe Password -> Property
verifyPasswordProp plainTextPassword passwordHash =
  counterexample ("Password doesn't match, plainText=" <> show plainTextPassword <> ", passwordHash=" <> show passwordHash) $
    fmap (Password.verifyPassword plainTextPassword) passwordHash == Just True

hashAndUpsertPassword :: (Member PasswordStore r, Member HashPassword r) => UserId -> PlainTextPassword8 -> Sem r ()
hashAndUpsertPassword uid password =
  upsertHashedPassword uid =<< hashPassword password

expect1ResetPasswordEmail :: (Member (State (Map EmailAddress [SentMail])) r) => EmailAddress -> Sem r PasswordResetPair
expect1ResetPasswordEmail email =
  getEmailsSentTo email
    <&> \case
      [] -> error "no emails sent"
      [SentMail _ (PasswordResetMail resetPair)] -> resetPair
      wrongEmails -> error $ "Wrong emails sent: " <> show wrongEmails

expectNoEmailSent :: (Member (State (Map EmailAddress [SentMail])) r) => Sem r ()
expectNoEmailSent = do
  emails <- get
  if null emails
    then pure ()
    else error $ "Expected no emails sent, got: " <> show emails
