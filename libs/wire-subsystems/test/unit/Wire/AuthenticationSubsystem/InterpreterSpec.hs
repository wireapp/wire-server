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
import Wire.API.Allowlists (AllowlistEmailDomains, AllowlistPhonePrefixes)
import Wire.API.Password
import Wire.API.User
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

interpretDependencies :: Domain -> [UserAccount] -> Map UserId Password -> Maybe AllowlistEmailDomains -> Sem AllEffects a -> a
interpretDependencies localDomain preexistingUsers preexistingPasswords mAllowedEmailDomains =
  run
    . runErrorUnsafe
    . userSubsystemTestInterpreter preexistingUsers
    . discardTinyLogs
    . evalState mempty
    . inMemoryPasswordResetCodeStore
    . evalState preexistingPasswords
    . inMemoryPasswordStoreInterpreter
    . evalState mempty
    . inMemorySessionStoreInterpreter
    . runInputConst Nothing
    . runInputConst mAllowedEmailDomains
    . runInputConst (toLocalUnsafe localDomain ())
    . interpretNowConst (UTCTime (ModifiedJulianDay 0) 0)
    . staticHashPasswordInterpreter

spec :: Spec
spec = describe "AuthenticationSubsystem.Interpreter" do
  describe "password reset" do
    prop "happy path" $
      \email userNoEmail (cookiesWithTTL :: [(Cookie (), Maybe TTL)]) mPreviousPassword newPassword ->
        let user = userNoEmail {userIdentity = Just $ EmailIdentity email}
            uid = userId user
            localDomain = userNoEmail.userQualifiedId.qDomain
            (newPasswordHash, cookiesAfterReset) =
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

    it "email not in allow list" do
      pending

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
