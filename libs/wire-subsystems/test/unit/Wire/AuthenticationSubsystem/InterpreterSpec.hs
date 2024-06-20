{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Wire.AuthenticationSubsystem.InterpreterSpec (spec) where

import Data.Domain
import Data.Id
import Data.Map qualified as Map
import Data.Misc
import Data.Qualified
import Data.Text.Ascii
import Data.Text.Encoding qualified as Text
import Data.Time
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.State
import Polysemy.TinyLog
import Test.Hspec
import Test.QuickCheck
import Wire.API.Password
import Wire.API.User
import Wire.API.User.Password
import Wire.AuthenticationSubsystem
import Wire.AuthenticationSubsystem.Interpreter
import Wire.HashPassword
import Wire.PasswordResetCodeStore
import Wire.PasswordStore
import Wire.Sem.Logger.TinyLog
import Wire.Sem.Now
import Wire.SessionStore
import Wire.UserKeyStore
import Wire.UserSubsystem

userSubsystemTestInterpreter :: [UserAccount] -> InterpreterFor UserSubsystem r
userSubsystemTestInterpreter initialUsers =
  interpret \case
    GetLocalUserAccountByUserKey localUserKey -> case (tUnqualified localUserKey) of
      UserEmailKey (EmailKey _ email) -> pure $ find (\u -> userEmail u.accountUser == Just email) initialUsers
      UserPhoneKey _ -> pure Nothing -- Phone stuff is deprecated and soon to be deleted anyway
    _ -> undefined

inMemoryPasswordStoreInterpreter :: (Member (State (Map UserId Password)) r) => InterpreterFor PasswordStore r
inMemoryPasswordStoreInterpreter = interpret $ \case
  UpsertHashedPassword uid password -> modify $ Map.insert uid password
  LookupHashedPassword uid -> gets $ Map.lookup uid

inMemoryPasswordResetCodeStore ::
  forall r.
  (Member (State (Map PasswordResetKey (PRQueryData Identity))) r) =>
  InterpreterFor PasswordResetCodeStore r
inMemoryPasswordResetCodeStore =
  interpret
    \case
      GenerateEmailCode ->
        pure . PasswordResetCode . encodeBase64Url $ "email-code"
      GeneratePhoneCode -> (error "deprecated")
      CodeSelect resetKey ->
        gets $ \codes ->
          mapPRQueryData (Just . runIdentity)
            <$> Map.lookup resetKey codes
      CodeInsert resetKey queryData _ttl ->
        modify $ Map.insert resetKey queryData
      CodeDelete resetKey ->
        modify $ Map.delete resetKey

inMemorySessionStoreInterpreter :: InterpreterFor SessionStore r
inMemorySessionStoreInterpreter = interpret $ \case
  InsertCookie uid cookie ttl -> (error "implement on demand") uid cookie ttl
  LookupCookie uid time cid -> (error "implement on demand") uid time cid
  ListCookies uid -> (error "implement on demand") uid
  DeleteAllCookies _ -> pure ()
  DeleteCookies uid cc -> (error "implement on demand") uid cc

-- TODO: unify with minibackend
interpretNowConst ::
  UTCTime ->
  Sem (Now : r) a ->
  Sem r a
interpretNowConst time = interpret \case
  Wire.Sem.Now.Get -> pure time

-- TODO: unify with minibackend
staticHashPasswordInterpreter :: InterpreterFor HashPassword r
staticHashPasswordInterpreter = interpret $ \case
  HashPasswordScrypt password -> go hashPasswordScryptWithSalt password
  HashPasswordArgon2id password -> go hashPasswordScryptWithSalt password
  where
    go alg password = do
      let passwordBS = Text.encodeUtf8 (fromPlainTextPassword password)
      pure $ unsafeMkPassword $ alg "salt" passwordBS

-- TOOD: unify with mini backend
runErrorUnsafe :: (HasCallStack, Exception e) => InterpreterFor (Error e) r
runErrorUnsafe action = do
  res <- runError action
  case res of
    Left e -> error $ "Unexpected error: " <> displayException e
    Right x -> pure x

type AllEffects =
  [ AuthenticationSubsystem,
    HashPassword,
    Now,
    Input (Local ()),
    SessionStore,
    PasswordStore,
    State (Map UserId Password),
    PasswordResetCodeStore,
    State (Map PasswordResetKey (PRQueryData Identity)),
    TinyLog,
    UserSubsystem,
    Error AuthenticationSubsystemError
  ]

interpreterUnderTest :: Domain -> [UserAccount] -> Map UserId Password -> Sem AllEffects a -> a
interpreterUnderTest localDomain preexistingUsers preexistingPasswords =
  run
    . runErrorUnsafe
    . userSubsystemTestInterpreter preexistingUsers
    . discardTinyLogs
    . evalState mempty
    . inMemoryPasswordResetCodeStore
    . evalState preexistingPasswords
    . inMemoryPasswordStoreInterpreter
    . inMemorySessionStoreInterpreter
    . runInputConst (toLocalUnsafe localDomain ())
    . interpretNowConst (UTCTime (ModifiedJulianDay 0) 0)
    . staticHashPasswordInterpreter
    . interpretAuthenticationSubsystem

spec :: Spec
spec = describe "AuthenticationSubsystem.Interpreter" do
  describe "password reset" do
    it "happy path" do
      -- TODO: Also assert that the cookies are gone
      -- config :: UserSubsystemConfig <- generate arbitrary
      email <- generate arbitrary
      userNoEmail <- generate arbitrary
      let userAccount =
            UserAccount
              { accountUser = userNoEmail {userIdentity = Just $ EmailIdentity email},
                accountStatus = Active
              }
          newPassword = plainTextPassword8Unsafe "newsecret"
          pureInterpreter =
            interpreterUnderTest
              userNoEmail.userQualifiedId.qDomain
              [userAccount]
              mempty
          newPasswordHash = pureInterpreter $ do
            (_, (_, code)) <- createPasswordResetCode (userEmailKey email)
            resetPassword (PasswordResetEmailIdentity email) code newPassword
            lookupHashedPassword (userId userNoEmail)
      verifyPassword newPassword <$> newPasswordHash `shouldBe` Just True

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
