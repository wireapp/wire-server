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
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Wire.API.Password
import Wire.API.User
import Wire.API.User.Auth
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

-- TODO: unify with minibackend
inMemorySessionStoreInterpreter :: (Member (State (Map UserId [Cookie ()])) r) => InterpreterFor SessionStore r
inMemorySessionStoreInterpreter = interpret $ \case
  InsertCookie uid cookie _ttl -> modify $ Map.insertWith (<>) uid [cookie]
  ListCookies uid -> gets (Map.findWithDefault [] uid)
  DeleteAllCookies uid -> modify $ Map.delete uid
  DeleteCookies uid cc -> (error "implement on demand") uid cc
  LookupCookie uid time cid -> (error "implement on demand") uid time cid

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

-- TODO: unify with mini backend
runErrorUnsafe :: (HasCallStack, Exception e) => InterpreterFor (Error e) r
runErrorUnsafe action = do
  res <- runError action
  case res of
    Left e -> error $ "Unexpected error: " <> displayException e
    Right x -> pure x

type AllEffects =
  [ HashPassword,
    Now,
    Input (Local ()),
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

interpretDependencies :: Domain -> [UserAccount] -> Map UserId Password -> Sem AllEffects a -> a
interpretDependencies localDomain preexistingUsers preexistingPasswords =
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
              interpretDependencies localDomain [UserAccount user Active] mempty
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
