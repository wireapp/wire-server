{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.AuthenticationSubsystem.InterpreterSpec (spec) where

import Data.Domain
import Data.Id
import Data.Misc
import Data.Qualified
import Data.Range (rcast)
import Data.Set qualified as Set
import Data.Text.Encoding (decodeUtf8)
import Data.Time
import Data.ZAuth.CryptoSign (CryptoSign)
import Data.ZAuth.Token qualified as ZAuth
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
import Wire.API.User
import Wire.API.User.Auth
import Wire.API.User.Password
import Wire.API.UserEvent
import Wire.AppStore
import Wire.AuthenticationSubsystem
import Wire.AuthenticationSubsystem.Config
import Wire.AuthenticationSubsystem.Interpreter
import Wire.AuthenticationSubsystem.ZAuth (randomConnId)
import Wire.EmailSubsystem
import Wire.Events
import Wire.HashPassword
import Wire.MiniBackend
import Wire.MockInterpreters
import Wire.MockInterpreters.Events as Event
import Wire.PasswordResetCodeStore
import Wire.PasswordStore
import Wire.PasswordStore qualified as PasswordStore
import Wire.RateLimit
import Wire.Sem.Logger.TinyLog
import Wire.Sem.Now (Now)
import Wire.Sem.Random (Random)
import Wire.SessionStore
import Wire.StoredUser
import Wire.UserKeyStore
import Wire.UserStore

type AllEffects =
  [ AuthenticationSubsystem,
    Events,
    Error AuthenticationSubsystemError,
    Error RateLimitExceeded,
    RateLimit,
    Random,
    HashPassword,
    CryptoSign,
    Now,
    State UTCTime,
    Input AuthenticationSubsystemConfig,
    SessionStore,
    State (Map UserId [Cookie ()]),
    PasswordStore,
    PasswordResetCodeStore,
    State (Map PasswordResetKey (PRQueryData Identity)),
    TinyLog,
    EmailSubsystem,
    UserStore,
    UserKeyStore,
    State [MiniEvent],
    State (Map EmailAddress [SentMail]),
    State [StoredApp]
  ]

runAllEffects :: Domain -> [StoredUser] -> Maybe [Text] -> Sem AllEffects a -> Either AuthenticationSubsystemError a
runAllEffects domain users emailDomains action = snd $ runAllEffectsWithEventState domain users emailDomains action

runAllEffectsWithEventState :: Domain -> [StoredUser] -> Maybe [Text] -> Sem AllEffects a -> ([MiniEvent], Either AuthenticationSubsystemError a)
runAllEffectsWithEventState localDomain preexistingUsers mAllowedEmailDomains =
  let cfg =
        defaultAuthenticationSubsystemConfig
          { allowlistEmailDomains = AllowlistEmailDomains <$> mAllowedEmailDomains,
            local = toLocalUnsafe localDomain ()
          }
   in run
        . evalState mempty
        . evalState mempty
        . runState mempty
        . runInMemoryUserKeyStoreIntepreterWithStoredUsers preexistingUsers
        . runInMemoryUserStoreInterpreter preexistingUsers
        . inMemoryEmailSubsystemInterpreter
        . discardTinyLogs
        . evalState mempty
        . inMemoryPasswordResetCodeStore
        . runInMemoryPasswordStoreInterpreter
        . evalState mempty
        . inMemorySessionStoreInterpreter
        . runInputConst cfg
        . evalState defaultTime
        . interpretNowAsState
        . runCryptoSignUnsafe
        . staticHashPasswordInterpreter
        . runRandomPure
        . noRateLimit
        . runErrorUnsafe
        . runError
        . miniEventInterpreter
        . interpretAuthenticationSubsystem inMemoryUserSubsystemInterpreter

toInputPassword :: PlainTextPassword8 -> PlainTextPassword6
toInputPassword pw8 =
  PlainTextPassword' . rcast $ fromPlainTextPassword' pw8

spec :: Spec
spec = describe "AuthenticationSubsystem.Interpreter" do
  describe "password reset" do
    prop "password reset should work with the email being used as password reset key" $
      \email userNoEmail (cookiesWithTTL :: [(Cookie (), Maybe TTL)]) mPreviousPassword newPassword ->
        let user =
              userNoEmail
                { email = Just email,
                  emailUnvalidated = Nothing,
                  status = Just Active
                }
            uid = user.id
            eithRes =
              runAllEffects testDomain [user] Nothing $ do
                forM_ mPreviousPassword (hashPassword >=> PasswordStore.upsertHashedPassword uid)
                mapM_ (uncurry (insertCookie uid)) cookiesWithTTL

                createPasswordResetCode (mkEmailKey email)
                (_, resetCode) <- expect1ResetPasswordEmail email
                resetPassword (PasswordResetEmailIdentity email) resetCode newPassword
                (,,)
                  <$> forM mPreviousPassword (verifyUserPassword uid . toInputPassword)
                  <*> verifyUserPassword uid (toInputPassword newPassword)
                  <*> listCookies uid
         in case eithRes of
              Left e -> counterexample ("Unexpected Error: " <> show e) False
              Right (mOldPasswordVerification, newPasswordVerification, cookiesAfterReset) ->
                (maybe (property True) (\(verification, _) -> verification === False) mOldPasswordVerification)
                  .&&. fst newPasswordVerification === True
                  .&&. (cookiesAfterReset === [])

    prop "password reset should work with the returned password reset key" $
      \email userNoEmail (cookiesWithTTL :: [(Cookie (), Maybe TTL)]) mPreviousPassword newPassword ->
        let user =
              userNoEmail
                { email = Just email,
                  emailUnvalidated = Nothing,
                  status = Just Active
                }
            uid = user.id
            Right (newPasswordVerification, cookiesAfterReset) =
              runAllEffects testDomain [user] Nothing $ do
                forM_ mPreviousPassword (hashPassword >=> PasswordStore.upsertHashedPassword uid)
                mapM_ (uncurry (insertCookie uid)) cookiesWithTTL

                createPasswordResetCode (mkEmailKey email)
                (passwordResetKey, resetCode) <- expect1ResetPasswordEmail email
                resetPassword (PasswordResetIdentityKey passwordResetKey) resetCode newPassword

                (,) <$> verifyUserPassword uid (toInputPassword newPassword) <*> listCookies uid
         in mPreviousPassword /= Just newPassword ==>
              (fst newPasswordVerification === True)
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
                { email = Just email,
                  emailUnvalidated = Nothing,
                  status = Just Active
                }
            createPasswordResetCodeResult =
              runAllEffects testDomain [user] (Just [decodeUtf8 $ domainPart email]) $
                createPasswordResetCode (mkEmailKey email)
         in counterexample ("expected Right, got: " <> show createPasswordResetCodeResult) $
              isRight createPasswordResetCodeResult

    prop "reset code is not generated for when user's status is not Active" $
      \email userNoEmail ->
        let user =
              userNoEmail
                { email = Just email,
                  emailUnvalidated = Nothing
                }
            createPasswordResetCodeResult =
              runAllEffects testDomain [user] Nothing $
                createPasswordResetCode (mkEmailKey email)
                  <* expectNoEmailSent
         in (user.status /= Just Active && user.status /= Nothing) ==>
              createPasswordResetCodeResult === Right ()

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
                { email = Just email,
                  emailUnvalidated = Nothing,
                  status = Just Active
                }
            uid = user.id
            Right (newPasswordVerification, mCaughtException) =
              runAllEffects testDomain [user] Nothing $ do
                createPasswordResetCode (mkEmailKey email)
                (_, resetCode) <- expect1ResetPasswordEmail email

                mCaughtExc <- catchExpectedError $ createPasswordResetCode (mkEmailKey email)

                -- Reset password still works with previously generated reset code
                resetPassword (PasswordResetEmailIdentity email) resetCode newPassword

                (,mCaughtExc) <$> verifyUserPassword uid (toInputPassword newPassword)
         in (fst newPasswordVerification === True)
              .&&. (mCaughtException === Nothing)

    prop "reset code is not accepted after expiry" $
      \email userNoEmail oldPassword newPassword ->
        let user =
              userNoEmail
                { email = Just email,
                  emailUnvalidated = Nothing,
                  status = Just Active
                }
            uid = user.id
            Right (oldPasswordVerification, newPasswordVerification, resetPasswordResult) =
              runAllEffects testDomain [user] Nothing $ do
                PasswordStore.upsertHashedPassword uid =<< hashPassword oldPassword
                createPasswordResetCode (mkEmailKey email)
                (_, resetCode) <- expect1ResetPasswordEmail email

                passTime (passwordResetCodeTtl + 1)

                mCaughtExc <- catchExpectedError $ resetPassword (PasswordResetEmailIdentity email) resetCode newPassword
                (,,mCaughtExc)
                  <$> verifyUserPassword uid (toInputPassword oldPassword)
                  <*> verifyUserPassword uid (toInputPassword newPassword)
         in resetPasswordResult === Just AuthenticationSubsystemInvalidPasswordResetCode
              .&&. fst oldPasswordVerification === True
              .&&. fst newPasswordVerification === False

    prop "password reset is not allowed with arbitrary codes when no other codes exist" $
      \email userNoEmail resetCode oldPassword newPassword ->
        let user =
              userNoEmail
                { email = Just email,
                  emailUnvalidated = Nothing,
                  status = Just Active
                }
            uid = user.id
            Right (oldPasswordVerification, newPasswordVerification, resetPasswordResult) =
              runAllEffects testDomain [user] Nothing $ do
                PasswordStore.upsertHashedPassword uid =<< hashPassword oldPassword
                mCaughtExc <- catchExpectedError $ resetPassword (PasswordResetEmailIdentity email) resetCode newPassword
                (,,mCaughtExc)
                  <$> verifyUserPassword uid (toInputPassword oldPassword)
                  <*> verifyUserPassword uid (toInputPassword newPassword)
         in resetPasswordResult === Just AuthenticationSubsystemInvalidPasswordResetCode
              .&&. fst oldPasswordVerification === True
              .&&. fst newPasswordVerification === False

    prop "password reset doesn't work if email is wrong" $
      \email wrongEmail userNoEmail resetCode oldPassword newPassword ->
        let user =
              userNoEmail
                { email = Just email,
                  emailUnvalidated = Nothing,
                  status = Just Active
                }
            uid = user.id
            Right (oldPasswordVerification, newPasswordVerification, resetPasswordResult) =
              runAllEffects testDomain [user] Nothing $ do
                hashAndUpsertPassword uid oldPassword
                mCaughtExc <- catchExpectedError $ resetPassword (PasswordResetEmailIdentity wrongEmail) resetCode newPassword
                (,,mCaughtExc)
                  <$> verifyUserPassword uid (toInputPassword oldPassword)
                  <*> verifyUserPassword uid (toInputPassword newPassword)
         in email /= wrongEmail ==>
              resetPasswordResult === Just AuthenticationSubsystemInvalidPasswordResetKey
                .&&. fst oldPasswordVerification === True
                .&&. fst newPasswordVerification === False

    prop "only 3 wrong password reset attempts are allowed" $
      \email userNoEmail arbitraryResetCode oldPassword newPassword (Upto4 wrongResetAttempts) ->
        let user =
              userNoEmail
                { email = Just email,
                  emailUnvalidated = Nothing,
                  status = Just Active
                }
            uid = user.id
            Right (oldPasswordVerification, newPasswordVerification, correctResetCode, wrongResetErrors, resetPassworedWithCorectCodeResult) =
              runAllEffects testDomain [user] Nothing $ do
                PasswordStore.upsertHashedPassword uid =<< hashPassword oldPassword
                createPasswordResetCode (mkEmailKey email)
                (_, generatedResetCode) <- expect1ResetPasswordEmail email

                wrongResetErrs <-
                  replicateM wrongResetAttempts $
                    catchExpectedError $
                      resetPassword (PasswordResetEmailIdentity email) arbitraryResetCode newPassword

                mFinalResetErr <- catchExpectedError $ resetPassword (PasswordResetEmailIdentity email) generatedResetCode newPassword
                (,,generatedResetCode,wrongResetErrs,mFinalResetErr)
                  <$> verifyUserPassword uid (toInputPassword oldPassword)
                  <*> verifyUserPassword uid (toInputPassword newPassword)
            expectedFinalResetResult =
              if wrongResetAttempts >= 3
                then Just AuthenticationSubsystemInvalidPasswordResetCode
                else Nothing
            assertPasswordVerification =
              if wrongResetAttempts >= 3
                then fst oldPasswordVerification === True .&&. fst newPasswordVerification === False
                else fst oldPasswordVerification === False .&&. fst newPasswordVerification === True
         in correctResetCode /= arbitraryResetCode ==>
              wrongResetErrors == replicate wrongResetAttempts (Just AuthenticationSubsystemInvalidPasswordResetCode)
                .&&. resetPassworedWithCorectCodeResult === expectedFinalResetResult
                .&&. assertPasswordVerification

  describe "internalLookupPasswordResetCode" do
    prop "should find password reset code by email" $
      \email userNoEmail newPassword ->
        let user =
              userNoEmail
                { email = Just email,
                  emailUnvalidated = Nothing,
                  status = Just Active
                }
            uid = user.id
            Right newPasswordVerification =
              runAllEffects testDomain [user] Nothing $ do
                void $ createPasswordResetCode (mkEmailKey email)
                mLookupRes <- internalLookupPasswordResetCode (mkEmailKey email)
                for_ mLookupRes $ \(_, resetCode) -> resetPassword (PasswordResetEmailIdentity email) resetCode newPassword
                verifyUserPassword uid (toInputPassword newPassword)
         in fst newPasswordVerification === True
  describe "newCookie" $ do
    prop "trivial attributes: plain user cookie" $
      \localDomain uid cid typ mLabel ->
        let Right (plainCookie, lhCookie) = runAllEffects localDomain [] Nothing $ do
              plain <- newCookie @_ @ZAuth.U uid cid typ mLabel RevokeSameLabel
              lh <- newCookie @_ @ZAuth.U uid cid typ mLabel RevokeSameLabel
              pure (plain, lh)
            assertCookie cookie =
              cookie.cookieCreated === defaultTime
                .&&. cookie.cookieLabel === mLabel
                .&&. cookie.cookieType === typ
                .&&. cookie.cookieSucc === Nothing
         in assertCookie plainCookie
              .&&. assertCookie lhCookie

    prop "persistent plain cookie expires at configured time" $
      \localDomain uid cid mLabel ->
        let Right cookie = runAllEffects localDomain [] Nothing $ do
              newCookie @_ @ZAuth.U uid cid PersistentCookie mLabel RevokeSameLabel
         in cookie.cookieExpires === addUTCTime (fromIntegral defaultZAuthSettings.userTokenTimeout.userTokenTimeoutSeconds) defaultTime

    prop "persistent LH cookie expires at configured time" $
      \localDomain uid cid mLabel ->
        let Right cookie = runAllEffects localDomain [] Nothing $ do
              newCookie @_ @ZAuth.LU uid cid PersistentCookie mLabel RevokeSameLabel
         in cookie.cookieExpires === addUTCTime (fromIntegral defaultZAuthSettings.legalHoldUserTokenTimeout.legalHoldUserTokenTimeoutSeconds) defaultTime

    modifyMaxSuccess (const 3) . prop "cookie is persisted" $
      \localDomain uid cid mLabel -> do
        let Right (cky, sto) = runAllEffects localDomain [] Nothing $ do
              c <- newCookie @_ @ZAuth.LU uid cid PersistentCookie mLabel RevokeSameLabel
              s <- listCookies uid
              pure (c, s)
        length sto `shouldBe` 1
        (head sto).cookieId `shouldBe` cky.cookieId

    prop "old cookies with same label are revoked on insert" $
      \localDomain uid cid typ mLabel otherLabel policy ->
        let (events, Right (cookie1, cookie2, cookie3, cookies)) =
              runAllEffectsWithEventState localDomain [] Nothing $
                (,,,)
                  <$> newCookie @_ @ZAuth.U uid cid typ mLabel policy
                  <*> newCookie @_ @ZAuth.U uid cid typ mLabel policy
                  <*> newCookie @_ @ZAuth.U uid cid typ (Just otherLabel) policy
                  <*> (Set.fromList . fmap cookieId <$> listCookies uid)
            assertEvents n =
              length events === n
                .&&. conjoin
                  ( fmap
                      ( \e ->
                          e.event === UserEvent UserSessionRefreshSuggested
                            .&&. e.userId === uid
                      )
                      events
                  )
         in case policy of
              KeepSameLabel ->
                cookies === Set.fromList (cookieId <$> [cookie1, cookie2, cookie3])
                  .&&. counterexample "there should be no events" (null events)
              RevokeSameLabel -> case mLabel of
                Just l
                  | l == otherLabel ->
                      cookies === Set.singleton cookie3.cookieId
                        .&&. assertEvents 2
                Just _ ->
                  cookies === Set.fromList (cookieId <$> [cookie2, cookie3])
                    .&&. assertEvents 1
                Nothing ->
                  cookies === Set.fromList (cookieId <$> [cookie1, cookie2, cookie3])
                    .&&. counterexample "there should be no events" (null events)

    prop "same-label revocation does not affect other users" $
      \localDomain uidA uidB cid typ lab policy ->
        uidA /= uidB ==>
          let (events, Right (cookieA1, cookieB, cookieA2, cookiesA, cookiesB)) =
                runAllEffectsWithEventState localDomain [] Nothing $
                  (,,,,)
                    <$> newCookie @_ @ZAuth.U uidA cid typ (Just lab) policy
                    <*> newCookie @_ @ZAuth.U uidB cid typ (Just lab) policy
                    <*> newCookie @_ @ZAuth.U uidA cid typ (Just lab) policy
                    <*> (Set.fromList . fmap cookieId <$> listCookies uidA)
                    <*> (Set.fromList . fmap cookieId <$> listCookies uidB)
           in case policy of
                KeepSameLabel ->
                  cookiesA === Set.fromList [cookieA1.cookieId, cookieA2.cookieId]
                    .&&. cookiesB === Set.singleton cookieB.cookieId
                    .&&. counterexample "first cookie for user A should be replaced by second" (cookieA1.cookieId /= cookieA2.cookieId)
                    .&&. counterexample "there should be no events" (null events)
                RevokeSameLabel ->
                  cookiesA === Set.singleton cookieA2.cookieId
                    .&&. cookiesB === Set.singleton cookieB.cookieId
                    .&&. counterexample "first cookie for user A should be replaced by second" (cookieA1.cookieId /= cookieA2.cookieId)
                    .&&. (event <$> events) === [UserEvent UserSessionRefreshSuggested]
                    .&&. (Event.userId <$> events) === [uidA]

  describe "randomConnId" $ do
    it "generates different connection ids" $ do
      let connIds = run . runRandomPure $ replicateM 100 randomConnId
          uniqueConnIds = nub connIds
      length connIds `shouldBe` length uniqueConnIds

newtype Upto4 = Upto4 Int
  deriving newtype (Show, Eq)

instance Arbitrary Upto4 where
  arbitrary = Upto4 <$> elements [0 .. 4]

hashAndUpsertPassword :: (Member PasswordStore r) => UserId -> PlainTextPassword8 -> Sem r ()
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
