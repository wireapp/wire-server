{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Wire.MockInterpreters.AuthenticationSubsystem where

import Data.Time
import Data.UUID qualified as UUID
import Data.ZAuth.Token as ZAuth
import Imports
import Polysemy
import Polysemy.State
import Sodium.Crypto.Sign
import Wire.API.User.Auth
import Wire.AuthenticationSubsystem

data MockAuthenticationState = MockAuthenticationState
  { verificationCodeCalls :: Int,
    reAuthCalls :: Int,
    revokeCookiesCalls :: Int
  }
  deriving stock (Eq, Show)

emptyMockAuthenticationState :: MockAuthenticationState
emptyMockAuthenticationState =
  MockAuthenticationState
    { verificationCodeCalls = 0,
      reAuthCalls = 0,
      revokeCookiesCalls = 0
    }

mockAuthenticationSubsystemInterpreter ::
  (Member (State MockAuthenticationState) r) =>
  InterpreterFor AuthenticationSubsystem r
mockAuthenticationSubsystemInterpreter = interpret \case
  ReauthenticateEither {} -> do
    modify \st -> st {reAuthCalls = st.reAuthCalls + 1}
    pure $ Right ()
  RevokeCookies {} ->
    modify \st -> st {revokeCookiesCalls = st.revokeCookiesCalls + 1}
  EnforceVerificationCodeEither {} -> do
    modify \st -> st {verificationCodeCalls = st.verificationCodeCalls + 1}
    pure $ pure ()
  VerifyUserPasswordError {} -> pure ()
  NewCookie {} -> pure fakeCookie
  NewCookieLimited {} -> pure (Right fakeCookie)
  --
  CreatePasswordResetCode {} -> error "AuthenticationSubsystem.CreatePasswordResetCode not implemented in mock interpreter"
  ResetPassword {} -> error "AuthenticationSubsystem.ResetPassword not implemented in mock interpreter"
  AuthenticateEither {} -> error "AuthenticationSubsystem.AuthenticateEither not implemented in mock interpreter"
  VerifyUserPassword {} -> error "AuthenticationSubsystem.VerifyUserPassword not implemented in mock interpreter"
  VerifyProviderPassword {} -> error "AuthenticationSubsystem.VerifyProviderPassword not implemented in mock interpreter"
  InternalLookupPasswordResetCode {} -> error "AuthenticationSubsystem.InternalLookupPasswordResetCode not implemented in mock interpreter"

fakeCookie :: forall t. (ZAuth.Body t ~ ZAuth.User) => Cookie (Token t)
fakeCookie =
  Cookie
    { cookieId = CookieId 3,
      cookieType = PersistentCookie,
      cookieCreated = now1,
      cookieExpires = now2,
      cookieLabel = Nothing,
      cookieSucc = Nothing,
      cookieValue = val
    }
  where
    val = Token (Signature "") (Header 0 0 0 Nothing) (ZAuth.User UUID.nil Nothing 0)
    Just now1 = parseTimeM True defaultTimeLocale "%Y" "1983"
    Just now2 = parseTimeM True defaultTimeLocale "%Y" "1987"
