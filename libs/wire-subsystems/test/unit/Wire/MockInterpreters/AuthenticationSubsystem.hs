module Wire.MockInterpreters.AuthenticationSubsystem where

import Imports
import Polysemy
import Polysemy.State
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
  _ -> error "mockAuthenticationSubsystemInterpreter: implement on demand"
