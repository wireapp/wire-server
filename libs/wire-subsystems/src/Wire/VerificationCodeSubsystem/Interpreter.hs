module Wire.VerificationCodeSubsystem.Interpreter where

import Data.Code
import Data.RetryAfter (RetryAfter)
import Data.UUID
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Wire.Arbitrary
import Wire.Sem.Random
import Wire.VerificationCode
import Wire.VerificationCodeGen
import Wire.VerificationCodeStore as Store hiding (DeleteCode)
import Wire.VerificationCodeSubsystem

interpretVerificationCodeSubsystem ::
  ( Member VerificationCodeStore r,
    Member Random r,
    Member (Error VerificationCodeSubsystemError) r,
    Member (Input VerificationCodeThrottleTTL) r
  ) =>
  InterpreterFor VerificationCodeSubsystem r
interpretVerificationCodeSubsystem = interpret $ \case
  CreateCode gen scope retries timeout mId -> createCodeImpl gen scope retries timeout mId
  CreateCodeOverwritePrevious gen scope retries timeout mId -> createCodeOverwritePreviousImpl gen scope retries timeout mId
  VerifyCode key scope val -> verifyCodeImpl key scope val
  DeleteCode key scope -> Store.deleteCode key scope
  InternalLookupCode key scope -> Store.lookupCode key scope

newtype VerificationCodeThrottleTTL = VerificationCodeThrottleTTL Word
  deriving (Show, Eq, Arbitrary, Num, Enum, Ord, Real, Integral)

createCodeImpl ::
  ( Member VerificationCodeStore r,
    Member Random r,
    Member (Error VerificationCodeSubsystemError) r,
    Member (Input VerificationCodeThrottleTTL) r
  ) =>
  VerificationCodeGen ->
  Scope ->
  Retries ->
  Timeout ->
  Maybe UUID ->
  Sem r (Either CodeAlreadyExists Code)
createCodeImpl gen scope retries timeout mId =
  lookupCode gen.genKey scope >>= \case
    Just c -> pure . Left $ CodeAlreadyExists c
    Nothing ->
      Right <$> createCodeOverwritePreviousImpl gen scope retries timeout mId

createCodeOverwritePreviousImpl ::
  ( Member VerificationCodeStore r,
    Member Random r,
    Member (Error VerificationCodeSubsystemError) r,
    Member (Input VerificationCodeThrottleTTL) r
  ) =>
  VerificationCodeGen ->
  Scope ->
  Retries ->
  Timeout ->
  Maybe UUID ->
  Sem r Code
createCodeOverwritePreviousImpl gen scope retries timeout mId = do
  code <- generateVerificationCode gen scope retries timeout mId
  maybe (pure code) (throw . VerificationCodeThrottled) =<< insert code

insert :: (Member VerificationCodeStore r, Member (Input VerificationCodeThrottleTTL) r) => Code -> Sem r (Maybe RetryAfter)
insert code = do
  VerificationCodeThrottleTTL ttl <- input
  mRetryAfter <- lookupThrottle (codeKey code) (codeScope code)
  case mRetryAfter of
    Just ra -> pure (Just ra)
    Nothing -> do
      insertThrottle code.codeKey code.codeScope ttl
      insertCode code
      pure Nothing

-- | Lookup and verify the code for the given key and scope
-- against the given value.
verifyCodeImpl :: (Member VerificationCodeStore r) => Key -> Scope -> Value -> Sem r (Maybe Code)
verifyCodeImpl k s v = lookupCode k s >>= maybe (pure Nothing) continue
  where
    continue c
      | codeValue c == v && codeRetries c > 0 = pure (Just c)
      | codeRetries c > 0 = do
          insertCode (c {codeRetries = codeRetries c - 1})
          pure Nothing
      | otherwise = pure Nothing
