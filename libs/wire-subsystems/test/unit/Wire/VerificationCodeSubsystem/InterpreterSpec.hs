module Wire.VerificationCodeSubsystem.InterpreterSpec where

import Data.Time
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.State
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Wire.MockInterpreters
import Wire.Sem.Now
import Wire.Sem.Random
import Wire.VerificationCode
import Wire.VerificationCodeGen
import Wire.VerificationCodeStore
import Wire.VerificationCodeSubsystem as Subsystem
import Wire.VerificationCodeSubsystem.Interpreter

spec :: Spec
spec = describe "Wire.VerificationCodeSubsystem.Interpreter" $ do
  describe "createCode/verifyCode" $ do
    prop "should be able to create and verify codes" $
      \gen scope retries (abs -> timeout) mId throttle ->
        let eitherProp =
              runDependencies throttle
                . interpretVerificationCodeSubsystem
                $ do
                  c <- createCode gen scope retries timeout mId
                  case c of
                    Left (CodeAlreadyExists code) -> pure $ unexpectedCodeAlreadyExists code
                    Right code -> do
                      mCode <- verifyCode gen.genKey scope code.codeValue
                      pure $ retries > 0 ==> mCode === Just code
         in assertRightProp eitherProp

    prop "should only allow verification with the same scope" $
      \gen scope retries (abs -> timeout) mId throttle arbitraryScope ->
        let eitherProp =
              runDependencies throttle
                . interpretVerificationCodeSubsystem
                $ do
                  c <- createCode gen scope retries timeout mId
                  case c of
                    Left (CodeAlreadyExists code) -> pure $ unexpectedCodeAlreadyExists code
                    Right code -> do
                      mCode <- verifyCode gen.genKey arbitraryScope code.codeValue
                      pure $ retries > 0 && arbitraryScope /= scope ==> mCode === Nothing
         in assertRightProp eitherProp

    prop "should only allow verification with correct value" $
      \gen scope retries (abs -> timeout) mId throttle arbitraryVal ->
        let eitherProp =
              runDependencies throttle
                . interpretVerificationCodeSubsystem
                $ do
                  c <- createCode gen scope retries timeout mId
                  case c of
                    Left (CodeAlreadyExists code) -> pure $ unexpectedCodeAlreadyExists code
                    Right code -> do
                      mCode <- verifyCode gen.genKey scope arbitraryVal
                      pure $ retries > 0 && arbitraryVal /= code.codeValue ==> mCode === Nothing
         in assertRightProp eitherProp

    prop "should allow retries" $
      \gen scope retries (abs -> timeout) mId throttle arbitraryVal ->
        let eitherProp =
              runDependencies throttle
                . interpretVerificationCodeSubsystem
                $ do
                  c <- createCode gen scope retries timeout mId
                  case c of
                    Left (CodeAlreadyExists code) -> pure $ unexpectedCodeAlreadyExists code
                    Right code -> do
                      codesWithArbitraryVal <-
                        catMaybes
                          <$> replicateM
                            (fromIntegral retries - 1)
                            (verifyCode gen.genKey scope arbitraryVal)
                      mCodeWithCorrectVal <- verifyCode gen.genKey scope code.codeValue
                      pure $
                        retries > 1 && arbitraryVal /= code.codeValue ==>
                          codesWithArbitraryVal === []
                            .&&. mCodeWithCorrectVal === Just (code {codeRetries = 1})
         in assertRightProp eitherProp

    prop "should only allow given number of retries" $
      \gen scope retries (abs -> timeout) mId throttle arbitraryVal ->
        let eitherProp =
              runDependencies throttle
                . interpretVerificationCodeSubsystem
                $ do
                  c <- createCode gen scope retries timeout mId
                  case c of
                    Left (CodeAlreadyExists code) -> pure $ unexpectedCodeAlreadyExists code
                    Right code -> do
                      codesWithArbitraryVal <-
                        catMaybes
                          <$> replicateM
                            (fromIntegral retries)
                            (verifyCode gen.genKey scope arbitraryVal)
                      mCodeWithCorrectVal <- verifyCode gen.genKey scope code.codeValue
                      pure $
                        retries > 0 && arbitraryVal /= code.codeValue ==>
                          codesWithArbitraryVal === []
                            .&&. mCodeWithCorrectVal === Nothing
         in assertRightProp eitherProp

  describe "createCode" $ do
    prop "should only allow one code at a time per (key, scope)" $ do
      \gen scope retries (abs -> timeout) mId throttle ->
        let eitherProp =
              runDependencies throttle
                . interpretVerificationCodeSubsystem
                $ do
                  c1 <- createCode gen scope retries timeout mId
                  case c1 of
                    Left (CodeAlreadyExists code) -> pure $ unexpectedCodeAlreadyExists code
                    Right code -> do
                      c2 <- createCode gen scope retries timeout mId
                      pure $ c2 === Left (CodeAlreadyExists code)
         in assertRightProp eitherProp

  describe "createCode/deleteCode/verifyCode" $ do
    prop "should not allow verification using a deleted code" $ do
      \gen scope retries (abs -> timeout) mId throttle ->
        let eitherProp =
              runDependencies throttle
                . interpretVerificationCodeSubsystem
                $ do
                  c <- createCode gen scope retries timeout mId
                  case c of
                    Left (CodeAlreadyExists code) -> pure $ unexpectedCodeAlreadyExists code
                    Right code -> do
                      Subsystem.deleteCode gen.genKey scope
                      mCode <- verifyCode gen.genKey scope code.codeValue
                      pure $ mCode === Nothing
         in assertRightProp eitherProp

  describe "createCodeOverwritePrevious/verifyCode" $ do
    prop "should allow creating code for the same scope and key, making previous code invalid" $ do
      \gen scope retries (abs -> timeout) mId throttle ->
        let eitherProp =
              runDependencies throttle
                . interpretVerificationCodeSubsystem
                $ do
                  code1 <- createCodeOverwritePrevious gen scope retries timeout mId
                  passTime (fromIntegral throttle + 1)
                  code2 <- createCodeOverwritePrevious gen scope retries timeout mId
                  mCode1 <- verifyCode gen.genKey scope code1.codeValue
                  mCode2 <- verifyCode gen.genKey scope code2.codeValue
                  pure $ retries > 1 ==> mCode1 === Nothing .&&. mCode2 === Just (code2 {codeRetries = retries - 1})
         in assertRightProp eitherProp

    prop "should throttle creating codes " $ do
      \gen scope retries (abs -> timeout) mId ((+ 1) -> throttle) ->
        let eitherProp =
              runDependencies throttle
                . interpretVerificationCodeSubsystem
                $ do
                  code <- createCodeOverwritePrevious gen scope retries timeout mId
                  mErrThrottled1 <- catchExpectedError $ createCodeOverwritePrevious gen scope retries timeout mId
                  mCode1 <- verifyCode gen.genKey scope code.codeValue
                  Subsystem.deleteCode gen.genKey scope
                  mErrThrottled2 <- catchExpectedError $ createCodeOverwritePrevious gen scope retries timeout mId
                  let expectedErr = Just $ VerificationCodeThrottled $ fromIntegral throttle
                  pure $
                    mErrThrottled1 === expectedErr
                      .&&. mErrThrottled2 === expectedErr
                      .&&. (retries > 1 ==> mCode1 === Just code)
         in assertRightProp eitherProp

  describe "internalLookupCode" $ do
    prop "should allow looking up code by scope and key" $ do
      \gen scope retries (abs -> timeout) mId throttle ->
        let eitherProp =
              runDependencies throttle
                . interpretVerificationCodeSubsystem
                $ do
                  code1 <- createCodeOverwritePrevious gen scope retries timeout mId
                  lookedUpCode <- internalLookupCode gen.genKey scope
                  pure $ lookedUpCode === Just code1
         in assertRightProp eitherProp

runDependencies :: VerificationCodeThrottleTTL -> Sem '[Input VerificationCodeThrottleTTL, VerificationCodeStore, Now, State UTCTime, Random, Error e] a -> Either e a
runDependencies throttle =
  run
    . runError
    . runRandomPure
    . evalState defaultTime
    . interpretNowAsState
    . runInMemoryVerificationCodeStore
    . runInputConst throttle

assertRightProp :: (Show e) => Either e Property -> Property
assertRightProp = either (\e -> counterexample ("unexpected error: " <> show e) False) id

unexpectedCodeAlreadyExists :: Code -> Property
unexpectedCodeAlreadyExists code = counterexample ("code shouldn't already exist, but exists: " <> show code) False
