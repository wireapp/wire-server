module Wire.ActivationCodeStore.InterpreterSpec (spec) where

import Data.Default
import Data.Map qualified as Map
import Imports
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Wire.API.User.Activation
import Wire.ActivationCodeStore
import Wire.MiniBackend
import Wire.MockInterpreters.ActivationCodeStore

spec :: Spec
spec = do
  describe "ActivationCodeStore effect" $ do
    prop "a code can be looked up" $ \emailKey config ->
      let c = emailKeyToCode emailKey
          localBackend =
            def {activationCodes = Map.singleton emailKey (Nothing, c)}
          result =
            runNoFederationStack localBackend mempty config $
              lookupActivationCode emailKey
       in result === Just (Nothing, c)
    prop "a code not found in the store" $ \emailKey config ->
      let localBackend = def
          result =
            runNoFederationStack localBackend mempty config $
              lookupActivationCode emailKey
       in result === Nothing
    prop "newly added code can be looked up" $ \emailKey mUid config ->
      let c = emailKeyToCode emailKey
          localBackend = def
          (actCode, lookupRes) =
            runNoFederationStack localBackend mempty config $ do
              ac <-
                (.activationCode)
                  <$> newActivationCode emailKey undefined mUid
              (ac,) <$> lookupActivationCode emailKey
       in actCode === c .&&. lookupRes === Just (mUid, c)
