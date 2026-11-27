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

module Wire.ActivationCodeStore.InterpreterSpec (spec) where

import Data.Default
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Text.Ascii qualified as Ascii
import Imports
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Printf (printf)
import Wire.API.User.Activation
import Wire.ActivationCodeStore
import Wire.ActivationCodeVerificationStore
import Wire.MiniBackend
import Wire.MockInterpreters.ActivationCodeStore
import Wire.UserKeyStore

spec :: Spec
spec = do
  describe "ActivationCodeStore effect" $ do
    prop "a code can be looked up" $ \emailKey config ->
      let c = emailKeyToCode emailKey
          localBackend =
            def {activationCodes = Map.singleton emailKey (Nothing, c, maxAttempts)}
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
    prop "a correct code verifies" $ \email config ->
      let key = mkEmailKey email
          result =
            runNoFederationStack def mempty config $
              interpretActivationCodeVerificationStore $ do
                a <- newActivationCode key undefined Nothing
                verifyActivationCode (a.activationKey) (a.activationCode)
       in result === Just (key, Nothing)
    prop "a wrong code fails to verify" $ \email config ->
      let key = mkEmailKey email
          result =
            runNoFederationStack def mempty config $
              interpretActivationCodeVerificationStore $ do
                a <- newActivationCode key undefined Nothing
                verifyActivationCode (a.activationKey) (bumpCode (a.activationCode))
       in result === Nothing
    prop "the correct code verifies after maxAttempts wrong codes" $ \email config ->
      let key = mkEmailKey email
          (wrongResults, result, stored) =
            runNoFederationStack def mempty config $
              interpretActivationCodeVerificationStore $ do
                a <- newActivationCode key undefined Nothing
                ws <-
                  replicateM (fromIntegral maxAttempts) $
                    verifyActivationCode a.activationKey (bumpCode a.activationCode)
                r <- verifyActivationCode a.activationKey a.activationCode
                (ws,r,) <$> lookupActivationCode key
       in wrongResults === replicate (fromIntegral maxAttempts) Nothing
            .&&. result === Just (key, Nothing)
            .&&. isJust stored === True
    prop "one more wrong code than maxAttempts deletes the key" $ \email config ->
      let key = mkEmailKey email
          (result, stored) =
            runNoFederationStack def mempty config $
              interpretActivationCodeVerificationStore $ do
                a <- newActivationCode key undefined Nothing
                replicateM_ (fromIntegral maxAttempts + 1) $
                  verifyActivationCode a.activationKey (bumpCode a.activationCode)
                r <- verifyActivationCode a.activationKey a.activationCode
                (r,) <$> lookupActivationCode key
       in result === Nothing .&&. stored === Nothing
  where
    -- a code that is different from the given one
    bumpCode :: ActivationCode -> ActivationCode
    bumpCode c =
      ActivationCode . Ascii.unsafeFromText . Text.pack . printf "%06d" $
        (read @Int (Text.unpack (Ascii.toText (fromActivationCode c))) + 1) `mod` 1000000
