{-# LANGUAGE DerivingVia #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.User.Client.Info where

-- TODO: explicit exports
-- TODO: make this a separate package

import Control.Monad.Except
import qualified Data.Binary.Builder as BLD
import qualified Data.ByteString.Conversion as BSC
import qualified Data.CaseInsensitive as CI
import qualified Data.SemVer as SV
import qualified Data.SemVer.Constraint as SVC
import Data.String.Conversions (cs)
import Imports
import Servant.API
import Test.Hspec
import Test.QuickCheck
import Wire.API.Arbitrary (GenericUniform (..))

data Platform = Android | IOS | WebApp | TeamSettings | AccountPages
  deriving stock (Eq, Show, Ord, Bounded, Enum, Generic)
  deriving (Arbitrary) via (GenericUniform Platform)

instance BSC.FromByteString Platform where
  parser = BSC.parser >>= maybe err pure . parsePlatform
    where
      err :: MonadFail m => m a
      err = fail ("invalid Platform, must be one of " <> show [(minBound :: Platform) ..])

      parsePlatform :: Text -> Maybe Platform
      parsePlatform txt = case CI.mk txt of
        "android" -> Just Android
        "ios" -> Just IOS
        "webapp" -> Just WebApp
        "team-settings" -> Just TeamSettings
        "account-pages" -> Just AccountPages
        _ -> Nothing

instance BSC.ToByteString Platform where
  builder Android = BLD.fromByteString "android"
  builder IOS = BLD.fromByteString "ios"
  builder WebApp = BLD.fromByteString "webapp"
  builder TeamSettings = BLD.fromByteString "team-settings"
  builder AccountPages = BLD.fromByteString "account-pages"

type ClientInfo = Header "X-Client-Platform" Platform :> Header "X-Client-Version" SV.Version

-- | FUTUREWORK: write a function that compiles this into something that doesn't require
-- sanity checks and constraint parsing, and call that function using TH to avoid the runtime
-- cost and uncertainty.  (even though the uncertainty should be covered if we have any tests
-- whatsoever.)
newtype Constraints = Constraints [(Platform, [Text])]

data ClientInfoError
  = IncompletePlatformList -- FUTUREWORK: should be a static error, see comment at 'Constraints'
  | MissingPlatform
  | MissingSemVer
  | InvalidPlatform String
  | InvalidSemVer String
  | InvalidConstraint Text String -- FUTUREWORK: should be a static error, see comment at 'Constraints'
  | ConstraintViolation Text
  deriving stock (Eq, Show, Ord, Generic)
  deriving (Arbitrary) via (GenericUniform ClientInfoError)

guardClientInfo :: forall m. (MonadError ClientInfoError m) => Constraints -> Maybe Text -> Maybe Text -> m ()
guardClientInfo (Constraints constraints) mbPlatform mbVersion = do
  unless (sort (fst <$> constraints) == [minBound ..]) $ do
    throwError IncompletePlatformList

  platform :: Platform <- do
    maybe
      (throwError MissingPlatform)
      (either (throwError . InvalidPlatform) pure . BSC.runParser' BSC.parser . cs)
      mbPlatform

  version :: SV.Version <-
    maybe
      (throwError MissingSemVer)
      (either (throwError . InvalidSemVer) pure . SV.fromText)
      mbVersion

  case lookup platform constraints of
    Just pcs -> do
      forM_ pcs $ \pc -> do
        let check cstr = unless (SVC.satisfies version cstr) $ do
              throwError $ ConstraintViolation pc
        either (throwError . InvalidConstraint pc) check (SVC.fromText pc)
    Nothing -> do
      error "impossible (see assertion above)."

----------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Platform" $ do
    it "parses case insensitively" $ do
      BSC.fromByteString "android" `shouldBe` Just Android
      BSC.fromByteString "anDRoid" `shouldBe` Just Android
      BSC.fromByteString "anDRoid--" `shouldBe` (Nothing :: Maybe Platform)

  describe "guardClientInfo" $ do
    it "requires platform list to be complete" $
      guardClientInfo (Constraints ((,[]) <$> take 2 [minBound ..])) (Just "android") (Just "1.2.3")
        `shouldBe` Left IncompletePlatformList

    it "fails on missing platform" $
      guardClientInfo (Constraints ((,[">=1.2.3"]) <$> [minBound ..])) Nothing (Just "1.2.3")
        `shouldBe` Left MissingPlatform

    it "fails on missing version" $
      guardClientInfo (Constraints ((,[">=1.2.3"]) <$> [minBound ..])) (Just "android") Nothing
        `shouldBe` Left MissingSemVer

    it "fails on invalid platform" $
      guardClientInfo (Constraints ((,[">=1.2.3"]) <$> [minBound ..])) (Just "@batman#") (Just "1.2.3")
        `shouldBe` Left (InvalidPlatform "Failed reading: invalid Platform, must be one of [Android,IOS,WebApp,TeamSettings,AccountPages]")

    it "fails on invalid version" $
      guardClientInfo (Constraints ((,[">=1.2.3"]) <$> [minBound ..])) (Just "android") (Just "@batman#")
        `shouldBe` Left (InvalidSemVer "Failed reading: takeWhile1")

    it "fails on invalid constraint" $
      guardClientInfo (Constraints ((,["@BAD"]) <$> [minBound ..])) (Just "android") (Just "1.2.3")
        `shouldBe` Left (InvalidConstraint "@BAD" "Failed reading: empty")

    it "fails on constraint violation" $
      guardClientInfo (Constraints ((,[">=1.2.9"]) <$> [minBound ..])) (Just "android") (Just "1.2.3")
        `shouldBe` Left (ConstraintViolation ">=1.2.9")

    it "succeeds if all constraints for platform are satisfied" $ do
      let constraints =
            Constraints
              [ (Android, [">=1.2.1", "<4.1.0"]),
                (IOS, [">=9.9.9"]),
                (WebApp, [">=9.9.9"]),
                (TeamSettings, [">=9.9.9"]),
                (AccountPages, [">=9.9.9"])
              ]
      guardClientInfo constraints (Just "android") (Just "1.2.3")
        `shouldBe` Right ()

    it "succeeds if constraint set is empty" $ do
      let constraints =
            Constraints
              [ (Android, []),
                (IOS, [">=9.9.9"]),
                (WebApp, [">=9.9.9"]),
                (TeamSettings, [">=9.9.9"]),
                (AccountPages, [">=9.9.9"])
              ]
      guardClientInfo constraints (Just "android") (Just "1.2.3")
        `shouldBe` Right ()

{-
----------------------------------------------------------------------

type API = "test" :> Constraints :> Get '[JSON] Bool

instance
  Servant.HasServer api '[] {- FUTUREWORK: constaint that tests that Constraints lists all platforms -} =>
  Servant.HasServer (Constraints :> api) '[]
  where
  type ServerT (Constraints :> api) (m :: * -> *) = ClientInfo :> Servant.ServerT api m

  route = undefined
  hoistServerWithContext Proxy Proxy _nt = undefined

api :: Servant.Server API
api = undefined

main :: IO ()
main = do
  hspec spec

{-
  let app :: Application
      app = Servant.serve (Proxy @API) api
  run 8080 app
-}
-}
