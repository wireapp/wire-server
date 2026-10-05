module Test.Wire.API.Federation.API.VersionSpec where

import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS
import Data.Set qualified as Set
import Imports
import Network.HTTP.Types qualified as HTTP
import Network.Wai
import Network.Wai.Internal (ResponseReceived (..))
import Test.Hspec
import Wire.API.Federation.Version
import Wire.API.VersionInfo qualified as API

spec :: Spec
spec = describe "Federation API versions" $ do
  it "uses v5 as the development version" $ do
    developmentVersions `shouldBe` Set.fromList [V5]
    expandVersionExp FederationVersionExpDevelopment `shouldBe` Set.fromList [V5]

  it "does not advertise disabled versions" $ do
    (versionInfoFor (supportedVersions Set.\\ developmentVersions)).vinfoSupported
      `shouldBe` [0, 1, 2, 3, 4]

  it "decodes version information with filtered legacy versions" $ do
    let info = versionInfoFor (Set.fromList [V0, V1, V2, V3, V4])
    fmap vinfoSupported (Aeson.decode (Aeson.encode info) :: Maybe VersionInfo)
      `shouldBe` Just [0, 1, 2, 3, 4]

  it "does not allow disabling legacy versions at runtime" $ do
    Aeson.decode "0" `shouldBe` (Nothing :: Maybe FederationVersionExp)
    Aeson.decode "1" `shouldBe` (Nothing :: Maybe FederationVersionExp)

  it "keeps explicitly selected versions available" $ do
    expandVersionExp (FederationVersionExpConst V4) `shouldBe` Set.fromList [V4]

  it "allows an enabled development version" $ do
    responseStatus <$> runMiddleware Set.empty (requestFor 5)
      `shouldReturn` HTTP.status200

  it "rejects a disabled development version" $ do
    responseStatus <$> runMiddleware developmentVersions (requestFor 5)
      `shouldReturn` HTTP.status404

  it "rejects an unsupported version" $ do
    responseStatus <$> runMiddleware Set.empty (requestFor 99)
      `shouldReturn` HTTP.status404

requestFor :: Int -> Request
requestFor version =
  defaultRequest
    { pathInfo = ["federation", "api-version"],
      requestHeaders = [(API.versionHeader, BS.pack (show version))]
    }

runMiddleware :: Set Version -> Request -> IO Response
runMiddleware disabled req = do
  result <- newIORef Nothing
  let app _ respond = respond (responseLBS HTTP.status200 [] "")
      save response = writeIORef result (Just response) $> ResponseReceived
  void $ federationVersionMiddleware disabled app req save
  fromMaybe (error "middleware did not produce a response") <$> readIORef result
