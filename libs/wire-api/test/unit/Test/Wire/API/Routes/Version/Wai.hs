module Test.Wire.API.Routes.Version.Wai where

import Data.Proxy
import qualified Data.Set as Set
import Data.String.Conversions
import Data.Text as T
import Imports
import Network.HTTP.Types.Status (status200, status400)
import Network.Wai
import Servant.API
import Servant.Server
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.Matcher
import Test.Tasty
import Test.Tasty.Hspec
import Wire.API.Routes.Version
import Wire.API.Routes.Version.Wai

implicitVersion :: Text
implicitVersion = "0"

sndGoodVersion :: Text
sndGoodVersion = "2"

disabledVersion :: Text
disabledVersion = T.filter isDigit . cs $ toHeader disabledVersionTyped

disabledVersionTyped :: Version
disabledVersionTyped = V3

unknownVersion :: Text
unknownVersion = "100"

tests :: IO TestTree
tests =
  testSpec "versionMiddleware" . with testApp $ do
    mkTest Nothing Nothing ("good", 200)
    mkTest Nothing (Just implicitVersion) ("mismatch: (Nothing,Just 0)", 400)
    mkTest Nothing (Just sndGoodVersion) ("mismatch: (Nothing,Just 2)", 400)
    mkTest (Just implicitVersion) (Just implicitVersion) ("good", 200)
    mkTest (Just sndGoodVersion) (Just sndGoodVersion) ("good", 200)
    mkTest (Just disabledVersion) (Just disabledVersion) (errmsg disabledVersion, 404)
    mkTest (Just unknownVersion) (Just unknownVersion) (errmsg unknownVersion, 404)
  where
    errmsg v = "{\"code\":404,\"label\":\"unsupported-version\",\"message\":\"Version v" <> cs v <> " is not supported\"}"

mkTest :: Maybe Text -> Maybe Text -> (LByteString, Int) -> SpecWith (st, Application)
mkTest mv1 mv2 (msg, status) =
  it ("GET " <> cs path <> " => " <> show (msg, status)) $ do
    get path `shouldRespondWith` ResponseMatcher status [] (bodyEquals msg)
  where
    path :: ByteString
    path = cs $ maybe "" ("/v" <>) mv1 <> "/check-version" <> maybe "" ("?version=" <>) mv2

type TestAPI = "check-version" :> QueryParam "version" Int :> Raw

testApp :: IO Application
testApp = pure $ versionMiddleware (Set.singleton disabledVersionTyped) (serve (Proxy @TestAPI) testHandler)

testHandler :: Server TestAPI
testHandler mVersionNumber = Tagged $ \req cont ->
  cont $
    let headerVersion = lookup "X-Wire-API-Version" (requestHeaders req)
     in if headerVersion == (cs . show <$> mVersionNumber)
          then responseLBS status200 [] "good"
          else responseLBS status400 [] (cs $ "mismatch: " <> show (headerVersion, mVersionNumber))
