module API.Federation (tests) where

import API.Util
import Bilge
import Bilge.Assert
import CargoHold.API.V3 (randToken)
import Conduit
import Control.Lens
import Data.Id
import Data.Qualified
import Data.UUID.V4
import Imports
import qualified Network.HTTP.Types as HTTP
import Test.Tasty
import Test.Tasty.HUnit
import TestSetup
import Wire.API.Asset
import Wire.API.Federation.API.Cargohold

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "API Federation"
    [ testGroup
        "get-asset"
        [ test s "private asset is available" (testGetAssetAvailable False),
          test s "public asset is available" (testGetAssetAvailable True),
          test s "not available" testGetAssetNotAvailable,
          test s "wrong token" testGetAssetWrongToken
        ],
      testGroup
        "stream-asset"
        [test s "streaming large asset" testLargeAsset]
    ]

testGetAssetAvailable :: Bool -> TestSignature ()
testGetAssetAvailable isPublicAsset c = do
  -- Initial upload
  let bdy = (applicationOctetStream, "Hello World")
      settings =
        defAssetSettings
          & set setAssetRetention (Just AssetVolatile)
          & set setAssetPublic isPublicAsset
  uid <- liftIO $ Id <$> nextRandom
  ast :: Asset <-
    responseJsonError
      =<< uploadSimple (c . path "/assets/v3") uid settings bdy
      <!! const 201 === statusCode

  -- Call get-asset federation API
  let tok = view assetToken ast
  let key = view assetKey ast
  let ga =
        GetAsset
          { gaUser = uid,
            gaToken = tok,
            gaKey = qUnqualified key
          }
  ok <-
    fmap gaAvailable . responseJsonError
      =<< post (c . path "/federation/get-asset" . json ga)
      <!! const 200 === statusCode

  -- check that asset is available
  liftIO $ ok @?= True

testGetAssetNotAvailable :: TestSignature ()
testGetAssetNotAvailable c = do
  uid <- liftIO $ Id <$> nextRandom
  token <- randToken

  assetId <- liftIO $ Id <$> nextRandom
  let key = AssetKeyV3 assetId AssetPersistent
  let ga =
        GetAsset
          { gaUser = uid,
            gaToken = Just token,
            gaKey = key
          }
  ok <-
    fmap gaAvailable . responseJsonError
      =<< post (c . path "/federation/get-asset" . json ga)
      <!! const 200 === statusCode

  -- check that asset is not available
  liftIO $ ok @?= False

testGetAssetWrongToken :: TestSignature ()
testGetAssetWrongToken c = do
  -- Initial upload
  let bdy = (applicationOctetStream, "Hello World")
      settings = defAssetSettings & set setAssetRetention (Just AssetVolatile)
  uid <- liftIO $ Id <$> nextRandom
  ast :: Asset <-
    responseJsonError
      =<< uploadSimple (c . path "/assets/v3") uid settings bdy
      <!! const 201 === statusCode

  -- Call get-asset federation API with wrong (random) token
  tok <- randToken
  let key = view assetKey ast
  let ga =
        GetAsset
          { gaUser = uid,
            gaToken = Just tok,
            gaKey = qUnqualified key
          }
  ok <-
    fmap gaAvailable . responseJsonError
      =<< post (c . path "/federation/get-asset" . json ga)
      <!! const 200 === statusCode

  -- check that asset is not available
  liftIO $ ok @?= False

testLargeAsset :: TestSignature ()
testLargeAsset c = do
  -- Initial upload
  let size = 1024 * 1024
      settings =
        defAssetSettings
          & set setAssetRetention (Just AssetVolatile)
  uid <- liftIO $ Id <$> nextRandom
  ast :: Asset <-
    responseJsonError
      =<< uploadRandom (c . path "/assets/v3") uid settings applicationOctetStream size
      <!! const 201 === statusCode

  -- Call get-asset federation API
  let tok = view assetToken ast
  let key = view assetKey ast
  let ga =
        GetAsset
          { gaUser = uid,
            gaToken = tok,
            gaKey = qUnqualified key
          }
  http empty (method HTTP.POST . c . path "/federation/stream-asset" . json ga) $ \resp -> do
    statusCode resp @?= 200
    -- check that the first chunk is received
    chunk <- responseBody resp
    print chunk
