module API.MLS where

import Bilge
import Bilge.Assert
import Brig.Options
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy as LBS
import Data.Domain
import Data.Id
import Data.Qualified
import qualified Data.Text as T
import Imports
import System.Process
import Test.Tasty
import Test.Tasty.HUnit
import Util
import Wire.API.MLS.KeyPackage
import Wire.API.User

tests :: Manager -> Brig -> Opts -> TestTree
tests m b _opts =
  testGroup
    "MLS"
    [ test m "POST /mls/key-packages/self/:client" (testKeyPackageUpload b),
      test m "GET /mls/key-packages/self/:client/count" (testKeyPackageZeroCount b)
    ]

testKeyPackageUpload :: Brig -> Http ()
testKeyPackageUpload brig = do
  u <- userQualifiedId <$> randomUser brig
  c <- randomClient
  uploadKeyPackages u c 5

  count :: KeyPackageCount <-
    responseJsonError
      =<< get
        ( brig . paths ["mls", "key-packages", "self", toByteString' c, "count"]
            . zUser (qUnqualified u)
        )
      <!! const 200 === statusCode
  liftIO $ count @?= 5

testKeyPackageZeroCount :: Brig -> Http ()
testKeyPackageZeroCount brig = do
  u <- userQualifiedId <$> randomUser brig
  c <- randomClient
  count :: KeyPackageCount <-
    responseJsonError
      =<< get
        ( brig . paths ["mls", "key-packages", "self", toByteString' c, "count"]
            . zUser (qUnqualified u)
        )
      <!! const 200 === statusCode
  liftIO $ count @?= 0

--------------------------------------------------------------------------------

uploadKeyPackages :: Qualified UserId -> ClientId -> Int -> Http ()
uploadKeyPackages u c n = do
  let cmd =
        "crypto-cli key-package "
          <> show (qUnqualified u)
          <> ":"
          <> T.unpack (client c)
          <> "@"
          <> T.unpack (domainText (qDomain u))
  kps <- replicateM n . liftIO . fmap (KeyPackageData . LBS.fromStrict) . spawn $ shell cmd
  let upload = KeyPackageUpload kps
  post
    ( brig
        . paths ["mls", "key-packages", "self", toByteString' c]
        . zUser (qUnqualified u)
        . json upload
    )
    !!! const 201 === statusCode
