module API.MLS where

import Bilge
import Bilge.Assert
import Brig.Options
import Data.Aeson (object, toJSON, (.=))
import Data.ByteString.Conversion
import Data.Domain
import Data.Id
import Data.Json.Util
import qualified Data.Map as Map
import Data.Qualified
import qualified Data.Set as Set
import qualified Data.Text as T
import Imports
import System.Process
import Test.Tasty
import Test.Tasty.HUnit
import UnliftIO.Temporary
import Util
import Web.HttpApiData
import Wire.API.MLS.Credential
import Wire.API.MLS.KeyPackage
import Wire.API.User
import Wire.API.User.Client

tests :: Manager -> Brig -> Opts -> TestTree
tests m b _opts =
  testGroup
    "MLS"
    [ test m "POST /mls/key-packages/self/:client" (testKeyPackageUpload b),
      test m "POST /mls/key-packages/self/:client (no public keys)" (testKeyPackageUploadNoKey b),
      test m "GET /mls/key-packages/self/:client/count" (testKeyPackageZeroCount b),
      test m "GET /mls/key-packages/claim/:domain/:user" (testKeyPackageClaim b)
    ]

testKeyPackageUpload :: Brig -> Http ()
testKeyPackageUpload brig = do
  u <- userQualifiedId <$> randomUser brig
  c <- createClient brig u 0
  withSystemTempFile "store.db" $ \store _ ->
    uploadKeyPackages brig store SetKey u c 5

  count <- getKeyPackageCount brig u c
  liftIO $ count @?= 5

testKeyPackageUploadNoKey :: Brig -> Http ()
testKeyPackageUploadNoKey brig = do
  u <- userQualifiedId <$> randomUser brig
  c <- createClient brig u 0
  withSystemTempFile "store.db" $ \store _ ->
    uploadKeyPackages brig store DontSetKey u c 5

  count <- getKeyPackageCount brig u c
  liftIO $ count @?= 0

testKeyPackageZeroCount :: Brig -> Http ()
testKeyPackageZeroCount brig = do
  u <- userQualifiedId <$> randomUser brig
  c <- randomClient
  count <- getKeyPackageCount brig u c
  liftIO $ count @?= 0

testKeyPackageClaim :: Brig -> Http ()
testKeyPackageClaim brig = do
  -- setup a user u with two clients c1 and c2
  u <- userQualifiedId <$> randomUser brig
  [c1, c2] <- for [0, 1] $ \i -> do
    c <- createClient brig u i
    -- upload 3 key packages for each client
    withSystemTempFile "store.db" $ \store _ ->
      uploadKeyPackages brig store SetKey u c 3
    pure c

  -- claim packages for both clients of u
  u' <- userQualifiedId <$> randomUser brig
  bundle <-
    responseJsonError
      =<< post
        ( brig
            . paths ["mls", "key-packages", "claim", toByteString' (qDomain u), toByteString' (qUnqualified u)]
            . zUser (qUnqualified u')
        )
        <!! const 200 === statusCode
  liftIO $ Set.map (\e -> (kpbeUser e, kpbeClient e)) (kpbEntries bundle) @?= Set.fromList [(u, c1), (u, c2)]

  -- check that we have one fewer key package now
  for_ [c1, c2] $ \c -> do
    count :: KeyPackageCount <-
      responseJsonError
        =<< get
          ( brig . paths ["mls", "key-packages", "self", toByteString' c, "count"]
              . zUser (qUnqualified u)
          )
        <!! const 200 === statusCode
    liftIO $ count @?= 2

  -- check that the package refs are correctly mapped
  for_ (kpbEntries bundle) $ \e -> do
    cid <-
      responseJsonError
        =<< get (brig . paths ["i", "mls", "key-packages", toHeader (kpbeRef e)])
        <!! const 200 === statusCode
    liftIO $ do
      ciDomain cid @?= qDomain u
      ciUser cid @?= qUnqualified u
      ciClient cid @?= kpbeClient e

--------------------------------------------------------------------------------

data SetKey = SetKey | DontSetKey
  deriving (Eq)

getKeyPackageCount :: Brig -> Qualified UserId -> ClientId -> Http KeyPackageCount
getKeyPackageCount brig u c =
  responseJsonError
    =<< get
      ( brig . paths ["mls", "key-packages", "self", toByteString' c, "count"]
          . zUser (qUnqualified u)
      )
    <!! const 200 === statusCode

createClient :: Brig -> Qualified UserId -> Int -> Http ClientId
createClient brig u i =
  fmap clientId $
    responseJsonError
      =<< addClient
        brig
        (qUnqualified u)
        (defNewClient PermanentClientType [somePrekeys !! i] (someLastPrekeys !! i))
      <!! const 201 === statusCode

uploadKeyPackages :: Brig -> FilePath -> SetKey -> Qualified UserId -> ClientId -> Int -> Http ()
uploadKeyPackages brig store sk u c n = do
  let cmd0 = ["crypto-cli", "--store", store, "--enc-key", "test"]
      clientId =
        show (qUnqualified u)
          <> ":"
          <> T.unpack (client c)
          <> "@"
          <> T.unpack (domainText (qDomain u))
  kps <-
    replicateM n . liftIO . spawn . shell . unwords $
      cmd0 <> ["key-package", clientId]
  when (sk == SetKey) $
    do
      pk <-
        liftIO . spawn . shell . unwords $
          cmd0 <> ["public-key", clientId]
      put
        ( brig
            . paths ["clients", toByteString' c]
            . zUser (qUnqualified u)
            . json defUpdateClient {updateClientMLSPublicKeys = Map.fromList [(Ed25519, pk)]}
        )
      !!! const 200 === statusCode
  let upload = object ["key_packages" .= toJSON (map Base64ByteString kps)]
  post
    ( brig
        . paths ["mls", "key-packages", "self", toByteString' c]
        . zUser (qUnqualified u)
        . json upload
    )
    !!! const (case sk of SetKey -> 201; DontSetKey -> 400) === statusCode
