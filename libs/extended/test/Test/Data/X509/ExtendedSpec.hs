module Test.Data.X509.ExtendedSpec where

import Data.ByteString qualified as BS
import Data.PEM
import Data.String.Conversions
import Data.X509
import Data.X509.Extended
import Imports
import Test.Hspec

spec :: Spec
spec =
  describe "Data.X509.Extended" $ do
    describe "certToString" $ do
      it "should render a representative string of a certificate from stars' Keyloak" $ do
        let pemFilePath = "test/data/" <> "sven-test.pem"
            expected = "Issuer: CN=sven-test; Subject: CN=sven-test; SHA256 Fingerprint: 84:73:C5:D1:5A:36:7B:E7:00:3F:C5:1B:F6:84:90:5B:21:77:DA:22:FC:3D:8B:94:A2:97:0D:C1:8F:26:F7:6B"
        checkDecodingWithPEMFile pemFilePath expected

      it "should render a representative string of a certificate from unit test data (saml2-web-sso)" $ do
        let pemFilePath = "test/data/" <> "test-cert.pem"
            expected = "Issuer: CN=accounts.accesscontrol.windows.net; Subject: CN=accounts.accesscontrol.windows.net; SHA256 Fingerprint: A5:0B:76:1A:A3:11:8E:78:CF:2C:75:95:6B:6A:59:D1:85:4E:EA:DE:20:7C:C4:AF:48:B7:7F:A7:90:48:33:DB"
        checkDecodingWithPEMFile pemFilePath expected

checkDecodingWithPEMFile :: FilePath -> String -> IO ()
checkDecodingWithPEMFile pemFilePath expected = do
  -- sanity check if the file even exists
  exists <- doesFileExist pemFilePath
  exists `shouldBe` True

  file <- BS.readFile pemFilePath
  let decoded :: SignedCertificate = either error id $ do
        pemBS <- pemContent . fromMaybe (error "Empty PEM list") . listToMaybe <$> pemParseBS file
        decodeSignedCertificate pemBS

  certToString decoded `shouldBe` expected
