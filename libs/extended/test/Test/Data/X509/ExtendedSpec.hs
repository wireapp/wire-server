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
      it "should render a representative string of a certificate from stars' Keycloak" $ do
        let pemFilePath = "test/data/" <> "sven-test.pem"
            expected = "Issuer: CN=sven-test; Subject: CN=sven-test; SHA1 Fingerprint: F4:A2:73:D7:B7:2E:EA:66:E1:CB:81:E9:58:BC:1A:E9:CF:3C:95:C4"
        checkDecodingWithPEMFile pemFilePath expected

      it "should render a representative string of a certificate from unit test data (saml2-web-sso)" $ do
        let pemFilePath = "test/data/" <> "test-cert.pem"
            expected = "Issuer: CN=accounts.accesscontrol.windows.net; Subject: CN=accounts.accesscontrol.windows.net; SHA1 Fingerprint: 15:28:A6:B8:5A:C5:36:80:B4:B0:95:C6:9A:FD:77:9C:D6:5C:78:37"
        checkDecodingWithPEMFile pemFilePath expected

    describe "certDescription" $ do
      it "should extract certificate description from stars' Keycloak certificate" $ do
        let pemFilePath = "test/data/" <> "sven-test.pem"
            expected =
              CertDescription
                { fingerprintAlgorithm = "SHA1",
                  fingerprint = "F4:A2:73:D7:B7:2E:EA:66:E1:CB:81:E9:58:BC:1A:E9:CF:3C:95:C4",
                  subject = "CN=sven-test",
                  issuer = "CN=sven-test"
                }
        checkCertDescriptionWithPEMFile pemFilePath expected

      it "should extract certificate description from unit test data (saml2-web-sso)" $ do
        let pemFilePath = "test/data/" <> "test-cert.pem"
            expected =
              CertDescription
                { fingerprintAlgorithm = "SHA1",
                  fingerprint = "15:28:A6:B8:5A:C5:36:80:B4:B0:95:C6:9A:FD:77:9C:D6:5C:78:37",
                  subject = "CN=accounts.accesscontrol.windows.net",
                  issuer = "CN=accounts.accesscontrol.windows.net"
                }
        checkCertDescriptionWithPEMFile pemFilePath expected

checkDecodingWithPEMFile :: FilePath -> String -> IO ()
checkDecodingWithPEMFile pemFilePath expected = do
  cert <- loadSignedCertificate pemFilePath
  certToString cert `shouldBe` expected

checkCertDescriptionWithPEMFile :: FilePath -> CertDescription -> IO ()
checkCertDescriptionWithPEMFile pemFilePath expected = do
  cert <- loadSignedCertificate pemFilePath
  certDescription cert `shouldBe` expected

-- | Load and decode a SignedCertificate from a PEM file
loadSignedCertificate :: FilePath -> IO SignedCertificate
loadSignedCertificate pemFilePath = do
  -- sanity check if the file even exists
  exists <- doesFileExist pemFilePath
  exists `shouldBe` True

  file <- BS.readFile pemFilePath
  pure . either error id $ do
    pemBS <- pemContent . fromMaybe (error "Empty PEM list") . listToMaybe <$> pemParseBS file
    decodeSignedCertificate pemBS
