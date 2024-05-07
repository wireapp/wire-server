{-# OPTIONS_GHC -Wwarn #-}

module Testlib.Certs where

import Crypto.Hash.Algorithms (SHA256 (SHA256))
import qualified Crypto.PubKey.RSA as RSA
import qualified Crypto.PubKey.RSA.PKCS15 as PKCS15
import Crypto.Store.PKCS8 (PrivateKeyFormat (PKCS8Format), keyToPEM)
import Crypto.Store.X509 (pubKeyToPEM)
import Data.ASN1.OID (OIDable (getObjectID))
import Data.Hourglass
import Data.PEM (PEM (PEM), pemWriteBS)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.X509
import Testlib.Prelude

type RSAKeyPair = (RSA.PublicKey, RSA.PrivateKey)

data CertificateBundle = MkCertificateBundle
  { keyMaterial :: RSAKeyPair,
    cert :: SignedExact Certificate
  }

data CreateCA = MkCreateCA
  { caName :: String
  }

instance Default CreateCA where
  def =
    MkCreateCA
      { caName = "ExampleCA"
      }

mkRSAKeys :: HasCallStack => App RSAKeyPair
mkRSAKeys = liftIO $ RSA.generate 1024 65537

-- | from a bundle, return a triple of cert, private key, public key as Strings
bundleToTriple :: CertificateBundle -> (String, String, String)
bundleToTriple bundle =
  ( toPem . PEM "CERTIFICATE" [] . encodeSignedObject $ bundle.cert,
    toPem $ keyToPEM PKCS8Format $ PrivKeyRSA $ snd bundle.keyMaterial,
    toPem $ pubKeyToPEM $ PubKeyRSA $ fst bundle.keyMaterial
  )
  where
    toPem = T.unpack . T.decodeASCII . pemWriteBS

-- | create a root certificate authority CertificateBundle
createRootCA :: HasCallStack => CreateCA -> App CertificateBundle
createRootCA createCA = do
  keys@(pubKey, privKey) <- mkRSAKeys
  pure
    MkCertificateBundle
      { cert = mkSignedCert pubKey (signMsgWithPrivateKey privKey) createCA.caName "-Root",
        keyMaterial = keys
      }

intermediateCA ::
  HasCallStack =>
  -- | instructions for creating the intermediate CA
  CreateCA ->
  -- | the cert and keymaterial of the signing (root) CA
  CertificateBundle ->
  App CertificateBundle
intermediateCA createCA rootCa = do
  keys@(pubKey, _) <- mkRSAKeys
  pure
    MkCertificateBundle
      { cert = mkSignedCert pubKey (signMsgWithPrivateKey (snd rootCa.keyMaterial)) createCA.caName "-Intermediate",
        keyMaterial = keys
      }

leafCert ::
  HasCallStack =>
  -- | the owner of the certificate
  String ->
  -- | certificate bundle of the Intermediate CA
  CertificateBundle ->
  App CertificateBundle
leafCert ownerName intermediateCa = do
  keys@(pubKey, _) <- mkRSAKeys
  pure
    MkCertificateBundle
      { cert = mkSignedCert pubKey (signMsgWithPrivateKey (snd intermediateCa.keyMaterial)) ownerName "-Leaf",
        keyMaterial = keys
      }

signMsgWithPrivateKey :: HasCallStack => RSA.PrivateKey -> ByteString -> ByteString
signMsgWithPrivateKey privKey = fromRight (error "signing unsuccessful") . PKCS15.sign Nothing (Just SHA256) privKey

mkSignedCert :: HasCallStack => RSA.PublicKey -> (ByteString -> ByteString) -> String -> String -> SignedExact Certificate
mkSignedCert pubKey signatureFn caName suffix =
  let distinguishedName =
        DistinguishedName
          [ (getObjectID DnCommonName, fromString $ caName <> suffix),
            (getObjectID DnCountry, fromString "DE"),
            (getObjectID DnOrganization, fromString caName)
          ]
   in fst $
        objectToSignedExact
          (\msg -> (signatureFn msg, SignatureALG HashSHA256 PubKeyALG_RSA, ()))
          Certificate
            { certVersion = 3,
              certSerial = 1,
              certSignatureAlg = SignatureALG HashSHA256 PubKeyALG_RSA,
              certIssuerDN = distinguishedName,
              certValidity = (DateTime {dtDate = Date 2000 January 1, dtTime = midNight}, DateTime {dtDate = Date 2049 January 1, dtTime = midNight}),
              certSubjectDN = distinguishedName,
              certPubKey = PubKeyRSA pubKey,
              certExtensions = Extensions Nothing
            }
  where
    midNight = TimeOfDay 0 0 0 0
