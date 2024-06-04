module Testlib.Certs where

import Crypto.Hash.Algorithms (SHA256 (SHA256))
import qualified Crypto.PubKey.RSA as RSA
import qualified Crypto.PubKey.RSA.PKCS15 as PKCS15
import Crypto.Store.PKCS8 (PrivateKeyFormat (PKCS8Format), keyToPEM)
import Crypto.Store.X509 (pubKeyToPEM)
import Data.ASN1.OID (OIDable (getObjectID))
import Data.Hourglass
import Data.PEM (PEM (PEM), pemWriteBS)
import Data.String.Conversions (cs)
import Data.X509
import Testlib.Prelude

type RSAKeyPair = (RSA.PublicKey, RSA.PrivateKey)

type SignedCert = SignedExact Certificate

-- | convert a PEM to a string
toPem :: PEM -> String
toPem = cs . pemWriteBS

-- | convert a signed certificate to a string
signedCertToString :: SignedCert -> String
signedCertToString = toPem . PEM "CERTIFICATE" [] . encodeSignedObject

-- | convert a private key to string
privateKeyToString :: RSA.PrivateKey -> String
privateKeyToString = toPem . keyToPEM PKCS8Format . PrivKeyRSA

-- | convert a public key to string
publicKeyToString :: RSA.PublicKey -> String
publicKeyToString = toPem . pubKeyToPEM . PubKeyRSA

-- | order: publickey, private key
keyPairToString :: RSAKeyPair -> (String, String)
keyPairToString = bimap publicKeyToString privateKeyToString

-- | the minimum key size is hard coded to be 256 bytes (= 2048 bits)
--
-- TODO(mangoiv): key generation takes an (actually multiple) eternities
mkKeyPair :: HasCallStack => App RSAKeyPair
mkKeyPair = liftIO do RSA.generate 2048 65537

-- | create a root certificate authority CertificateBundle
createRootCA ::
  HasCallStack =>
  -- | the root CA's name
  String ->
  -- | the root CA's keymaterial
  RSAKeyPair ->
  SignedCert
createRootCA caName (pubKey, privKey) =
  mkSignedCert
    pubKey
    privKey
    caName
    caName

-- | sign an intermediate/ leaf certificate by signing with an intermediate/ root CA's key
intermediateCert ::
  HasCallStack =>
  -- | name of the owner of the certificate
  String ->
  -- | the public key of the owner
  RSA.PublicKey ->
  -- | name of the signatory (intermediate/ root CA)
  String ->
  -- | the private (signature) key of the signing (intermediate/ root) CA
  RSA.PrivateKey ->
  SignedCert
intermediateCert intermediateCaName pubKey rootCaName rootKey =
  mkSignedCert
    pubKey
    rootKey
    rootCaName
    intermediateCaName

-- | self sign a certificate
selfSignedCert ::
  HasCallStack =>
  -- | name of the owner
  String ->
  -- | key material of the owner
  RSAKeyPair ->
  SignedCert
selfSignedCert ownerName (pubKey, privKey) =
  mkSignedCert
    pubKey
    privKey
    ownerName
    ownerName

signMsgWithPrivateKey :: HasCallStack => RSA.PrivateKey -> ByteString -> ByteString
signMsgWithPrivateKey privKey = fromRight (error "signing unsuccessful") . PKCS15.sign Nothing (Just SHA256) privKey

-- | create a signed certificate
mkSignedCert ::
  HasCallStack =>
  -- | public key of the *owner*
  RSA.PublicKey ->
  -- | private key of *signatory*
  RSA.PrivateKey ->
  -- | name of the issuer
  String ->
  -- | name of the owner
  String ->
  SignedExact Certificate
mkSignedCert pubKey privKey caName ownerName =
  let distinguishedName name =
        DistinguishedName
          [ (getObjectID DnCommonName, fromString $ name),
            (getObjectID DnCountry, fromString "DE")
          ]
   in fst $
        objectToSignedExact
          (\msg -> (signMsgWithPrivateKey privKey msg, SignatureALG HashSHA256 PubKeyALG_RSA, ()))
          Certificate
            { certVersion = 3,
              certSerial = 1,
              certSignatureAlg = SignatureALG HashSHA256 PubKeyALG_RSA,
              certIssuerDN = distinguishedName caName,
              certValidity = (DateTime {dtDate = Date 2000 January 1, dtTime = midNight}, DateTime {dtDate = Date 2049 January 1, dtTime = midNight}),
              certSubjectDN = distinguishedName ownerName,
              certPubKey = PubKeyRSA pubKey,
              certExtensions = Extensions Nothing
            }
  where
    midNight = TimeOfDay 0 0 0 0
