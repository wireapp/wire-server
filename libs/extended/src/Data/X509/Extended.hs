{-# LANGUAGE RecordWildCards #-}

module Data.X509.Extended
  ( certToString,
    certDescription,
    CertDescription (..),
    Fingerprint,
    unFingerprint,
    certSha1Fingerprint,
    parseFingerprintHex,
    renderFingerprintHex,
  )
where

import Crypto.Hash
import Data.ASN1.OID
import Data.ASN1.Types
import Data.ByteArray qualified as BA
import Data.ByteArray.Encoding qualified as BAE
import Data.ByteString qualified as BS
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.X509
import Imports

certToString :: SignedCertificate -> String
certToString signedCert =
  let desc = certDescription signedCert
   in -- Split into pairs and join with ':'
      mconcat . intersperse "; " $
        [ "Issuer: " <> desc.issuer,
          "Subject: " <> desc.subject,
          desc.fingerprintAlgorithm <> " Fingerprint: " <> desc.fingerprint
        ]

data CertDescription = CertDescription
  { fingerprintAlgorithm :: String,
    fingerprint :: String,
    subject :: String,
    issuer :: String
  }
  deriving (Eq, Show)

-- | Extract structured certificate description information
certDescription :: SignedCertificate -> CertDescription
certDescription signedCert =
  let cert = getCertificate signedCert
      issuer = dnToString $ certIssuerDN cert
      subject = dnToString $ certSubjectDN cert
      fingerprint = T.unpack . renderFingerprintHex . certSha1Fingerprint $ signedCert
      fingerprintAlgorithm = "SHA1"
   in CertDescription {..}

-- | SHA-1 fingerprint: 20 bytes. Build via 'certSha1Fingerprint' or
-- 'parseFingerprintHex' (both guarantee the length).
newtype Fingerprint = Fingerprint {unFingerprint :: ByteString}
  deriving (Eq, Ord, Show)

certSha1Fingerprint :: SignedCertificate -> Fingerprint
certSha1Fingerprint signedCert =
  let der = encodeSignedObject signedCert
   in Fingerprint (BA.convert (hash der :: Digest SHA1))

-- | Permissive: ignores case and ':'/whitespace separators.
parseFingerprintHex :: Text -> Either String Fingerprint
parseFingerprintHex t =
  let cleaned = T.filter (\c -> c /= ':' && not (isSpace c)) t
      asBs = T.encodeUtf8 cleaned
   in case BAE.convertFromBase BAE.Base16 asBs of
        Left e -> Left ("invalid hex fingerprint: " <> e)
        Right bytes
          | BS.length bytes == 20 -> Right (Fingerprint bytes)
          | otherwise ->
              Left $
                "invalid SHA-1 fingerprint length: expected 20 bytes, got "
                  <> show (BS.length bytes)

-- | Canonical openssl form: uppercase pairs, ':'-separated.
--
-- >>> renderFingerprintHex (Fingerprint "\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\x0f\x10\x11\x12\x13\x14")
-- "01:02:03:04:05:06:07:08:09:0A:0B:0C:0D:0E:0F:10:11:12:13:14"
renderFingerprintHex :: Fingerprint -> Text
renderFingerprintHex (Fingerprint bs) =
  let hex = T.toUpper . T.decodeUtf8 . BAE.convertToBase BAE.Base16 $ bs
   in T.intercalate ":" (T.chunksOf 2 hex)

dnToString :: DistinguishedName -> String
dnToString (getDistinguishedElements -> es) =
  let dess :: [String] = mapMaybe distinguishedElementString es
   in mconcat $ intersperse "," dess
  where
    distinguishedElementString :: (OID, ASN1CharacterString) -> Maybe String
    distinguishedElementString (oid, aSN1CharacterString) = do
      (_element, desc) <- Map.lookup oid dnElementMap
      val <- asn1CharacterToString aSN1CharacterString
      pure $ desc <> "=" <> val

    dnElementMap :: Map OID (DnElement, String)
    dnElementMap =
      Map.fromList
        [ (mkEntry DnCommonName "CN"),
          (mkEntry DnCountry "Country"),
          (mkEntry DnOrganization "O"),
          (mkEntry DnOrganizationUnit "OU"),
          (mkEntry DnEmailAddress "Email Address")
        ]
      where
        mkEntry :: DnElement -> String -> (OID, (DnElement, String))
        mkEntry e s = (getObjectID e, (e, s))
