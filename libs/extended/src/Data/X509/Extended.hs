{-# LANGUAGE RecordWildCards #-}

module Data.X509.Extended (certToString, certDescription, CertDescription (..)) where

import Crypto.Hash
import Data.ASN1.OID
import Data.ASN1.Types
import Data.ByteArray.Encoding qualified as BAE
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
      der = encodeSignedObject signedCert
      fingerprintBS :: ByteString = BAE.convertToBase BAE.Base16 (hash der :: Digest SHA1)
      fingerprint =
        let hex = (T.decodeUtf8 fingerprintBS)
            pairs = T.unpack <$> T.chunksOf 2 hex
         in map toUpper (intercalate ":" pairs)
      fingerprintAlgorithm = "SHA1"
   in CertDescription {..}

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
