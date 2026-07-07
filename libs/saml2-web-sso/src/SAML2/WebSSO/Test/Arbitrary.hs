{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module SAML2.WebSSO.Test.Arbitrary where

import Control.Lens
import Crypto.Hash.Algorithms (SHA256 (SHA256))
import Crypto.PubKey.RSA qualified as RSA
import Crypto.PubKey.RSA.PKCS15 qualified as PKCS15
import Data.ASN1.OID (getObjectID)
import Data.ASN1.Types (ASN1StringEncoding (UTF8))
import Data.CaseInsensitive qualified as CI
import Data.Fixed
import Data.Hourglass qualified as Hourglass
import Data.List.NonEmpty as NL
import Data.Map qualified as Map
import Data.Proxy
import Data.String.Conversions
import Data.Text qualified as ST
import Data.Time
import Data.UUID qualified as UUID
import Data.X509 qualified as X509
import GHC.Stack
import GHC.TypeLits
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Gen.QuickCheck qualified as THQ
import Hedgehog.Range qualified as Range
import SAML2.WebSSO
import SAML2.WebSSO.Types.Email qualified as Email
import Servant.Multipart
import Test.QuickCheck (Arbitrary (arbitrary, shrink))
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Hedgehog qualified as TQH
import Test.QuickCheck.Instances ()
import Text.XML
import Text.XML.DSig qualified as DSig
import URI.ByteString
import Web.Cookie

genHttps :: Gen URI
genHttps = genHttps' Nothing

-- | arbitrary 'URI' with restricted length.
--
-- uri-bytestring has Arbitrary instances, but they are likely to remain internal.  also we're not
-- sure what restrictions we'll need to impose on those in roder to get the URIs of the shape
-- required here.  https://github.com/Soostone/uri-bytestring/issues/45
genHttps' :: Maybe (Range Int) -> Gen URI
genHttps' glen = do
  domain <- ST.intercalate "." <$> Gen.list (Range.linear 2 5) genNiceWord
  path <- ST.intercalate "/" <$> Gen.list (Range.linear 0 5) genNiceWord
  mMaxLen :: Maybe Int <- maybe (pure Nothing) (fmap Just . Gen.integral_) glen
  let uri = maybe id ST.take mMaxLen $ "https://" <> domain <> "/" <> path
  either (error . show) pure $ parseURI' uri

-- | pick N words from a dictionary of popular estonian first names.  this should yield enough
-- entropy, but is much nicer to read.
--
-- (quickcheck has something like this as well.)
genNiceText :: Range Int -> Gen ST
genNiceText rng = ST.unwords <$> Gen.list rng word
  where
    -- popular estonian first names.
    word =
      Gen.element
        [ "aiandama",
          "aitama",
          "aitamah",
          "aleksander",
          "andres",
          "andrus",
          "anu",
          "arri",
          "aruka",
          "aytama",
          "aytamah",
          "betti",
          "daggi",
          "dagi",
          "dagmara",
          "diana",
          "edenema",
          "eduk",
          "eliisabet",
          "elisabet",
          "elsbet",
          "elts",
          "etti",
          "etty",
          "hele",
          "hendrik",
          "jaak",
          "juku",
          "juri",
          "kaisa",
          "kaja",
          "katariina",
          "koit",
          "leena",
          "lenni",
          "liisi",
          "lilli",
          "loviise",
          "maarja",
          "marika",
          "nikolai",
          "rina",
          "sandra",
          "sula",
          "taevas",
          "taniel",
          "tonis",
          "ulli",
          "urmi",
          "vicenc",
          "anna",
          "eluta",
          "hillar",
          "jaagup",
          "jaan",
          "janek",
          "jannis",
          "jens",
          "johan",
          "johanna",
          "juhan",
          "katharina",
          "kati",
          "katja",
          "krista",
          "kristian",
          "kristina",
          "kristjan",
          "krists",
          "laura",
          "leks",
          "liisa",
          "marga",
          "margarete",
          "mari",
          "maria",
          "marye",
          "mati",
          "matt",
          "mihkel",
          "mikk",
          "olli",
          "olly",
          "peet",
          "peeter",
          "pinja",
          "reet",
          "riki",
          "riks",
          "rolli",
          "toomas"
        ]

genNiceWord :: Gen ST
genNiceWord = genNiceText (Range.singleton 1)

genConfig :: Gen Config
genConfig = do
  _cfgLogLevel <- Gen.enumBounded
  _cfgSPHost <- cs <$> genNiceWord
  _cfgSPPort <- Gen.int (Range.linear 1 9999)
  _cfgDomainConfigs <- Left <$> genMultiIngressDomainConfig
  pure Config {..}

genMultiIngressDomainConfig :: Gen MultiIngressDomainConfig
genMultiIngressDomainConfig = do
  _cfgSPAppURI <- genHttps
  _cfgSPSsoURI <- genHttps
  _cfgContacts <- Gen.list (Range.linear 0 3) genSPContactPerson
  pure MultiIngressDomainConfig {..}

genSPContactPerson :: Gen ContactPerson
genSPContactPerson =
  ContactPerson
    <$> Gen.enumBounded
    <*> Gen.maybe genNiceWord
    <*> Gen.maybe genNiceWord
    <*> Gen.maybe genNiceWord
    <*> Gen.maybe genHttps
    <*> Gen.maybe genNiceWord

genIdPMetadata :: Gen IdPMetadata
genIdPMetadata =
  IdPMetadata
    <$> genIssuer
    <*> genHttps
    <*> (NL.fromList <$> Gen.list (Range.linear 1 3) genX509SignedCertificate)

-- FUTUREWORK: we can do better than constant here...
genX509SignedCertificate :: Gen X509.SignedCertificate
genX509SignedCertificate = either (error . show) pure $ DSig.parseKeyInfo False "<KeyInfo xmlns=\"http://www.w3.org/2000/09/xmldsig#\"><X509Data><X509Certificate>MIIDBTCCAe2gAwIBAgIQev76BWqjWZxChmKkGqoAfDANBgkqhkiG9w0BAQsFADAtMSswKQYDVQQDEyJhY2NvdW50cy5hY2Nlc3Njb250cm9sLndpbmRvd3MubmV0MB4XDTE4MDIxODAwMDAwMFoXDTIwMDIxOTAwMDAwMFowLTErMCkGA1UEAxMiYWNjb3VudHMuYWNjZXNzY29udHJvbC53aW5kb3dzLm5ldDCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAMgmGiRfLh6Fdi99XI2VA3XKHStWNRLEy5Aw/gxFxchnh2kPdk/bejFOs2swcx7yUWqxujjCNRsLBcWfaKUlTnrkY7i9x9noZlMrijgJy/Lk+HH5HX24PQCDf+twjnHHxZ9G6/8VLM2e5ZBeZm+t7M3vhuumEHG3UwloLF6cUeuPdW+exnOB1U1fHBIFOG8ns4SSIoq6zw5rdt0CSI6+l7b1DEjVvPLtJF+zyjlJ1Qp7NgBvAwdiPiRMU4l8IRVbuSVKoKYJoyJ4L3eXsjczoBSTJ6VjV2mygz96DC70MY3avccFrk7tCEC6ZlMRBfY1XPLyldT7tsR3EuzjecSa1M8CAwEAAaMhMB8wHQYDVR0OBBYEFIks1srixjpSLXeiR8zES5cTY6fBMA0GCSqGSIb3DQEBCwUAA4IBAQCKthfK4C31DMuDyQZVS3F7+4Evld3hjiwqu2uGDK+qFZas/D/eDunxsFpiwqC01RIMFFN8yvmMjHphLHiBHWxcBTS+tm7AhmAvWMdxO5lzJLS+UWAyPF5ICROe8Mu9iNJiO5JlCo0Wpui9RbB1C81Xhax1gWHK245ESL6k7YWvyMYWrGqr1NuQcNS0B/AIT1Nsj1WY7efMJQOmnMHkPUTWryVZlthijYyd7P2Gz6rY5a81DAFqhDNJl2pGIAE6HWtSzeUEh3jCsHEkoglKfm4VrGJEuXcALmfCMbdfTvtu4rlsaP2hQad+MG/KJFlenoTK34EMHeBPDCpqNDz8UVNk</X509Certificate></X509Data></KeyInfo>"

genSPMetadata :: Gen SPMetadata
genSPMetadata = do
  _spID <- genID
  _spValidUntil <- fromTime <$> genTime
  _spCacheDuration <- genNominalDifftime
  _spOrgName <- genNiceWord
  _spOrgDisplayName <- genNiceWord
  _spOrgURL <- genHttps
  _spResponseURL <- genHttps
  _spContacts <- Gen.list (Range.linear 0 3) genContactPerson
  pure SPMetadata {..}

genContactPerson :: Gen ContactPerson
genContactPerson = do
  _cntType <- Gen.enumBounded
  _cntCompany <- Gen.maybe genNiceWord
  _cntGivenName <- Gen.maybe genNiceWord
  _cntSurname <- Gen.maybe genNiceWord
  _cntEmail <- Gen.maybe genEmailURI
  _cntPhone <- Gen.maybe genNiceWord
  pure ContactPerson {..}

genEmailURI :: Gen URI
genEmailURI = do
  loc <- genNiceWord
  pure . unsafeParseURI $ "email:" <> loc <> "@example.com"

genEmail :: (HasCallStack) => Gen (CI.CI Email.Email)
genEmail = do
  loc <- genNiceWord
  either (error . ("genEmail: " <>)) pure . Email.validate $ loc <> "@example.com"

genAuthnRequest :: Gen AuthnRequest
genAuthnRequest =
  AuthnRequest
    <$> genID
    <*> genTime
    <*> genIssuer
    <*> Gen.maybe genNameIDPolicy

-- | (we only allow full microseconds, since someone, somewhere does the rounding for us in the
-- tests if we don't do it here, which makes the affected tests fail.)
genTime :: Gen Time
genTime = Time . picoToMicro <$> THQ.quickcheck arbitrary
  where
    picoToMicro = seconds %~ ((* (1000 * 1000)) . (/ (1000 * 1000)))

genDuration :: Gen Duration
genDuration = pure Duration

genNominalDifftime :: Gen NominalDiffTime
genNominalDifftime = THQ.quickcheck arbitrary

genID :: Gen (ID a)
genID = ID . ("_" <>) . UUID.toText <$> genUUID

genIssuer :: Gen Issuer
genIssuer = Issuer <$> genHttps

genNameIDPolicy :: Gen NameIdPolicy
genNameIDPolicy =
  NameIdPolicy
    <$> genNameIDFormat
    <*> Gen.maybe genNiceWord
    <*> Gen.bool

genNameIDFormat :: Gen NameIDFormat
genNameIDFormat = Gen.enumBounded

genNameID :: Gen NameID
genNameID = do
  unid <- genUnqualifiedNameID
  case unid of
    UNameIDEntity enturi -> pure $ entityNameID enturi
    _ ->
      either (error . show) pure
        =<< (mkNameID unid <$> qualifier <*> qualifier <*> qualifier)
  where
    qualifier = Gen.maybe . genNiceText $ Range.exponential 1 100

genUnqualifiedNameID :: Gen UnqualifiedNameID
genUnqualifiedNameID =
  Gen.choice
    [ UNameIDUnspecified <$> mktxt 2000,
      UNameIDEmail <$> genEmail,
      UNameIDX509 <$> mktxt 2000,
      UNameIDWindows <$> mktxt 2000,
      UNameIDKerberos <$> mktxt 2000,
      UNameIDEntity <$> genHttps' (Just (Range.linear 12 1024)),
      UNameIDPersistent <$> mktxt 1024,
      UNameIDTransient <$> mktxt 2000
    ]
  where
    mktxt charlen = Gen.text (Range.linear 1 charlen) Gen.alpha

genNonEmpty :: Range Int -> Gen a -> Gen (NonEmpty a)
genNonEmpty rng gen = (:|) <$> gen <*> Gen.list rng gen

genStatus :: Gen Status
genStatus = Gen.enumBounded

genAuthnResponse :: Gen AuthnResponse
genAuthnResponse = genResponse (NL.fromList <$> Gen.list (Range.linear 1 3) genAssertion)

genResponse :: forall payload. Gen payload -> Gen (Response payload)
genResponse genPayload = do
  _rspID <- genID
  _rspInRespTo <- Gen.maybe genID
  _rspIssueInstant <- genTime
  _rspDestination <- Gen.maybe genHttps
  _rspIssuer <- Gen.maybe genIssuer
  _rspStatus <- genStatus
  _rspPayload <- Gen.small genPayload
  pure Response {..}

genAssertion :: Gen Assertion
genAssertion = do
  _assID <- genID
  _assIssueInstant <- genTime
  _assIssuer <- genIssuer
  _assConditions <- Gen.maybe genConditions
  _assContents <- genSubjectAndStatements
  pure Assertion {..}

genConditions :: Gen Conditions
genConditions =
  Conditions
    <$> Gen.maybe genTime
    <*> Gen.maybe genTime
    <*> Gen.bool
    <*> Gen.list (Range.linear 0 3) (genNonEmpty (Range.linear 0 3) genHttps)

genSubjectAndStatements :: Gen SubjectAndStatements
genSubjectAndStatements =
  SubjectAndStatements
    <$> genSubject
    <*> genNonEmpty (Range.linear 0 3) genStatement

genSubject :: Gen Subject
genSubject =
  Subject
    <$> genNameID
    <*> Gen.list (Range.linear 0 8) genSubjectConfirmation

genSubjectConfirmation :: Gen SubjectConfirmation
genSubjectConfirmation =
  SubjectConfirmation
    <$> genSubjectConfirmationMethod
    <*> Gen.maybe genSubjectConfirmationData

genSubjectConfirmationMethod :: Gen SubjectConfirmationMethod
genSubjectConfirmationMethod = Gen.enumBounded

genSubjectConfirmationData :: Gen SubjectConfirmationData
genSubjectConfirmationData = do
  _scdNotBefore <- Gen.maybe genTime
  _scdNotOnOrAfter <- genTime
  _scdRecipient <- genHttps
  _scdInResponseTo <- Gen.maybe genID
  _scdAddress <- Gen.maybe genIP
  pure SubjectConfirmationData {..}

genDNSName :: Gen DNSName
genDNSName =
  Gen.choice $
    pure . mkDNSName
      <$> [ "localhost",
            "one.example.com",
            "two.example.com",
            "three.example.com",
            "four.example.com",
            "five.example.com",
            "six.example.com",
            "seven.example.com"
          ]

genIP :: Gen IP
genIP =
  Gen.choice $
    either (error . show) pure
      . mkIP
      <$> [ "127.0.0.1",
            "::1",
            "192.168.1.0",
            "192.168.1.1",
            "192.168.1.2",
            "192.168.1.3",
            "192.168.1.4",
            "192.168.1.5",
            "192.168.1.6",
            "192.168.1.7",
            "192.168.1.8",
            "192.168.1.9"
          ]

genStatement :: Gen Statement
genStatement = do
  _astAuthnInstant <- genTime
  _astSessionIndex <- Gen.maybe genNiceWord
  _astSessionNotOnOrAfter <- Gen.maybe genTime
  _astSubjectLocality <- Gen.maybe genLocality
  pure AuthnStatement {..}

genLocality :: Gen Locality
genLocality =
  Locality
    <$> Gen.maybe genIP
    <*> Gen.maybe genDNSName

genXMLDocument :: Gen Document
genXMLDocument = do
  el <- genXMLElement
  pure $ Document (Prologue [] Nothing []) el []

genXMLNode :: Gen Node
genXMLNode =
  Gen.choice
    [ NodeElement <$> genXMLElement,
      NodeInstruction <$> genXMLInstruction,
      NodeContent <$> genNiceText (Range.linear 0 100),
      NodeComment <$> genNiceText (Range.linear 0 100)
    ]

genXMLElement :: Gen Element
genXMLElement =
  Element
    <$> genXMLName
    <*> genXMLAttrs
    <*> Gen.list (Range.linear 1 10) (Gen.small genXMLNode)

genXMLName :: Gen Name
genXMLName =
  Name
    <$> genNiceWord
    <*> Gen.maybe genNiceWord
    <*> pure Nothing -- @Gen.maybe genNiceWord@, but in documents that use the same prefix for two
    -- different spaces, this breaks the test suite.  (FUTUREWORK: arguably the
    -- parser libraries (either HXT or xml-conduit) should catch this and throw an
    -- error.  current behavior is unspecified result of the name space lookup.)

genXMLAttrs :: Gen (Map.Map Name ST)
genXMLAttrs = Map.fromList <$> Gen.list (Range.linear 1 7) genXMLAttr

genXMLAttr :: Gen (Name, ST)
genXMLAttr = (,) <$> genXMLName <*> genNiceWord

genXMLInstruction :: Gen Instruction
genXMLInstruction = Instruction <$> genNiceWord <*> genNiceWord

genUUID :: (HasCallStack) => Gen UUID.UUID
genUUID = THQ.quickcheck arbitrary

genIdPId :: Gen IdPId
genIdPId = IdPId <$> genUUID

-- | Always returns the same constant cert. Prefer 'mkArbitrarySignedCert'.
genSignedCertificate :: Gen X509.SignedCertificate
genSignedCertificate =
  either (error . show) pure $
    DSig.parseKeyInfo
      False
      "<KeyInfo xmlns=\"http://www.w3.org/2000/09/xmldsig#\"><X509Data><X509Certificate>MIIDBTCCAe2gAwIBAgIQev76BWqjWZxChmKkGqoAfDANBgkqhkiG9w0BAQsFADAtMSswKQYDVQQDEyJhY2NvdW50cy5hY2Nlc3Njb250cm9sLndpbmRvd3MubmV0MB4XDTE4MDIxODAwMDAwMFoXDTIwMDIxOTAwMDAwMFowLTErMCkGA1UEAxMiYWNjb3VudHMuYWNjZXNzY29udHJvbC53aW5kb3dzLm5ldDCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAMgmGiRfLh6Fdi99XI2VA3XKHStWNRLEy5Aw/gxFxchnh2kPdk/bejFOs2swcx7yUWqxujjCNRsLBcWfaKUlTnrkY7i9x9noZlMrijgJy/Lk+HH5HX24PQCDf+twjnHHxZ9G6/8VLM2e5ZBeZm+t7M3vhuumEHG3UwloLF6cUeuPdW+exnOB1U1fHBIFOG8ns4SSIoq6zw5rdt0CSI6+l7b1DEjVvPLtJF+zyjlJ1Qp7NgBvAwdiPiRMU4l8IRVbuSVKoKYJoyJ4L3eXsjczoBSTJ6VjV2mygz96DC70MY3avccFrk7tCEC6ZlMRBfY1XPLyldT7tsR3EuzjecSa1M8CAwEAAaMhMB8wHQYDVR0OBBYEFIks1srixjpSLXeiR8zES5cTY6fBMA0GCSqGSIb3DQEBCwUAA4IBAQCKthfK4C31DMuDyQZVS3F7+4Evld3hjiwqu2uGDK+qFZas/D/eDunxsFpiwqC01RIMFFN8yvmMjHphLHiBHWxcBTS+tm7AhmAvWMdxO5lzJLS+UWAyPF5ICROe8Mu9iNJiO5JlCo0Wpui9RbB1C81Xhax1gWHK245ESL6k7YWvyMYWrGqr1NuQcNS0B/AIT1Nsj1WY7efMJQOmnMHkPUTWryVZlthijYyd7P2Gz6rY5a81DAFqhDNJl2pGIAE6HWtSzeUEh3jCsHEkoglKfm4VrGJEuXcALmfCMbdfTvtu4rlsaP2hQad+MG/KJFlenoTK34EMHeBPDCpqNDz8UVNk</X509Certificate></X509Data></KeyInfo>"

genDistinguishedName :: Gen X509.DistinguishedName
genDistinguishedName = do
  cn <- genNiceWord
  org <- Gen.maybe genNiceWord
  pure $
    X509.DistinguishedName $
      (getObjectID X509.DnCommonName, X509.ASN1CharacterString UTF8 (cs cn))
        : maybe [] (\o -> [(getObjectID X509.DnOrganization, X509.ASN1CharacterString UTF8 (cs o))]) org

-- | Build a self-signed certificate
mkArbitrarySignedCert :: QC.Gen X509.SignedCertificate
mkArbitrarySignedCert = do
  serial <- TQH.hedgehog $ Gen.integral_ (Range.linear 1 (2 ^ (31 :: Int)))
  certVer <- TQH.hedgehog $ Gen.integral_ (Range.linear 1 3)
  dn <- TQH.hedgehog genDistinguishedName
  pure $
    fst $
      X509.objectToSignedExact
        (\msg -> (signMsg msg, X509.SignatureALG X509.HashSHA256 X509.PubKeyALG_RSA, ()))
        X509.Certificate
          { X509.certVersion = certVer,
            X509.certSerial = serial,
            X509.certSignatureAlg = X509.SignatureALG X509.HashSHA256 X509.PubKeyALG_RSA,
            X509.certIssuerDN = dn,
            X509.certValidity =
              ( Hourglass.DateTime {Hourglass.dtDate = Hourglass.Date 2000 Hourglass.January 1, Hourglass.dtTime = Hourglass.TimeOfDay 0 0 0 0},
                Hourglass.DateTime {Hourglass.dtDate = Hourglass.Date 2049 Hourglass.January 1, Hourglass.dtTime = Hourglass.TimeOfDay 0 0 0 0}
              ),
            X509.certSubjectDN = dn,
            X509.certPubKey = X509.PubKeyRSA arbitraryCertPubKey,
            X509.certExtensions = X509.Extensions Nothing
          }
  where
    signMsg msg =
      either (error "mkArbitrarySignedCert: signing failed") id $
        PKCS15.sign Nothing (Just SHA256) arbitraryCertPrivKey msg

-- | Fixed RSA key pair for test cert signing. Hardcoded primes avoid IO.
arbitraryCertPubKey :: RSA.PublicKey
arbitraryCertPrivKey :: RSA.PrivateKey
(arbitraryCertPubKey, arbitraryCertPrivKey) =
  case RSA.generateWith arbitraryCertPrimes 2048 65537 of
    Just k -> k
    Nothing -> error "arbitraryCertPubKey: invalid primes"

arbitraryCertPrimes :: (Integer, Integer)
arbitraryCertPrimes =
  ( 1013416710455617992060044810859399709890835129925648843043641673852539448350775594187007527506724875627885909523835606557173980236290013476205929897072239944138314384631600538474898358198731711608598716779857515154388088878657555928549962380829213547435085854695442354636327047821108802590374275481605077802187415357974963365435650338024405558985202998762641404395411587629314013330411500470203761301812113710962088477051775450894192994742118846780105265558368972170180276350636994878636389758206123738715722878057404540464220733023391993383290494652037274532356460190907090422144536951440069212998822960155765054879900781581263606916652700903953626527029121897494538017122565993895036773799860052414697053960902764894046849087727915659738623914130083281919853081537137782445589156217286369690178786653090799221857147470043219175767249163571686740347462294750028790472737772761949491538873890614496706566060247820117584298845501935064037819052405654373374661838572553244593002834443762478259268799467895951456315647324157054992319938064879914915556645111272573189405077515029783954913337757933225821260787418411247627537065834022908147122036442414923430533383989652364612738513379313521406363716216150953874675705623133860932309998632104801092827841702718992714882139811954467163400593020720191718049863114367363094097654194786896842879463158349468509662084081492854544553121389587671952367596127566679408181243898540691657673709282297206699665271972122876732477153246545187514721891966873910637813569799235783300883640120382296336980469139678449923244327325676463743789034561023783533980749100272005938046751700931286800296518645750336292219055157506140422334232031499441618108378207249469768514341014613604798707882336528213109908520952809254346958192134161621644423814067058523341464457188689237566854457651740962437154879472377563420329379777383724869785437079461381042576932777663816932792106785972722313112138774627384189872028788531464434347861094422498231096686231475413078333450041613628998736286930594422166708703115486915826404578851616898264340560519310655180870217752558303339822824214706404615558734661262111177357709447064658518593459191904042065215329175588893364731436963818899069593653897213811368511785916948261704025900054681973429106441628584851712758726618885443787735678619865846520873765930283904988556631550968487727144405349504203063775775239807234977371854786517646240982498594502233136236903225375658288185007963323167751702824125884605983,
    927336758709169856221729309972684377326012758705584701160913392855296574209188805952293975727392736357355525822682625960867980784906333126250176772633612511280160520450355917665344680820117001909657304528897728644985372222487760541890997744380957145384918405839817509991111341989419216342513467094263440712622240826707558561965237909070383875063686755789716081493927682670013715434239129366779748040394792694841549258598842315715859562294976974200564408338450316192760863885386436881465495436476022429943600686139972778561942722494137924396693749231870673494020761865863446686474725091312431012619078931330640808188498974525508440925548025604310429878232463952454557835744654770844144316962049844107999645072674978011865146180434315809137160022154815275730622923394822959089495198091753080586758917401240837851455881168916390487103230014598246305055773428160686563500509562651266122967947533947385066722712316194439650272469880653336775557226431438158529031941085177895035782278423238393385871537920481620086314516883242108371084035236009476902958675684122414056114458154814623140680549398143962297844269217544119579639388880282746926211911340151495180800938356829417651851575812389707158878607136197574826859775996273379970390171328581948608028025142182853278853363612390290636206287758711077096741448655899931751827724488361988091582792716911972718148392453707898042946671553774030598713651389432173834332238513353580335392843797930178943386918304488493730840967156657148290968957715981554273773737487151449135620952308225431024688393136984555900143424679822610046551196808932727745248865362347785479364187372055325574195459037155066312293273886348144861748982170185415622553571530631513603477602826429579398186262265223153306278304799915076700814229178193555765145764377299909576623617487785999435363105546438656832847240507003602597491108906216981192670279162943412764046303699081784813538920115117298548433198843455119043790372888336933692344328141527872374759669746090941218187034798766305747971923638002946091334202545017363599031086846658957509235784541901412672981937055987278520433029602910026209333275313496848631869151490522436140352421940732910006747478399676998276993458833024795683746787074826108339213690383195100285198326586610540809574097037429381790444840835133521220930836457168264627708965665242143474257229651142989737540001394269465834767510321913987796958346807012067096569096845804007816516090656151634293085062792873308124403242170010908041
  )

genIdPConfig :: Gen a -> Gen (IdPConfig a)
genIdPConfig genExtra = do
  _idpId <- genIdPId
  _idpMetadataURI <- genHttps
  _idpMetadata <- genIdPMetadata
  _idpExtraInfo <- genExtra
  pure IdPConfig {..}

genFormRedirect :: Gen a -> Gen (FormRedirect a)
genFormRedirect genBody = FormRedirect <$> genHttps <*> genBody

genSimpleSetCookie :: forall (name :: Symbol). (KnownSymbol name) => Gen (SimpleSetCookie name)
genSimpleSetCookie = do
  val <- cs <$> genNiceWord
  path <-
    Gen.choice
      [ Just . cs . ST.intercalate "/" <$> Gen.list (Range.linear 0 3) genNiceWord,
        pure $ Just "/",
        pure Nothing
      ]
  expires <- Gen.maybe (THQ.quickcheck arbitrary <&> seconds %~ (* 10e12) . (/ 10e12)) -- only full seconds
  maxage <- Gen.maybe $ fromIntegral <$> Gen.int (Range.linear 0 1000) -- only non-negative, full seconds
  domain <- Gen.maybe (cs . ST.intercalate "." <$> Gen.list (Range.linear 2 3) genNiceWord)
  httponly <- Gen.bool
  secure <- Gen.bool
  samesite <- Gen.maybe $ Gen.element [sameSiteLax, sameSiteStrict]
  pure . SimpleSetCookie $
    def
      { setCookieName = cookieName (Proxy @name),
        setCookieValue = val,
        setCookiePath = path,
        setCookieExpires = expires,
        setCookieMaxAge = maxage,
        setCookieDomain = domain,
        setCookieHttpOnly = httponly,
        setCookieSecure = secure,
        setCookieSameSite = samesite
      }

{-
-- FUTUREWORK: this would be much more possible to implement if 'AuthnResponseBody' would be
-- defined with type parameters rather than existentially quantified types in
-- 'authnResponseBodyAction'.)
genAuthnResponseBody :: Gen AuthnResponseBody
genAuthnResponseBody = do
  aresp <- genAuthnResponse
  idp <- genIdPConfig (pure ())
  raw <- genRawAuthnResponseBody
  pure (AuthnResponseBody (\_ -> pure (aresp, idp)) raw)
-}

genRawAuthnResponseBody :: Gen (MultipartData Mem)
genRawAuthnResponseBody = do
  raw <- Gen.text (Range.linear 50 100) Gen.ascii
  pure MultipartData {files = [], inputs = [Input {iName = "SAMLResponse", iValue = raw}]}

-- FUTUREWORK: the following could be TH-generated entirely (take all declarations matching '^gen' and
-- turn the resp. types into Arbitrary instances).

instance Arbitrary UserRef where
  arbitrary = UserRef <$> arbitrary <*> arbitrary

instance Arbitrary (MultipartData Mem) where
  arbitrary = TQH.hedgehog genRawAuthnResponseBody

instance Arbitrary Assertion where
  arbitrary = TQH.hedgehog genAssertion

instance Arbitrary AuthnRequest where
  arbitrary = TQH.hedgehog genAuthnRequest

instance Arbitrary Conditions where
  arbitrary = TQH.hedgehog genConditions

instance Arbitrary Config where
  arbitrary = TQH.hedgehog genConfig

instance Arbitrary Duration where
  arbitrary = TQH.hedgehog genDuration

instance Arbitrary Issuer where
  arbitrary = TQH.hedgehog genIssuer

instance Arbitrary Locality where
  arbitrary = TQH.hedgehog genLocality

instance Arbitrary NameID where
  arbitrary = TQH.hedgehog genNameID

instance (Arbitrary payload) => Arbitrary (Response payload) where
  arbitrary = TQH.hedgehog (genResponse $ THQ.quickcheck arbitrary)

instance Arbitrary SubjectConfirmationData where
  arbitrary = TQH.hedgehog genSubjectConfirmationData

instance Arbitrary SubjectConfirmationMethod where
  arbitrary = TQH.hedgehog genSubjectConfirmationMethod

instance Arbitrary Time where
  arbitrary = TQH.hedgehog genTime

instance Arbitrary UnqualifiedNameID where
  arbitrary = TQH.hedgehog genUnqualifiedNameID

instance Arbitrary URI where
  arbitrary = TQH.hedgehog genHttps

instance Arbitrary IdPId where
  arbitrary = TQH.hedgehog genIdPId

instance Arbitrary X509.SignedCertificate where
  arbitrary = TQH.hedgehog genSignedCertificate

instance (Arbitrary a) => Arbitrary (IdPConfig a) where
  arbitrary = TQH.hedgehog (genIdPConfig (THQ.quickcheck arbitrary))

instance (Arbitrary a) => Arbitrary (FormRedirect a) where
  arbitrary = TQH.hedgehog (genFormRedirect (THQ.quickcheck arbitrary))

instance Arbitrary Document where
  arbitrary = TQH.hedgehog genXMLDocument
  shrink (Document pro el epi) = (\el' -> Document pro el' epi) <$> shrinkElement el

instance Arbitrary Node where
  arbitrary = TQH.hedgehog genXMLNode
  shrink = shrinkNode

instance Arbitrary Name where
  arbitrary = TQH.hedgehog genXMLName

instance Arbitrary IdPMetadata where
  arbitrary = TQH.hedgehog genIdPMetadata

shrinkElement :: Element -> [Element]
shrinkElement (Element tag attrs nodes) = case (shrinkAttrs attrs, shrink nodes) of
  ([], []) -> []
  (attrs', []) -> (\shrunk -> Element tag shrunk nodes) <$> attrs'
  ([], nodes') -> (\shrunk -> Element tag attrs shrunk) <$> nodes'
  (attrs', nodes') -> Element tag <$> attrs' <*> nodes'

shrinkAttrs :: Map.Map Name ST.Text -> [Map.Map Name ST.Text]
shrinkAttrs = fmap Map.fromList . shallowShrinkList . Map.toList

shrinkNode :: Node -> [Node]
shrinkNode (NodeElement el) = NodeElement <$> shrinkElement el
shrinkNode (NodeInstruction _) = []
shrinkNode (NodeContent "") = []
shrinkNode (NodeContent _) = [NodeContent ""]
shrinkNode (NodeComment "") = []
shrinkNode (NodeComment _) = [NodeComment ""]

shallowShrinkList :: (Eq a) => [a] -> [[a]]
shallowShrinkList [] = []
shallowShrinkList [_] = []
shallowShrinkList xs@(_ : _ : _) = [] : ((: []) <$> xs)

-- copied from from lens-datetime

diffTOD :: Iso' DiffTime TimeOfDay
diffTOD = iso timeToTimeOfDay timeOfDayToTime

timeAsDiff :: Lens' UTCTime DiffTime
timeAsDiff f (UTCTime d t) = UTCTime d <$> f t

-- | Lens into the second value of a 'Timeable'.
--
-- Warning: this is not a proper lens for 'UTCTime': it only obeys the
-- lens laws if used with valid values.
seconds :: Lens' UTCTime Pico
seconds = timeAsDiff . diffTOD . seconds'
  where
    seconds' f (TimeOfDay h m s) = TimeOfDay h m <$> f s
