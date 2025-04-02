{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module SAML2.WebSSO.Test.Arbitrary where

import Control.Lens
import Data.CaseInsensitive qualified as CI
import Data.Fixed
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
  _cfgSPAppURI <- genHttps
  _cfgSPSsoURI <- genHttps
  _cfgContacts <- Gen.list (Range.linear 0 3) genSPContactPerson
  pure Config {..}

genSPContactPerson :: Gen ContactPerson
genSPContactPerson =
  ContactPerson
    <$> Gen.enumBounded
    <*> Gen.maybe (mkXmlText <$> genNiceWord)
    <*> Gen.maybe (mkXmlText <$> genNiceWord)
    <*> Gen.maybe (mkXmlText <$> genNiceWord)
    <*> Gen.maybe genHttps
    <*> Gen.maybe (mkXmlText <$> genNiceWord)

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
  _spOrgName <- mkXmlText <$> genNiceWord
  _spOrgDisplayName <- mkXmlText <$> genNiceWord
  _spOrgURL <- genHttps
  _spResponseURL <- genHttps
  _spContacts <- Gen.list (Range.linear 0 3) genContactPerson
  pure SPMetadata {..}

genContactPerson :: Gen ContactPerson
genContactPerson = do
  _cntType <- Gen.enumBounded
  _cntCompany <- Gen.maybe (mkXmlText <$> genNiceWord)
  _cntGivenName <- Gen.maybe (mkXmlText <$> genNiceWord)
  _cntSurname <- Gen.maybe (mkXmlText <$> genNiceWord)
  _cntEmail <- Gen.maybe genEmailURI
  _cntPhone <- Gen.maybe (mkXmlText <$> genNiceWord)
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
genID = mkID . ("_" <>) . UUID.toText <$> genUUID

genIssuer :: Gen Issuer
genIssuer = Issuer <$> genHttps

genNameIDPolicy :: Gen NameIdPolicy
genNameIDPolicy =
  NameIdPolicy
    <$> genNameIDFormat
    <*> Gen.maybe (mkXmlText <$> genNiceWord)
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
    mktxt charlen = mkXmlText <$> Gen.text (Range.linear 1 charlen) Gen.alpha

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
    either (error . show) pure . mkIP
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
  _astSessionIndex <- Gen.maybe (mkXmlText <$> genNiceWord)
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

genSignedCertificate :: Gen X509.SignedCertificate
genSignedCertificate =
  either (error . show) pure $
    DSig.parseKeyInfo
      False
      "<KeyInfo xmlns=\"http://www.w3.org/2000/09/xmldsig#\"><X509Data><X509Certificate>MIIDBTCCAe2gAwIBAgIQev76BWqjWZxChmKkGqoAfDANBgkqhkiG9w0BAQsFADAtMSswKQYDVQQDEyJhY2NvdW50cy5hY2Nlc3Njb250cm9sLndpbmRvd3MubmV0MB4XDTE4MDIxODAwMDAwMFoXDTIwMDIxOTAwMDAwMFowLTErMCkGA1UEAxMiYWNjb3VudHMuYWNjZXNzY29udHJvbC53aW5kb3dzLm5ldDCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAMgmGiRfLh6Fdi99XI2VA3XKHStWNRLEy5Aw/gxFxchnh2kPdk/bejFOs2swcx7yUWqxujjCNRsLBcWfaKUlTnrkY7i9x9noZlMrijgJy/Lk+HH5HX24PQCDf+twjnHHxZ9G6/8VLM2e5ZBeZm+t7M3vhuumEHG3UwloLF6cUeuPdW+exnOB1U1fHBIFOG8ns4SSIoq6zw5rdt0CSI6+l7b1DEjVvPLtJF+zyjlJ1Qp7NgBvAwdiPiRMU4l8IRVbuSVKoKYJoyJ4L3eXsjczoBSTJ6VjV2mygz96DC70MY3avccFrk7tCEC6ZlMRBfY1XPLyldT7tsR3EuzjecSa1M8CAwEAAaMhMB8wHQYDVR0OBBYEFIks1srixjpSLXeiR8zES5cTY6fBMA0GCSqGSIb3DQEBCwUAA4IBAQCKthfK4C31DMuDyQZVS3F7+4Evld3hjiwqu2uGDK+qFZas/D/eDunxsFpiwqC01RIMFFN8yvmMjHphLHiBHWxcBTS+tm7AhmAvWMdxO5lzJLS+UWAyPF5ICROe8Mu9iNJiO5JlCo0Wpui9RbB1C81Xhax1gWHK245ESL6k7YWvyMYWrGqr1NuQcNS0B/AIT1Nsj1WY7efMJQOmnMHkPUTWryVZlthijYyd7P2Gz6rY5a81DAFqhDNJl2pGIAE6HWtSzeUEh3jCsHEkoglKfm4VrGJEuXcALmfCMbdfTvtu4rlsaP2hQad+MG/KJFlenoTK34EMHeBPDCpqNDz8UVNk</X509Certificate></X509Data></KeyInfo>"

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
