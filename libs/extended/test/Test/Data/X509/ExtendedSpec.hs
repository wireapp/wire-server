module Test.Data.X509.ExtendedSpec where

import Crypto.Hash.Algorithms (SHA256 (SHA256))
import Crypto.PubKey.RSA qualified as RSA
import Crypto.PubKey.RSA.PKCS15 qualified as PKCS15
import Data.ASN1.OID (getObjectID)
import Data.ByteString qualified as BS
import Data.Hourglass
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

      it "should handle empty issuer and subject" $ do
        let dn = DistinguishedName []
            certString = certToString $ mkSignedCert dn dn
        certString `shouldStartWith` "Issuer: ; Subject: ; SHA1 Fingerprint:"

      it "should handle certificate with all possible DistinguishedName fields" $ do
        let issuerDN =
              DistinguishedName
                [ (getObjectID DnCommonName, fromString "Wire Root CA"),
                  (getObjectID DnOrganization, fromString "Wire Swiss GmbH"),
                  (getObjectID DnOrganizationUnit, fromString "Engineering"),
                  (getObjectID DnCountry, fromString "CH"),
                  (getObjectID DnEmailAddress, fromString "ca@wire.com")
                ]
            subjectDN =
              DistinguishedName
                [ (getObjectID DnCommonName, fromString "api.wire.com"),
                  (getObjectID DnOrganization, fromString "Wire Germany GmbH"),
                  (getObjectID DnOrganizationUnit, fromString "Backend Services"),
                  (getObjectID DnCountry, fromString "DE"),
                  (getObjectID DnEmailAddress, fromString "admin@wire.com")
                ]
            certString = certToString $ mkSignedCert issuerDN subjectDN
        certString
          `shouldStartWith` "Issuer: CN=Wire Root CA,O=Wire Swiss GmbH,OU=Engineering,Country=CH,Email Address=ca@wire.com;"
            ++ " Subject: CN=api.wire.com,O=Wire Germany GmbH,OU=Backend Services,Country=DE,Email Address=admin@wire.com; SHA1 Fingerprint:"

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

      it "should handle empty issuer and subject" $ do
        let dn = DistinguishedName []
            desc = certDescription $ mkSignedCert dn dn
        desc.fingerprintAlgorithm `shouldBe` "SHA1"
        desc.fingerprint `shouldNotBe` ""
        desc.subject `shouldBe` ""
        desc.issuer `shouldBe` ""

      it "should handle certificate with all possible DistinguishedName fields" $ do
        let issuerDN =
              DistinguishedName
                [ (getObjectID DnCommonName, fromString "Wire Root CA"),
                  (getObjectID DnOrganization, fromString "Wire Swiss GmbH"),
                  (getObjectID DnOrganizationUnit, fromString "Engineering"),
                  (getObjectID DnCountry, fromString "CH"),
                  (getObjectID DnEmailAddress, fromString "ca@wire.com")
                ]
            subjectDN =
              DistinguishedName
                [ (getObjectID DnCommonName, fromString "api.wire.com"),
                  (getObjectID DnOrganization, fromString "Wire Germany GmbH"),
                  (getObjectID DnOrganizationUnit, fromString "Backend Services"),
                  (getObjectID DnCountry, fromString "DE"),
                  (getObjectID DnEmailAddress, fromString "admin@wire.com")
                ]
            desc = certDescription $ mkSignedCert issuerDN subjectDN
        desc.fingerprintAlgorithm `shouldBe` "SHA1"
        desc.fingerprint `shouldNotBe` ""
        desc.issuer `shouldBe` "CN=Wire Root CA,O=Wire Swiss GmbH,OU=Engineering,Country=CH,Email Address=ca@wire.com"
        desc.subject `shouldBe` "CN=api.wire.com,O=Wire Germany GmbH,OU=Backend Services,Country=DE,Email Address=admin@wire.com"

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

-- | create a (self) signed certificate (inspired by `Testlib.Certs`)
mkSignedCert ::
  (HasCallStack) =>
  DistinguishedName ->
  DistinguishedName ->
  SignedExact Certificate
mkSignedCert distinguishedNameIssuer distinguishedNameSubject =
  let (pubKey, privKey) = mkKeyPair
   in fst $
        objectToSignedExact
          (\msg -> (signMsgWithPrivateKey privKey msg, SignatureALG HashSHA256 PubKeyALG_RSA, ()))
          Certificate
            { certVersion = 3,
              certSerial = 1,
              certSignatureAlg = SignatureALG HashSHA256 PubKeyALG_RSA,
              certIssuerDN = distinguishedNameIssuer,
              certValidity = (DateTime {dtDate = Date 2000 January 1, dtTime = midNight}, DateTime {dtDate = Date 2049 January 1, dtTime = midNight}),
              certSubjectDN = distinguishedNameSubject,
              certPubKey = PubKeyRSA pubKey,
              certExtensions = Extensions Nothing
            }
  where
    midNight = TimeOfDay 0 0 0 0

    -- the minimum key size is hard coded to be 256 bytes (= 2048 bits)
    mkKeyPair :: (RSA.PublicKey, RSA.PrivateKey)
    mkKeyPair = case (RSA.generateWith primesA 2048 65537) of
      Just k -> k
      Nothing -> error "Failed to generate key pair"

    primesA :: (Integer, Integer)
    primesA =
      ( 1013416710455617992060044810859399709890835129925648843043641673852539448350775594187007527506724875627885909523835606557173980236290013476205929897072239944138314384631600538474898358198731711608598716779857515154388088878657555928549962380829213547435085854695442354636327047821108802590374275481605077802187415357974963365435650338024405558985202998762641404395411587629314013330411500470203761301812113710962088477051775450894192994742118846780105265558368972170180276350636994878636389758206123738715722878057404540464220733023391993383290494652037274532356460190907090422144536951440069212998822960155765054879900781581263606916652700903953626527029121897494538017122565993895036773799860052414697053960902764894046849087727915659738623914130083281919853081537137782445589156217286369690178786653090799221857147470043219175767249163571686740347462294750028790472737772761949491538873890614496706566060247820117584298845501935064037819052405654373374661838572553244593002834443762478259268799467895951456315647324157054992319938064879914915556645111272573189405077515029783954913337757933225821260787418411247627537065834022908147122036442414923430533383989652364612738513379313521406363716216150953874675705623133860932309998632104801092827841702718992714882139811954467163400593020720191718049863114367363094097654194786896842879463158349468509662084081492854544553121389587671952367596127566679408181243898540691657673709282297206699665271972122876732477153246545187514721891966873910637813569799235783300883640120382296336980469139678449923244327325676463743789034561023783533980749100272005938046751700931286800296518645750336292219055157506140422334232031499441618108378207249469768514341014613604798707882336528213109908520952809254346958192134161621644423814067058523341464457188689237566854457651740962437154879472377563420329379777383724869785437079461381042576932777663816932792106785972722313112138774627384189872028788531464434347861094422498231096686231475413078333450041613628998736286930594422166708703115486915826404578851616898264340560519310655180870217752558303339822824214706404615558734661262111177357709447064658518593459191904042065215329175588893364731436963818899069593653897213811368511785916948261704025900054681973429106441628584851712758726618885443787735678619865846520873765930283904988556631550968487727144405349504203063775775239807234977371854786517646240982498594502233136236903225375658288185007963323167751702824125884605983,
        927336758709169856221729309972684377326012758705584701160913392855296574209188805952293975727392736357355525822682625960867980784906333126250176772633612511280160520450355917665344680820117001909657304528897728644985372222487760541890997744380957145384918405839817509991111341989419216342513467094263440712622240826707558561965237909070383875063686755789716081493927682670013715434239129366779748040394792694841549258598842315715859562294976974200564408338450316192760863885386436881465495436476022429943600686139972778561942722494137924396693749231870673494020761865863446686474725091312431012619078931330640808188498974525508440925548025604310429878232463952454557835744654770844144316962049844107999645072674978011865146180434315809137160022154815275730622923394822959089495198091753080586758917401240837851455881168916390487103230014598246305055773428160686563500509562651266122967947533947385066722712316194439650272469880653336775557226431438158529031941085177895035782278423238393385871537920481620086314516883242108371084035236009476902958675684122414056114458154814623140680549398143962297844269217544119579639388880282746926211911340151495180800938356829417651851575812389707158878607136197574826859775996273379970390171328581948608028025142182853278853363612390290636206287758711077096741448655899931751827724488361988091582792716911972718148392453707898042946671553774030598713651389432173834332238513353580335392843797930178943386918304488493730840967156657148290968957715981554273773737487151449135620952308225431024688393136984555900143424679822610046551196808932727745248865362347785479364187372055325574195459037155066312293273886348144861748982170185415622553571530631513603477602826429579398186262265223153306278304799915076700814229178193555765145764377299909576623617487785999435363105546438656832847240507003602597491108906216981192670279162943412764046303699081784813538920115117298548433198843455119043790372888336933692344328141527872374759669746090941218187034798766305747971923638002946091334202545017363599031086846658957509235784541901412672981937055987278520433029602910026209333275313496848631869151490522436140352421940732910006747478399676998276993458833024795683746787074826108339213690383195100285198326586610540809574097037429381790444840835133521220930836457168264627708965665242143474257229651142989737540001394269465834767510321913987796958346807012067096569096845804007816516090656151634293085062792873308124403242170010908041
      )
    signMsgWithPrivateKey :: (HasCallStack) => RSA.PrivateKey -> ByteString -> ByteString
    signMsgWithPrivateKey privKey = fromRight (error "signing unsuccessful") . PKCS15.sign Nothing (Just SHA256) privKey
