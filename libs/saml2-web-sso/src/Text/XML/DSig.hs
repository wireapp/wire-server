{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Partial implementation of <https://www.w3.org/TR/xmldsig-core/>.  We use hsaml2, hxt, x509 and
-- other dubious packages internally, but expose xml-types and cryptonite.
--
-- FUTUREWORK: other implementations that could be used for testing:
-- https://www.aleksey.com/xmlsec/ (C);
-- https://github.com/yaronn/xml-crypto (js)
module Text.XML.DSig
  ( -- * types
    SignCreds (..),
    SignDigest (..),
    SignKey (..),
    SignPrivCreds (..),
    SignPrivKey (..),

    -- * credential handling
    verifySelfSignature,
    parseKeyInfo,
    renderKeyInfo,
    certToCreds,
    certToPublicKey,
    mkSignCreds,
    mkSignCredsWithCert,

    -- * signature verification
    verify,
    verifyRoot,
    verifyIO,

    -- * signature creation
    signRoot,
    signRootAt,

    -- * testing
    HasMonadSign,
    MonadSign (MonadSign),
    runMonadSign,
    signElementIO,
    signElementIOAt,
  )
where

import Control.Exception (ErrorCall (ErrorCall), SomeException, throwIO, try)
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import qualified Crypto.Hash as Crypto
import qualified Crypto.PubKey.RSA as RSA
import qualified Crypto.PubKey.RSA.PKCS15 as RSA
import qualified Crypto.PubKey.RSA.Types as RSA
import qualified Crypto.Random.Types as Crypto
import qualified Data.ByteArray as ByteArray
import Data.Either (isRight)
import Data.EitherR (fmapL)
import Data.Foldable (toList)
import qualified Data.Hourglass as Hourglass
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NL
import qualified Data.Map as Map
import Data.String.Conversions
import Data.UUID as UUID
import qualified Data.X509 as X509
import GHC.Stack
import Network.URI (URI, parseRelativeReference)
import qualified SAML2.XML as HS hiding (Node, URI)
import qualified SAML2.XML.Canonical as HS
import qualified SAML2.XML.Signature as HS
import System.IO (stderr, stdout)
import System.IO.Silently (hCapture)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (mkStdGen, random)
import Text.XML as XML
import qualified Text.XML.HXT.DOM.XmlNode as HXT
import Text.XML.Util
import qualified Time.System as Hourglass

----------------------------------------------------------------------
-- types

data SignCreds = SignCreds SignDigest SignKey
  deriving (Eq, Show)

data SignDigest = SignDigestSha256
  deriving (Eq, Show, Bounded, Enum)

data SignKey = SignKeyRSA RSA.PublicKey
  deriving (Eq, Show)

data SignPrivCreds = SignPrivCreds SignDigest SignPrivKey
  deriving (Eq, Show)

data SignPrivKey = SignPrivKeyRSA RSA.KeyPair
  deriving (Eq, Show)

----------------------------------------------------------------------
-- credential handling

verifySelfSignature :: (HasCallStack, MonadError String m) => X509.SignedCertificate -> m ()
verifySelfSignature cert = do
  certToCreds cert >>= \case
    SignCreds SignDigestSha256 (SignKeyRSA pubkey) -> do
      let signedMessage = X509.getSignedData cert
          signatureValue = X509.signedSignature $ X509.getSigned cert
      unless (RSA.verify (Just Crypto.SHA256) pubkey signedMessage signatureValue) $
        throwError "verifySelfSignature: invalid signature."

-- | Read the KeyInfo element of a meta file's IDPSSODescriptor into a public key that can be used
-- for signing.  Tested for KeyInfo elements that contain an x509 certificate.
--
-- Self-signatures are only verified if first argument is 'True'.  The reason for this flag is
-- that some IdPs (e.g. centrify) sign their certificates with external CAs.  Verification
-- against external cert needs to be done separately before calling this function.
parseKeyInfo :: (HasCallStack, MonadError String m) => Bool -> LT -> m X509.SignedCertificate
parseKeyInfo doVerify (cs @LT @LBS -> lbs) = case HS.xmlToSAML @HS.KeyInfo =<< stripWhitespaceLBS lbs of
  Right keyinf -> case filter (not . ignorable) . toList $ HS.keyInfoElements keyinf of
    [HS.X509Data dt] ->
      parseX509Data dt
    [] ->
      throwError $ "KeyInfo element must contain X509Data"
    unsupported ->
      throwError $ "unsupported children in KeyInfo element: " <> show unsupported
  Left errmsg ->
    throwError $ "expected exactly one KeyInfo element: " <> errmsg
  where
    ignorable (HS.KeyName _) = True
    ignorable _ = False
    parseX509Data (HS.X509Certificate cert :| []) =
      when doVerify (verifySelfSignature cert) >> pure cert
    parseX509Data bad =
      throwError $ "data with more than one child: " <> show (toList bad)

-- | Call 'stripWhitespaceDoc' on a rendered bytestring.
stripWhitespaceLBS :: (m ~ Either String) => LBS -> m LBS
stripWhitespaceLBS lbs = renderLBS def . stripWhitespace <$> fmapL show (parseLBS def lbs)

renderKeyInfo :: (HasCallStack) => X509.SignedCertificate -> LT
renderKeyInfo cert = cs . HS.samlToXML . HS.KeyInfo Nothing $ HS.X509Data (HS.X509Certificate cert :| []) :| []

certToCreds :: (HasCallStack, MonadError String m) => X509.SignedCertificate -> m SignCreds
certToCreds cert = do
  digest <- case X509.signedAlg $ X509.getSigned cert of
    X509.SignatureALG X509.HashSHA256 X509.PubKeyALG_RSA -> pure SignDigestSha256
    bad -> throwError $ "unsupported: " <> show bad
  key <- case X509.certPubKey . X509.signedObject $ X509.getSigned cert of
    X509.PubKeyRSA pk -> pure $ SignKeyRSA pk
    bad -> throwError $ "unsupported: " <> show bad
  pure $ SignCreds digest key

certToPublicKey :: (HasCallStack, MonadError String m) => X509.SignedCertificate -> m RSA.PublicKey
certToPublicKey cert = certToCreds cert <&> \(SignCreds _ (SignKeyRSA key)) -> key

mkSignCreds :: (Crypto.MonadRandom m, MonadIO m) => Int -> m (SignPrivCreds, SignCreds)
mkSignCreds size = mkSignCredsWithCert Nothing size <&> \(priv, pub, _) -> (priv, pub)

-- | If first argument @validSince@ is @Nothing@, use cucrent system time.
mkSignCredsWithCert ::
  forall m.
  (Crypto.MonadRandom m, MonadIO m) =>
  Maybe Hourglass.DateTime ->
  Int ->
  m (SignPrivCreds, SignCreds, X509.SignedCertificate)
mkSignCredsWithCert mValidSince size = do
  let rsaexp = 17
  (pubkey, privkey) <- RSA.generate size rsaexp
  let -- https://github.com/vincenthz/hs-certificate/issues/119
      cropToSecs :: Hourglass.DateTime -> Hourglass.DateTime
      cropToSecs dt = dt {Hourglass.dtTime = (Hourglass.dtTime dt) {Hourglass.todNSec = 0}}
  validSince :: Hourglass.DateTime <- cropToSecs <$> maybe (liftIO Hourglass.dateCurrent) pure mValidSince
  let validUntil = validSince `Hourglass.timeAdd` mempty {Hourglass.durationHours = 24 * 365 * 20}
      signcert :: SBS -> m (SBS, X509.SignatureALG)
      signcert sbs = (,sigalg) <$> sigval
        where
          sigalg = X509.SignatureALG X509.HashSHA256 X509.PubKeyALG_RSA
          sigval :: m SBS =
            liftIO $
              RSA.signSafer (Just Crypto.SHA256) privkey sbs
                >>= either (throwIO . ErrorCall . show) pure
  cert <-
    X509.objectToSignedExactF
      signcert
      X509.Certificate
        { X509.certVersion = 2 :: Int,
          X509.certSerial = 387928798798718181888591698169861 :: Integer,
          X509.certSignatureAlg = X509.SignatureALG X509.HashSHA256 X509.PubKeyALG_RSA,
          X509.certIssuerDN = X509.DistinguishedName [],
          X509.certValidity = (validSince, validUntil),
          X509.certSubjectDN = X509.DistinguishedName [],
          X509.certPubKey = X509.PubKeyRSA pubkey,
          X509.certExtensions = X509.Extensions Nothing
        }
  pure
    ( SignPrivCreds SignDigestSha256 . SignPrivKeyRSA $ RSA.KeyPair privkey,
      SignCreds SignDigestSha256 . SignKeyRSA $ pubkey,
      cert
    )

----------------------------------------------------------------------
-- signature verification

-- | We sometimes get XML documents that are underspecific about which credentials they are going to
-- use later.  As longs as all credentials are from the same authoritative source, it may be ok to
-- ask for *any* of them to match a signature.  So here is an @or@ over 'verify' and a non-empty
-- list of 'SignCred's.
--
-- NB: The call to 'unsafePerformIO' in this function is sound under the assumption that
-- 'verifyIO' has no effects in 'IO' other than throwing 'SomeException' (which are captured
-- by 'try'.  Technically, it does have other effects, like opening temp files for capturing
-- stderr (if any), but we do not care about those.  The only thing we care about is that the
-- conceptually pure function of validating a signature will either be called twice with the
-- same arguments and return the same result value, or not be called a second time with the
-- same arguments, in which case that same value will be used.
{-# NOINLINE verify #-}
verify :: forall m. (MonadError String m) => NonEmpty SignCreds -> LBS -> String -> m ()
verify creds el signedID = case unsafePerformIO (try @SomeException $ verifyIO creds el signedID) of
  Right [] -> pure ()
  Right errs -> throwError $ show (snd <$> errs)
  Left exc -> throwError $ show exc

verifyRoot :: forall m. (MonadError String m) => NonEmpty SignCreds -> LBS -> m ()
verifyRoot creds el = do
  signedID <- do
    XML.Document _ (XML.Element _ attrs _) _ <-
      either
        (throwError . ("Could not parse signed document: " <>) . cs . show)
        pure
        (XML.parseLBS XML.def el)
    maybe
      (throwError $ "Could not parse signed document: no ID attribute in root element." <> show el)
      (pure . cs)
      (Map.lookup "ID" attrs)
  verify creds el signedID

-- | Try a list of creds against a document.  If all fail, return a list of errors for each cert; if
-- *any* succeed, return the empty list.
verifyIO :: NonEmpty SignCreds -> LBS -> String -> IO [(SignCreds, Either HS.SignatureError ())]
verifyIO creds el signedID = capture' $ do
  results <- NL.zip creds <$> forM creds (\cred -> verifyIO' cred el signedID)
  case NL.filter (isRight . snd) results of
    (_ : _) -> pure []
    [] -> pure $ NL.toList results
  where
    capture' :: IO a -> IO a
    capture' action =
      hCapture [stdout, stderr] action >>= \case
        ("", out) -> pure out
        (noise, _) -> throwIO . ErrorCall $ "noise on stdout/stderr from hsaml2 package: " <> noise

verifyIO' :: SignCreds -> LBS -> String -> IO (Either HS.SignatureError ())
verifyIO' (SignCreds SignDigestSha256 (SignKeyRSA key)) el signedID = runExceptT $ do
  el' <- either (throwError . HS.SignatureParseError) pure $ HS.xmlToDocE el
  ExceptT $ HS.verifySignatureUnenvelopedSigs (HS.PublicKeys Nothing . Just $ key) signedID el'

----------------------------------------------------------------------
-- signature creation

-- | Make sure that root node node has ID attribute and sign it.  This is similar to the more
-- primitive 'HS.generateSignature'.  Cons signature to the children list (left-most position).
signRoot :: (Crypto.MonadRandom m, MonadError String m) => SignPrivCreds -> XML.Document -> m XML.Document
signRoot = signRootAt 0

-- | Like 'signRoot', but insert signature at any given position in the children list.  If the list
-- is too short for this position, throw an error.
signRootAt :: (Crypto.MonadRandom m, MonadError String m) => Int -> SignPrivCreds -> XML.Document -> m XML.Document
signRootAt sigPos (SignPrivCreds hashAlg (SignPrivKeyRSA keypair)) doc =
  do
    (docWithID :: XML.Document, reference) <- addRootIDIfMissing doc
    docInHXT <- conduitToHxt docWithID
    let canoAlg = HS.CanonicalXMLExcl10 True
        transforms =
          Just . HS.Transforms $
            HS.Transform
              { HS.transformAlgorithm = HS.Identified HS.TransformEnvelopedSignature,
                HS.transformInclusiveNamespaces = Nothing,
                HS.transform = []
              }
              :| [ HS.Transform
                     { HS.transformAlgorithm = HS.Identified (HS.TransformCanonicalization canoAlg),
                       HS.transformInclusiveNamespaces = Nothing,
                       HS.transform = []
                     }
                 ]
    docCanonic :: SBS <-
      either (throwError . show) (pure . cs) . unsafePerformIO . try @SomeException $
        HS.applyTransforms transforms (HXT.mkRoot [] [docInHXT])
    let digest :: SBS
        digest = case hashAlg of
          SignDigestSha256 -> ByteArray.convert $ Crypto.hash @SBS @Crypto.SHA256 docCanonic
    let signedInfo =
          HS.SignedInfo
            { signedInfoId = Nothing :: Maybe HS.ID,
              signedInfoCanonicalizationMethod = HS.CanonicalizationMethod (HS.Identified canoAlg) Nothing [],
              signedInfoSignatureMethod = HS.SignatureMethod (HS.Identified HS.SignatureRSA_SHA256) Nothing [],
              signedInfoReference =
                HS.Reference
                  { referenceId = Nothing,
                    referenceURI = Just reference,
                    referenceType = Nothing,
                    referenceTransforms = transforms,
                    referenceDigestMethod = HS.DigestMethod (HS.Identified HS.DigestSHA256) [],
                    referenceDigestValue = digest
                  }
                  :| []
            }
    -- (note that there are two rounds of SHA256 application, hence two mentions of the has alg here)

    signedInfoSBS :: SBS <-
      either (throwError . show) (pure . cs) . unsafePerformIO . try @SomeException $
        HS.applyCanonicalization (HS.signedInfoCanonicalizationMethod signedInfo) Nothing $
          HS.samlToDoc signedInfo
    sigval :: SBS <-
      either (throwError . show @RSA.Error) pure
        =<< RSA.signSafer
          (Just Crypto.SHA256)
          (RSA.toPrivateKey keypair)
          signedInfoSBS
    let sig =
          HS.Signature
            { signatureId = Nothing :: Maybe HS.ID,
              signatureSignedInfo = signedInfo :: HS.SignedInfo,
              signatureSignatureValue = HS.SignatureValue Nothing sigval :: HS.SignatureValue,
              signatureKeyInfo = Nothing :: Maybe HS.KeyInfo,
              signatureObject = []
            }
    unless (RSA.verify (Just Crypto.SHA256) (RSA.toPublicKey keypair) signedInfoSBS sigval) $
      throwError "signRoot: internal error: failed to verify my own signature!"
    injectSignedInfoAtRoot sigPos sig =<< hxtToConduit docInHXT

addRootIDIfMissing :: forall m. (MonadError String m, Crypto.MonadRandom m) => XML.Document -> m (XML.Document, URI)
addRootIDIfMissing (XML.Document prol (Element tag attrs nodes) epil) = do
  (fresh, ref) <- maybe makeID keepID $ Map.lookup "ID" attrs
  uriref <- maybe (throwError "bad reference URI") pure . parseRelativeReference . cs $ "#" <> ref
  let updAttrs = if fresh then Map.insert "ID" ref else id
  pure (XML.Document prol (Element tag (updAttrs attrs) nodes) epil, uriref)
  where
    makeID :: m (Bool, ST)
    makeID = (True,) . UUID.toText <$> randomUUID
    keepID :: ST -> m (Bool, ST)
    keepID = pure . (False,)

randomUUID :: (Crypto.MonadRandom m) => m UUID.UUID
randomUUID = fst . random . mkStdGen . fromIntegral <$> randomInteger

-- | (uses 64 bits of entropy)
randomInteger :: (Crypto.MonadRandom m) => m Integer
randomInteger =
  ( Crypto.getRandomBytes 8
      <&> ByteArray.unpack @ByteArray.Bytes
  )
    <&> foldl' (*) 1 . fmap fromIntegral

injectSignedInfoAtRoot :: (MonadError String m) => Int -> HS.Signature -> XML.Document -> m XML.Document
injectSignedInfoAtRoot sigPos signedInfo (XML.Document prol (Element tag attrs nodes) epil) = do
  when (sigPos > Prelude.length nodes) $ do
    throwError ("child list too short: is " <> show (Prelude.length nodes) <> ", need " <> show sigPos)
  XML.Document _ signedInfoXML _ <- samlToConduit signedInfo
  pure $ XML.Document prol (Element tag attrs (insertAt sigPos (XML.NodeElement signedInfoXML) nodes)) epil
  where
    insertAt :: Int -> a -> [a] -> [a]
    insertAt pos el els = case Prelude.splitAt pos els of (prefix, suffix) -> prefix <> [el] <> suffix

----------------------------------------------------------------------
-- testing

newtype MonadSign a = MonadSign {runMonadSign' :: ExceptT String IO a}
  deriving (Functor, Applicative, Monad)

runMonadSign :: MonadSign a -> IO (Either String a)
runMonadSign = runExceptT . runMonadSign'

instance Crypto.MonadRandom MonadSign where
  getRandomBytes l = MonadSign . ExceptT $ Right <$> Crypto.getRandomBytes l

instance MonadError String MonadSign where
  throwError = MonadSign . throwError
  catchError (MonadSign m) handler = MonadSign $ m `catchError` (runMonadSign' . handler)

type HasMonadSign = MonadIO

signElementIO :: (HasCallStack, HasMonadSign m) => SignPrivCreds -> [Node] -> m [Node]
signElementIO = signElementIOAt 0

signElementIOAt :: (HasCallStack, HasMonadSign m) => Int -> SignPrivCreds -> [Node] -> m [Node]
signElementIOAt sigPos creds [NodeElement el] = do
  eNodes :: Either String [Node] <-
    liftIO . runMonadSign . fmap docToNodes . signRootAt sigPos creds . mkDocument $ el
  either error pure eNodes
signElementIOAt _ _ bad = liftIO . throwIO . ErrorCall . show $ bad
