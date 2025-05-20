{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- FUTUREWORK: consider using http://hackage.haskell.org/package/xml-conduit-parse

module SAML2.WebSSO.XML
  ( HasXML (..),
    HasXMLRoot (..),
    HasXMLImport (..),
    attributeIsCI,
    defNameSpaces,
    encode,
    decode,
    encodeElem,
    decodeElem,
    renderToDocument,
    parseFromDocument,
    parseFromXmlTree,
    unsafeReadTime,
    decodeTime,
    renderTime,
    explainDeniedReason,
    mkSPMetadata,
  )
where

import Control.Arrow ((>>>))
import Control.Category (Category (..))
import Control.Exception (SomeException)
import Control.Lens hiding (element)
import Control.Monad
import Control.Monad.Except
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI
import Data.EitherR
import Data.Foldable (toList)
import Data.Kind (Type)
import Data.List qualified as List
import Data.List.NonEmpty as NL (NonEmpty ((:|)), nonEmpty)
import Data.List.NonEmpty qualified as NL
import Data.Map qualified as Map
import Data.Maybe
import Data.String.Conversions
import Data.Text (Text)
import Data.Text qualified as ST
import Data.Text.Lazy.Encoding
import Data.Time
import Data.Typeable (Proxy (Proxy), Typeable)
import Data.X509 qualified as X509
import GHC.Stack
import Network.URI qualified as URI
import SAML2.Bindings.Identifiers qualified as HX
import SAML2.Core qualified as HX
import SAML2.Metadata.Metadata qualified as HX
import SAML2.Profiles qualified as HX
import SAML2.Util
import SAML2.WebSSO.SP
import SAML2.WebSSO.Types
import SAML2.WebSSO.Types.Email qualified as Email
import SAML2.XML qualified as HX
import SAML2.XML.Schema.Datatypes qualified as HX (Boolean, Duration, UnsignedShort)
import SAML2.XML.Signature.Types qualified as HX (Signature)
import Text.Hamlet.XML (xml)
import Text.XML
import Text.XML.Cursor
import Text.XML.DSig (parseKeyInfo, renderKeyInfo)
import Text.XML.HXT.Arrow.Pickle.Xml qualified as HXT
import Text.XML.HXT.DOM.TypeDefs (XmlTree)
import URI.ByteString as U
import Prelude hiding (id, (.))

defNameSpaces :: [(ST, ST)]
defNameSpaces =
  [ ("samlp", "urn:oasis:names:tc:SAML:2.0:protocol"),
    ("samla", "urn:oasis:names:tc:SAML:2.0:assertion"),
    ("samlm", "urn:oasis:names:tc:SAML:2.0:metadata"),
    ("ds", "http://www.w3.org/2000/09/xmldsig#")
  ]

----------------------------------------------------------------------
-- HasXML class

encode :: forall a. (HasXMLRoot a) => a -> LT
encode = Text.XML.renderText settings . renderToDocument
  where
    settings = def {rsNamespaces = nameSpaces (Proxy @a), rsXMLDeclaration = False}

decode :: forall m a. (HasXMLRoot a, MonadError String m) => LT -> m a
decode = either (throwError . show @SomeException) parseFromDocument . parseText def

encodeElem :: forall a. (HasXML a) => a -> LT
encodeElem = Text.XML.renderText settings . mkDocument' . render
  where
    settings = def {rsNamespaces = nameSpaces (Proxy @a), rsXMLDeclaration = False}
    mkDocument' [NodeElement el] = mkDocument el
    mkDocument' bad = error $ "encodeElem: " <> show bad

decodeElem :: forall a m. (HasXML a, MonadError String m) => LT -> m a
decodeElem = either (throwError . show @SomeException) parseFromDocument . parseText def

renderToDocument :: (HasXMLRoot a) => a -> Document
renderToDocument = mkDocument . renderRoot

parseFromDocument :: (HasXML a, MonadError String m) => Document -> m a
parseFromDocument doc = parse [NodeElement $ documentRoot doc]

parseFromXmlTree :: (MonadError String m, HasXML a) => XmlTree -> m a
parseFromXmlTree raw = do
  doc :: Document <- decode . decodeUtf8 $ ourDocToXMLWithRoot raw
  parseFromDocument doc

-- FUTUREWORK: perhaps we want to split this up: HasXML (for nameSpaces), and HasXMLParse, HasXMLRender,
-- and drop the assymetric, little used render function from HasXML?

class HasXML a where
  nameSpaces :: Proxy a -> [(ST, ST)]
  nameSpaces Proxy = defNameSpaces

  render :: a -> [Node]
  default render :: (HasXMLRoot a) => a -> [Node]
  render = (: []) . NodeElement . renderRoot

  parse :: (MonadError String m) => [Node] -> m a

class (HasXML a) => HasXMLRoot a where
  renderRoot :: a -> Element

instance HasXML Document where
  parse [NodeElement el] = pure $ Document defPrologue el defMiscellaneous
  parse bad = die (Proxy @Document) bad

instance HasXMLRoot Document where
  renderRoot (Document _ el _) = el

----------------------------------------------------------------------
-- util

-- | Attribute either is not present or has a different value.  Oppositve of 'attributeIs'.
attributeIsNot :: Name -> ST.Text -> Axis
attributeIsNot key val cur = [cur | null $ attributeIs key val cur]

-- | Do not use this in production!  It works, but it's slow and failures are a bit violent.
unsafeReadTime :: (HasCallStack) => String -> Time
unsafeReadTime s = either (error ("decodeTime: " <> show s)) id $ decodeTime s

decodeTime :: (MonadError String m, ConvertibleStrings s String) => s -> m Time
decodeTime (cs -> s) = case parseTimeM True defaultTimeLocale timeFormat s of
  Just t -> pure $ Time t
  Nothing -> die (Proxy @Time) (s, timeFormat)

renderTime :: Time -> ST
renderTime (Time utctime) =
  cs . accomodateMSAD $ formatTime defaultTimeLocale timeFormat utctime
  where
    -- more than 7 decimal points make Active Directory choke.
    accomodateMSAD :: String -> String
    accomodateMSAD s = case List.elemIndex '.' s of
      Nothing -> s
      Just i -> case List.splitAt i s of
        (t, u) -> case List.splitAt 8 u of
          (_, "") -> t <> u
          (v, _) -> t <> v <> "Z"

defAuthnRequest :: HX.ProtocolType -> HX.AuthnRequest
defAuthnRequest proto =
  HX.AuthnRequest
    { HX.authnRequest = HX.RequestAbstractType proto,
      HX.authnRequestForceAuthn = False,
      HX.authnRequestIsPassive = False,
      HX.authnRequestAssertionConsumerService = HX.AssertionConsumerServiceURL Nothing Nothing,
      HX.authnRequestAssertionConsumingServiceIndex = Nothing,
      HX.authnRequestProviderName = Nothing,
      HX.authnRequestSubject = Nothing,
      HX.authnRequestNameIDPolicy = Nothing,
      HX.authnRequestConditions = Nothing,
      HX.authnRequestRequestedAuthnContext = Nothing,
      HX.authnRequestScoping = Nothing
    }

defProtocolType :: HX.ID -> HX.DateTime -> HX.ProtocolType
defProtocolType pid iinst =
  HX.ProtocolType
    { HX.protocolID = pid,
      HX.protocolVersion = HX.SAML20,
      HX.protocolIssueInstant = iinst,
      HX.protocolDestination = Nothing,
      HX.protocolConsent = HX.Identified HX.ConsentUnspecified,
      HX.protocolIssuer = Nothing,
      HX.protocolSignature = Nothing,
      HX.protocolExtensions = [],
      HX.relayState = Nothing
    }

explainDeniedReason :: DeniedReason -> ST
explainDeniedReason = \case
  DeniedStatusFailure -> "status: failure"
  DeniedBadUserRefs msg -> "bad user refs: " <> cs msg
  DeniedBadInResponseTos msg -> "bad InResponseTo attribute(s): " <> cs msg
  DeniedNoInResponseTo ->
    -- this can be turned into a redirect to simulate idp-initiated login.
    "authentication response without authentication request ID"
  DeniedAssertionIssueInstantNotInPast ts now ->
    "IssueInstant in Assertion must be older than "
      <> renderTime now
      <> ", but is "
      <> renderTime ts
  DeniedAuthnStatementIssueInstantNotInPast ts now ->
    "IssueInstant in AuthnStatement must be older than "
      <> renderTime now
      <> ", but is "
      <> renderTime ts
  DeniedBadRecipient weare theywant -> cs $ "bad Recipient: we are " <> weare <> ", they expected " <> theywant
  DeniedIssuerMismatch inh inass ->
    cs $
      "mismatching Issuers: in header: "
        <> maybe "Nothing" encodeElem inh
        <> ", in Assertion: "
        <> encodeElem inass
  DeniedNoStatements -> "no statements"
  DeniedNoAuthnStatement -> "no AuthnStatement"
  DeniedAuthnStatmentExpiredAt eol -> "AuthnStatement expired at " <> renderTime eol
  DeniedNoBearerConfSubj -> "No Bearer SubjectConfirmation"
  DeniedBearerConfAssertionsWithoutAudienceRestriction -> "AudienceRestriction required"
  DeniedNotOnOrAfterSubjectConfirmation eol -> "SubjectConfirmation expired at " <> renderTime eol
  DeniedNotBeforeSubjectConfirmation bol -> "SubjectConfirmation only valid starting " <> renderTime bol
  DeniedNotOnOrAfterCondition eol -> "Condition expired at " <> renderTime eol
  DeniedNotBeforeCondition bol -> "Condition only valid starting " <> renderTime bol
  DeniedAudienceMismatch we they ->
    "Audience mismatch: we are "
      <> renderURI we
      <> ", they expect one of ["
      <> ST.intercalate ", " (renderURI <$> toList they)
      <> "]"

----------------------------------------------------------------------
-- hack: use hsaml2 parsers and convert from SAMLProtocol instances

class HasXMLImport us them where
  importXml :: (MonadError String m) => them -> m us
  exportXml :: us -> them

wrapParse ::
  forall (m :: Type -> Type) them us.
  (HasCallStack, MonadError String m, HXT.XmlPickler them, HasXML us, Typeable us) =>
  (them -> m us) ->
  [Node] ->
  m us
wrapParse imprt [NodeElement el] =
  either (die (Proxy @us)) imprt $
    HX.xmlToSAML (renderLBS def $ Document defPrologue el defMiscellaneous)
wrapParse _ badxml = error $ "internal error: " <> show badxml

wrapRender ::
  forall them us.
  (HasCallStack, HXT.XmlPickler them, HasXML us) =>
  (us -> them) ->
  us ->
  [Node]
wrapRender exprt = parseElement . ourSamlToXML . exprt
  where
    parseElement lbs = case parseLBS def lbs of
      Right (Document _ el _) -> [NodeElement el]
      Left msg -> error $ show (Proxy @us, msg)

wrapRenderRoot ::
  forall them us.
  (HasCallStack, HXT.XmlPickler them, HasXMLRoot us) =>
  (us -> them) ->
  us ->
  Element
wrapRenderRoot exprt = parseElement . ourSamlToXML . exprt
  where
    parseElement lbs = case parseLBS def lbs of
      Right (Document _ el _) -> el
      Left msg -> error $ show (Proxy @us, msg)

----------------------------------------------------------------------
-- map individual types from hsaml2 to saml2-web-sso

importAuthnRequest :: (MonadError String m) => HX.AuthnRequest -> m AuthnRequest
importAuthnRequest req = do
  let proto = HX.requestProtocol $ HX.authnRequest req
  () <- importVersion $ HX.protocolVersion proto
  _rqID <- importID $ HX.protocolID proto
  _rqIssueInstant <- importTime $ HX.protocolIssueInstant proto
  _rqIssuer <- importRequiredIssuer $ HX.protocolIssuer proto
  _rqNameIDPolicy <- traverse importNameIDPolicy $ HX.authnRequestNameIDPolicy req
  traverse importURI (HX.protocolDestination proto) >>= \case
    Nothing -> pure ()
    Just dest -> die (Proxy @AuthnRequest) ("protocol destination not allowed: " <> show dest)
  pure AuthnRequest {..}

exportAuthnRequest :: AuthnRequest -> HX.AuthnRequest
exportAuthnRequest req =
  (defAuthnRequest proto)
    { HX.authnRequestNameIDPolicy = exportNameIDPolicy <$> req ^. rqNameIDPolicy
    }
  where
    proto =
      (defProtocolType (exportID $ req ^. rqID) (exportTime $ req ^. rqIssueInstant))
        { HX.protocolVersion = exportVersion,
          HX.protocolIssuer = exportRequiredIssuer $ req ^. rqIssuer,
          HX.protocolDestination = Nothing
        }

importNameIDPolicy :: (HasCallStack, MonadError String m) => HX.NameIDPolicy -> m NameIdPolicy
importNameIDPolicy nip = do
  _nidFormat <- importNameIDFormat $ HX.nameIDPolicyFormat nip
  let _nidSpNameQualifier = cs <$> HX.nameIDPolicySPNameQualifier nip
      _nidAllowCreate = HX.nameIDPolicyAllowCreate nip
  pure $ NameIdPolicy _nidFormat _nidSpNameQualifier _nidAllowCreate

exportNameIDPolicy :: (HasCallStack) => NameIdPolicy -> HX.NameIDPolicy
exportNameIDPolicy nip =
  HX.NameIDPolicy
    { HX.nameIDPolicyFormat = exportNameIDFormat $ nip ^. nidFormat,
      HX.nameIDPolicySPNameQualifier = cs <$> nip ^. nidSpNameQualifier,
      HX.nameIDPolicyAllowCreate = nip ^. nidAllowCreate
    }

importNameIDFormat :: (HasCallStack, MonadError String m) => HX.IdentifiedURI HX.NameIDFormat -> m NameIDFormat
importNameIDFormat = \case
  HX.Identified HX.NameIDFormatUnspecified -> pure NameIDFUnspecified
  HX.Identified HX.NameIDFormatEmail -> pure NameIDFEmail
  HX.Identified HX.NameIDFormatX509 -> pure NameIDFX509
  HX.Identified HX.NameIDFormatWindows -> pure NameIDFWindows
  HX.Identified HX.NameIDFormatKerberos -> pure NameIDFKerberos
  HX.Identified HX.NameIDFormatEntity -> pure NameIDFEntity
  HX.Identified HX.NameIDFormatPersistent -> pure NameIDFPersistent
  HX.Identified HX.NameIDFormatTransient -> pure NameIDFTransient
  bad@(HX.Identified HX.NameIDFormatEncrypted) -> throwError $ "unsupported: " <> show bad
  bad@(HX.Unidentified _) -> throwError $ "unsupported: " <> show bad

exportNameIDFormat :: NameIDFormat -> HX.IdentifiedURI HX.NameIDFormat
exportNameIDFormat = \case
  NameIDFUnspecified -> HX.Identified HX.NameIDFormatUnspecified
  NameIDFEmail -> HX.Identified HX.NameIDFormatEmail
  NameIDFX509 -> HX.Identified HX.NameIDFormatX509
  NameIDFWindows -> HX.Identified HX.NameIDFormatWindows
  NameIDFKerberos -> HX.Identified HX.NameIDFormatKerberos
  NameIDFEntity -> HX.Identified HX.NameIDFormatEntity
  NameIDFPersistent -> HX.Identified HX.NameIDFormatPersistent
  NameIDFTransient -> HX.Identified HX.NameIDFormatTransient

importAuthnResponse :: (HasCallStack, MonadError String m) => HX.Response -> m AuthnResponse
importAuthnResponse rsp = do
  let rsptyp :: HX.StatusResponseType = HX.response rsp
      proto :: HX.ProtocolType = HX.statusProtocol rsptyp
  () <- importVersion $ HX.protocolVersion proto
  _rspID <- importID $ HX.protocolID proto
  _rspInRespTo <- (importID . cs) `traverse` HX.statusInResponseTo rsptyp
  _rspIssueInstant <- importTime $ HX.protocolIssueInstant proto
  _rspDestination <- traverse importURI $ HX.protocolDestination proto
  _rspIssuer <- traverse importIssuer $ HX.protocolIssuer proto
  _rspStatus <- importStatus $ HX.status rsptyp
  _rspPayload <- maybe (throwError "no assertions") pure . NL.nonEmpty =<< (importPossiblyEncryptedAssertion `mapM` HX.responseAssertions rsp)
  -- ignore: @HX.protocolSignature proto :: Maybe SAML2.XML.Signature.Types.Signature@
  -- ignore: @HX.relayState proto :: Maybe SAML2.Bindings.General.RelayState@

  pure Response {..}

exportAuthnResponse :: (HasCallStack) => AuthnResponse -> HX.Response
exportAuthnResponse rsp =
  HX.Response
    { HX.response =
        HX.StatusResponseType
          { HX.statusProtocol =
              HX.ProtocolType
                { HX.protocolID = exportID (rsp ^. rspID),
                  HX.protocolVersion = exportVersion,
                  HX.protocolIssueInstant = exportTime (rsp ^. rspIssueInstant),
                  HX.protocolDestination = exportURI <$> (rsp ^. rspDestination),
                  HX.protocolConsent = HX.Identified HX.ConsentUnspecified, -- [1/8.4.1] there are no rules how to process the consent value.
                  HX.protocolIssuer = exportIssuer <$> (rsp ^. rspIssuer) :: Maybe HX.Issuer,
                  HX.protocolSignature = Nothing,
                  HX.protocolExtensions = [],
                  HX.relayState = Nothing
                },
            HX.statusInResponseTo = exportID <$> (rsp ^. rspInRespTo),
            HX.status = exportStatus (rsp ^. rspStatus)
          },
      HX.responseAssertions = toList $ exportPossiblyEncryptedAssertion <$> (rsp ^. rspPayload)
    }

importPossiblyEncryptedAssertion :: (HasCallStack, MonadError String m) => HX.PossiblyEncrypted HX.Assertion -> m Assertion
importPossiblyEncryptedAssertion bad@(HX.SoEncrypted _) = die (Proxy @Assertion) bad
importPossiblyEncryptedAssertion (HX.NotEncrypted ass) = importAssertion ass

importAssertion :: (HasCallStack, MonadError String m) => HX.Assertion -> m Assertion
importAssertion ass = do
  () <- importVersion $ HX.assertionVersion ass
  _assID <- importID $ HX.assertionID ass
  _assIssueInstant <- importTime $ HX.assertionIssueInstant ass
  _assIssuer <- importIssuer $ HX.assertionIssuer ass
  _assConditions <- traverse importConditions $ HX.assertionConditions ass
  _assContents <- do
    subj <- importSubject $ HX.assertionSubject ass
    when (null $ HX.assertionStatement ass) $
      die (Proxy @Assertion) ("no statements" :: String)
    mstmts <- importStatement `mapM` HX.assertionStatement ass
    case catMaybes mstmts of
      stmt : stmts -> pure $ SubjectAndStatements subj (stmt :| stmts)
      [] -> die (Proxy @Assertion) ("no statements" :: String)
  unless (null $ HX.assertionAdvice ass) $
    die (Proxy @Assertion) (HX.assertionAdvice ass)
  pure Assertion {..}

exportPossiblyEncryptedAssertion :: (HasCallStack) => Assertion -> HX.PossiblyEncrypted HX.Assertion
exportPossiblyEncryptedAssertion = HX.NotEncrypted . exportAssertion

exportAssertion :: (HasCallStack) => Assertion -> HX.Assertion
exportAssertion ass =
  HX.Assertion
    { HX.assertionVersion = exportVersion,
      HX.assertionID = exportID (ass ^. assID),
      HX.assertionIssueInstant = exportTime (ass ^. assIssueInstant),
      HX.assertionIssuer = exportIssuer (ass ^. assIssuer),
      HX.assertionSignature = Nothing, -- signatures are handled before parsing.
      HX.assertionSubject = exportSubject $ ass ^. assContents . sasSubject,
      HX.assertionConditions = exportConditions <$> (ass ^. assConditions),
      HX.assertionAdvice = Nothing,
      HX.assertionStatement = exportStatement <$> (ass ^. assContents . sasStatements . to toList)
    }

importSubject :: (HasCallStack, MonadError String m) => HX.Subject -> m Subject
importSubject (HX.Subject Nothing _) = die (Proxy @Subject) ("Subject NameID is missing" :: String)
importSubject (HX.Subject (Just (HX.SoEncrypted _)) _) = die (Proxy @Subject) ("encrypted subjects not supported" :: String)
importSubject (HX.Subject (Just (HX.NotEncrypted sid)) scs) = case sid of
  HX.IdentifierName nameid -> do
    nameid' <- importNameID nameid
    Subject nameid' <$> importSubjectConfirmation nameid' `mapM` scs
  bad@(HX.IdentifierBase _baseid) -> do
    die (Proxy @Subject) ("unsupported subject identifier: " <> show bad)

exportSubject :: (HasCallStack) => Subject -> HX.Subject
exportSubject subj = HX.Subject (Just (HX.NotEncrypted sid)) scs
  where
    sid :: HX.Identifier
    sid = HX.IdentifierName $ exportNameID (subj ^. subjectID)
    scs :: [HX.SubjectConfirmation]
    scs = exportSubjectConfirmation <$> subj ^. subjectConfirmations

importSubjectConfirmation :: (HasCallStack, MonadError String m) => NameID -> HX.SubjectConfirmation -> m SubjectConfirmation
importSubjectConfirmation = go
  where
    go _ (HX.SubjectConfirmation meth _ _)
      | meth /= HX.Identified HX.ConfirmationMethodBearer =
          die (Proxy @SubjectConfirmation) ("unsupported confirmation method: " <> show meth)
    go uid (HX.SubjectConfirmation _ (Just (HX.NotEncrypted (HX.IdentifierName uid'))) _)
      | Right uid /= fmapL (const ()) (importNameID uid') =
          die (Proxy @SubjectConfirmation) ("uid mismatch: " <> show (uid, uid'))
    go _ (HX.SubjectConfirmation _ (Just bad) _) =
      die (Proxy @SubjectConfirmation) ("unsupported identifier: " <> show bad)
    go _ (HX.SubjectConfirmation _ _ confdata) =
      SubjectConfirmation SubjectConfirmationMethodBearer <$> importSubjectConfirmationData `mapM` confdata

exportSubjectConfirmation :: (HasCallStack) => SubjectConfirmation -> HX.SubjectConfirmation
exportSubjectConfirmation (SubjectConfirmation SubjectConfirmationMethodBearer scd) =
  HX.SubjectConfirmation (HX.Identified HX.ConfirmationMethodBearer) Nothing $ exportSubjectConfirmationData <$> scd

importSubjectConfirmationData :: (HasCallStack, MonadError String m) => HX.SubjectConfirmationData -> m SubjectConfirmationData
importSubjectConfirmationData (HX.SubjectConfirmationData notbefore (Just notonorafter) (Just recipient) inresp confaddr _ _) =
  SubjectConfirmationData
    <$> importTime `traverse` notbefore
    <*> importTime notonorafter
    <*> importURI recipient
    <*> importID `traverse` inresp
    <*> importIP `traverse` confaddr
-- ignore: 'HX.subjectConfirmationKeyInfo' (this is only required for holder of key subjects
-- [3/3.1], [1/2.4.1.2], [1/2.4.1.4])
-- ignore: 'HX.subjectConfirmationXML' (there is nothing we can assume about it's semantics)

importSubjectConfirmationData bad@(HX.SubjectConfirmationData _ Nothing _ _ _ _ _) =
  die (Proxy @SubjectConfirmationData) ("missing NotOnOrAfter: " <> show bad)
importSubjectConfirmationData bad@(HX.SubjectConfirmationData _ _ Nothing _ _ _ _) =
  die (Proxy @SubjectConfirmationData) ("missing Recipient: " <> show bad)

exportSubjectConfirmationData :: (HasCallStack) => SubjectConfirmationData -> HX.SubjectConfirmationData
exportSubjectConfirmationData scd =
  HX.SubjectConfirmationData
    { HX.subjectConfirmationNotBefore = exportTime <$> scd ^. scdNotBefore,
      HX.subjectConfirmationNotOnOrAfter = Just . exportTime $ scd ^. scdNotOnOrAfter,
      HX.subjectConfirmationRecipient = Just . exportURI $ scd ^. scdRecipient,
      HX.subjectConfirmationInResponseTo = cs . fromID <$> scd ^. scdInResponseTo,
      HX.subjectConfirmationAddress = exportIP <$> scd ^. scdAddress,
      HX.subjectConfirmationKeyInfo = mempty,
      HX.subjectConfirmationXML = mempty
    }

importIP :: (HasCallStack, MonadError String m) => HX.IP -> m IP
importIP = mkIP . cs

exportIP :: (HasCallStack) => IP -> HX.IP
exportIP = cs . ipToST

importConditions :: forall m. (HasCallStack, MonadError String m) => HX.Conditions -> m Conditions
importConditions conds = do
  _condNotBefore <- traverse importTime $ HX.conditionsNotBefore conds
  _condNotOnOrAfter <- traverse importTime $ HX.conditionsNotOnOrAfter conds
  let _condOneTimeUse = False
      _condAudienceRestriction = []
      go :: Conditions -> HX.Condition -> m Conditions
      go conds' HX.OneTimeUse =
        pure $ conds' & condOneTimeUse .~ True
      go conds' (HX.AudienceRestriction hsrs) = do
        rs :: NonEmpty URI <- (importURI . HX.audience) `mapM` hsrs
        pure $ conds' & condAudienceRestriction %~ (rs :)
      go _ badcond = die (Proxy @Conditions) ("unsupported condition" :: String, badcond)
  foldM go (Conditions {..}) (HX.conditions conds)

exportConditions :: (HasCallStack) => Conditions -> HX.Conditions
exportConditions conds =
  HX.Conditions
    { HX.conditionsNotBefore = exportTime <$> conds ^. condNotBefore,
      HX.conditionsNotOnOrAfter = exportTime <$> conds ^. condNotOnOrAfter,
      HX.conditions =
        [HX.OneTimeUse | conds ^. condOneTimeUse]
          <> [ HX.AudienceRestriction (HX.Audience . exportURI <$> hsrs)
               | hsrs <- conds ^. condAudienceRestriction
             ]
    }

-- | Attribute statements are silently ignored.
importStatement ::
  (HasCallStack, MonadError String m) =>
  HX.Statement ->
  m (Maybe Statement)
importStatement (HX.StatementAttribute _) = pure Nothing
importStatement (HX.StatementAuthn st) =
  Just <$> do
    _astAuthnInstant <- importTime $ HX.authnStatementInstant st
    let _astSessionIndex = cs <$> HX.authnStatementSessionIndex st
    _astSessionNotOnOrAfter <- traverse importTime $ HX.authnStatementSessionNotOnOrAfter st
    _astSubjectLocality <- traverse importLocality $ HX.authnStatementSubjectLocality st
    -- NB: @HX.authnStatementContext st@ is ignored [1/2.7.2.2].
    pure $ AuthnStatement _astAuthnInstant _astSessionIndex _astSessionNotOnOrAfter _astSubjectLocality
importStatement bad = die (Proxy @Statement) bad

exportStatement :: (HasCallStack) => Statement -> HX.Statement
exportStatement stm =
  HX.StatementAuthn
    HX.AuthnStatement
      { HX.authnStatementInstant = exportTime $ stm ^. astAuthnInstant,
        HX.authnStatementSessionIndex = cs <$> (stm ^. astSessionIndex),
        HX.authnStatementSessionNotOnOrAfter = exportTime <$> (stm ^. astSessionNotOnOrAfter),
        HX.authnStatementSubjectLocality = exportLocality <$> (stm ^. astSubjectLocality),
        HX.authnStatementContext = HX.AuthnContext Nothing Nothing []
      }

importLocality :: (HasCallStack, MonadError String m) => HX.SubjectLocality -> m Locality
importLocality loc =
  Locality
    <$> traverse importIP (HX.subjectLocalityAddress loc)
    <*> pure ((mkDNSName . cs) <$> HX.subjectLocalityDNSName loc)

exportLocality :: (HasCallStack) => Locality -> HX.SubjectLocality
exportLocality loc =
  HX.SubjectLocality
    { HX.subjectLocalityAddress = exportIP <$> loc ^. localityAddress,
      HX.subjectLocalityDNSName = cs . fromDNSName <$> loc ^. localityDNSName
    }

importID :: (HasCallStack, MonadError String m) => HX.ID -> m (ID a)
importID = pure . mkID . cs

exportID :: (HasCallStack) => ID a -> HX.ID
exportID = cs . fromID

importNameID :: (HasCallStack, MonadError String m) => HX.NameID -> m NameID
importNameID bad@(HX.NameID (HX.BaseID {}) (HX.Unidentified _) _) =
  die (Proxy @NameID) (show bad)
importNameID (HX.NameID (HX.BaseID m1 m2 nid) (HX.Identified hsNameIDFormat) m3) =
  either (die (Proxy @NameID)) pure $
    form hsNameIDFormat (cs nid) >>= \nid' -> mkNameID nid' (cs <$> m1) (cs <$> m2) (cs <$> m3)
  where
    form :: (MonadError String m) => HX.NameIDFormat -> ST -> m UnqualifiedNameID
    form HX.NameIDFormatUnspecified = pure . UNameIDUnspecified
    form HX.NameIDFormatEmail = mkUNameIDEmail
    form HX.NameIDFormatX509 = pure . UNameIDX509
    form HX.NameIDFormatWindows = pure . UNameIDWindows
    form HX.NameIDFormatKerberos = pure . UNameIDKerberos
    form HX.NameIDFormatEntity = fmap UNameIDEntity . parseURI'
    form HX.NameIDFormatPersistent = pure . UNameIDPersistent
    form HX.NameIDFormatTransient = pure . UNameIDTransient
    form b@HX.NameIDFormatEncrypted = \_ -> die (Proxy @NameID) (show b)

exportNameID :: NameID -> HX.NameID
exportNameID name =
  HX.NameID
    { HX.nameBaseID =
        HX.BaseID
          (ST.unpack <$> name ^. nameIDNameQ)
          (ST.unpack <$> name ^. nameIDSPNameQ)
          (ST.unpack nid),
      HX.nameIDFormat = fmt,
      HX.nameSPProvidedID = ST.unpack <$> name ^. nameIDSPProvidedID
    }
  where
    (fmt, nid) = unform (name ^. nameID)
    unform :: UnqualifiedNameID -> (HX.IdentifiedURI HX.NameIDFormat, ST)
    unform (UNameIDUnspecified n) = (HX.Identified HX.NameIDFormatUnspecified, n)
    unform (UNameIDEmail n) =
      ( HX.Identified HX.NameIDFormatEmail,
        Email.render . CI.original $ n
      )
    unform (UNameIDX509 n) = (HX.Identified HX.NameIDFormatX509, n)
    unform (UNameIDWindows n) = (HX.Identified HX.NameIDFormatWindows, n)
    unform (UNameIDKerberos n) = (HX.Identified HX.NameIDFormatKerberos, n)
    unform (UNameIDEntity n) =
      ( HX.Identified HX.NameIDFormatEntity,
        renderURI n
      )
    unform (UNameIDPersistent n) = (HX.Identified HX.NameIDFormatPersistent, n)
    unform (UNameIDTransient n) = (HX.Identified HX.NameIDFormatTransient, n)

importVersion :: (HasCallStack, MonadError String m) => HX.SAMLVersion -> m ()
importVersion HX.SAML20 = pure ()
importVersion bad = die (Proxy @HX.SAMLVersion) bad

exportVersion :: (HasCallStack) => HX.SAMLVersion
exportVersion = HX.SAML20

importTime :: (HasCallStack, MonadError String m) => HX.DateTime -> m Time
importTime = pure . Time

exportTime :: (HasCallStack) => Time -> HX.DateTime
exportTime = fromTime

importURI :: (HasCallStack, MonadError String m) => HX.URI -> m URI
importURI uri = parseURI' . cs $ URI.uriToString id uri mempty

exportURI :: (HasCallStack) => URI -> HX.URI
exportURI uri = fromMaybe err . URI.parseURIReference . cs . renderURI $ uri
  where
    err = error $ "internal error: " <> show uri

-- | [1/3.2.2.1;3.2.2.2]
importStatus :: (HasCallStack, Monad m) => HX.Status -> m Status
importStatus =
  pure . \case
    HX.Status (HX.StatusCode HX.StatusSuccess _) _ _ -> StatusSuccess
    _ -> StatusFailure

exportStatus :: (HasCallStack) => Status -> HX.Status
exportStatus = \case
  StatusSuccess -> HX.Status (HX.StatusCode HX.StatusSuccess []) Nothing Nothing
  StatusFailure -> HX.Status (HX.StatusCode HX.StatusRequester []) Nothing Nothing

importIssuer :: (HasCallStack, MonadError String m) => HX.Issuer -> m Issuer
importIssuer = fmap Issuer . (nameIDToURI <=< importNameID) . HX.issuer
  where
    nameIDToURI nameid@(view nameID -> UNameIDEntity uri)
      | ( isNothing (nameid ^. nameIDNameQ)
            && isNothing (nameid ^. nameIDSPNameQ)
            && isNothing (nameid ^. nameIDSPProvidedID)
        ) =
          pure uri
    nameIDToURI bad = die (Proxy @Issuer) bad

exportIssuer :: (HasCallStack) => Issuer -> HX.Issuer
exportIssuer = HX.Issuer . exportNameID . entityNameID . _fromIssuer

importRequiredIssuer :: (HasCallStack, MonadError String m) => Maybe HX.Issuer -> m Issuer
importRequiredIssuer = maybe (die (Proxy @AuthnRequest) ("no issuer id" :: String)) importIssuer

exportRequiredIssuer :: (HasCallStack) => Issuer -> Maybe HX.Issuer
exportRequiredIssuer = Just . exportIssuer

----------------------------------------------------------------------
-- metadata

-- | Construct SP metadata with a new UUID and current time stamp.
--
-- The @resp@ argument here must match the @finalize-login@ end-point (as can be constructed by
-- 'getSsoURL').
mkSPMetadata :: (Monad m, SP m) => ST -> URI -> URI -> [ContactPerson] -> m SPMetadata
mkSPMetadata nick org resp contacts = do
  mid <- createID
  now <- getNow
  pure $ mkSPMetadata' mid now nick org resp contacts

mkSPMetadata' :: ID SPMetadata -> Time -> ST -> URI -> URI -> [ContactPerson] -> SPMetadata
mkSPMetadata' mid now nick org resp contacts =
  let _spID = mid
      _spCacheDuration = months 1
      _spOrgName = nick
      _spOrgDisplayName = nick
      _spOrgURL = org
      _spResponseURL = resp
      _spContacts = contacts
      years n = days n * 365
      months n = days n * 30
      days n = n * 60 * 60 * 24
      Time _spValidUntil = addTime (years 1) now
   in SPMetadata {..}

-- | NB: this works best under the assumption that the input has been constructed by
-- 'exportSPMetadata'.
importSPMetadata :: (HasCallStack, MonadError String m) => HX.Metadata -> m SPMetadata
importSPMetadata (NL.head . HX.descriptors . HX.entityDescriptors -> desc) = do
  case desc of
    HX.SPSSODescriptor {} -> pure ()
    bad -> throwError $ "malformed HX.Descriptor: " <> show bad
  _spID <-
    let raw = HX.roleDescriptorID . HX.descriptorRole $ desc
     in maybe (throwError ("malformed descriptorID: " <> show raw)) (pure . mkID . cs) raw
  _spValidUntil <-
    let raw = HX.roleDescriptorValidUntil . HX.descriptorRole $ desc
     in maybe (throwError $ "bad validUntil: " <> show raw) (fmap fromTime . importXml) raw
  _spCacheDuration <-
    let raw = HX.roleDescriptorCacheDuration . HX.descriptorRole $ desc
     in maybe (throwError $ "bad cacheDuration: " <> show raw) pure raw
  _spOrgName :: Text <-
    let raw = case fmap HX.organizationName . HX.roleDescriptorOrganization . HX.descriptorRole $ desc of
          Just (HX.Localized "EN" x :| []) -> Just x
          _ -> Nothing
     in maybe (throwError $ "bad orgName: " <> show raw) (pure . cs) raw
  _spOrgDisplayName <-
    let raw = case fmap HX.organizationDisplayName . HX.roleDescriptorOrganization . HX.descriptorRole $ desc of
          Just (HX.Localized "EN" x :| []) -> Just x
          _ -> Nothing
     in maybe (throwError $ "bad orgDisplayName: " <> show raw) (pure . cs) raw
  _spOrgURL <-
    let raw = fmap HX.organizationURL . HX.roleDescriptorOrganization . HX.descriptorRole $ desc
     in case raw of
          Just (HX.Localized "EN" u :| []) -> importURI u
          bad -> throwError $ "bad or no organizationURL" <> show bad
  _spResponseURL <-
    importURI
      . HX.endpointLocation
      . HX.indexedEndpoint
      . NL.head
      . HX.descriptorAssertionConsumerService
      $ desc
  _spContacts <- mapM importContactPerson . HX.roleDescriptorContactPerson . HX.descriptorRole $ desc
  pure SPMetadata {..}

exportSPMetadata :: (HasCallStack) => SPMetadata -> HX.Metadata
exportSPMetadata spdesc =
  HX.EntityDescriptor
    { HX.entityID = exportURI (spdesc ^. spOrgURL) :: HX.EntityID,
      HX.metadataID = Nothing :: Maybe HX.ID,
      HX.metadataValidUntil = Nothing :: Maybe HX.DateTime,
      HX.metadataCacheDuration = Nothing :: Maybe HX.Duration,
      HX.entityAttrs = mempty :: HX.Nodes,
      HX.metadataSignature = Nothing :: Maybe HX.Signature,
      HX.metadataExtensions = mempty :: HX.Extensions,
      HX.entityDescriptors = HX.Descriptors (exportSPMetadata' spdesc :| []),
      HX.entityOrganization = Nothing :: Maybe HX.Organization,
      HX.entityContactPerson = mempty :: [HX.Contact],
      HX.entityAditionalMetadataLocation = mempty :: [HX.AdditionalMetadataLocation]
    }

-- | [4/2.6], [4/2]
exportSPMetadata' :: (HasCallStack) => SPMetadata -> HX.Descriptor
exportSPMetadata' spdesc =
  HX.SPSSODescriptor
    { HX.descriptorRole =
        HX.RoleDescriptor
          { HX.roleDescriptorID = Just (cs . fromID $ spdesc ^. spID) :: Maybe HX.ID,
            HX.roleDescriptorValidUntil = Just (spdesc ^. spValidUntil) :: Maybe HX.DateTime,
            HX.roleDescriptorCacheDuration = Just (spdesc ^. spCacheDuration) :: Maybe HX.Duration,
            HX.roleDescriptorProtocolSupportEnumeration = [HX.samlURN HX.SAML20 ["protocol"]] :: [HX.AnyURI],
            HX.roleDescriptorErrorURL = Nothing :: Maybe HX.AnyURI,
            HX.roleDescriptorAttrs = [] :: HX.Nodes,
            HX.roleDescriptorSignature = Nothing :: Maybe HX.Signature,
            HX.roleDescriptorExtensions = HX.Extensions [],
            HX.roleDescriptorKeyDescriptor = [] :: [HX.KeyDescriptor],
            HX.roleDescriptorOrganization =
              Just
                HX.Organization
                  { HX.organizationAttrs = [],
                    HX.organizationExtensions = HX.Extensions [],
                    HX.organizationName = HX.Localized "EN" (cs $ spdesc ^. spOrgName) :| [],
                    HX.organizationDisplayName = HX.Localized "EN" (cs $ spdesc ^. spOrgDisplayName) :| [],
                    HX.organizationURL = HX.Localized "EN" (exportURI $ spdesc ^. spOrgURL) :| [] :: HX.List1 HX.LocalizedURI
                  },
            HX.roleDescriptorContactPerson = exportContactPerson <$> (spdesc ^. spContacts)
          },
      HX.descriptorSSO =
        HX.SSODescriptor
          { HX.ssoDescriptorArtifactResolutionService = [] :: [HX.IndexedEndpoint],
            HX.ssoDescriptorSingleLogoutService = [] :: [HX.Endpoint],
            HX.ssoDescriptorManageNameIDService = [] :: [HX.Endpoint],
            HX.ssoDescriptorNameIDFormat = [HX.Identified HX.NameIDFormatUnspecified, HX.Identified HX.NameIDFormatEntity] -- [1/8.3]
          },
      HX.descriptorAuthnRequestsSigned = False,
      HX.descriptorWantAssertionsSigned = True,
      HX.descriptorAssertionConsumerService =
        HX.IndexedEndpoint
          { HX.indexedEndpoint =
              HX.Endpoint
                { HX.endpointBinding = HX.Identified HX.BindingHTTPPOST :: HX.IdentifiedURI HX.Binding,
                  HX.endpointLocation = exportURI $ spdesc ^. spResponseURL :: HX.AnyURI,
                  HX.endpointResponseLocation = Nothing :: Maybe HX.AnyURI,
                  HX.endpointAttrs = [] :: HX.Nodes,
                  HX.endpointXML = [] :: HX.Nodes
                },
            HX.indexedEndpointIndex = 0 :: HX.UnsignedShort,
            HX.indexedEndpointIsDefault = True :: HX.Boolean
          }
          :| [] ::
          HX.List1 HX.IndexedEndpoint,
      HX.descriptorAttributeConsumingService = [] :: [HX.AttributeConsumingService]
      -- (for identification we do not need any attributes, but can use the 'SubjectID' that is
      -- always included in the response.)
    }

exportContactPerson :: ContactPerson -> HX.Contact
exportContactPerson contact =
  HX.ContactPerson
    { HX.contactType = exportContactType $ contact ^. cntType,
      HX.contactAttrs = [],
      HX.contactExtensions = HX.Extensions [],
      HX.contactCompany = cs <$> contact ^. cntCompany,
      HX.contactGivenName = cs <$> contact ^. cntGivenName,
      HX.contactSurName = cs <$> contact ^. cntSurname,
      HX.contactEmailAddress = maybeToList $ exportURI <$> contact ^. cntEmail :: [HX.AnyURI],
      HX.contactTelephoneNumber = maybeToList $ cs <$> contact ^. cntPhone
    }

importContactPerson :: (MonadError String m) => HX.Contact -> m ContactPerson
importContactPerson contact = do
  let _cntType = importContactType $ HX.contactType contact
      _cntCompany = cs <$> HX.contactCompany contact
      _cntGivenName = cs <$> HX.contactGivenName contact
      _cntSurname = cs <$> HX.contactSurName contact
      _cntPhone = listToMaybe $ cs <$> HX.contactTelephoneNumber contact
  _cntEmail <- traverse importURI $ listToMaybe (HX.contactEmailAddress contact)
  pure ContactPerson {..}

exportContactType :: ContactType -> HX.ContactType
exportContactType = \case
  ContactTechnical -> HX.ContactTypeTechnical
  ContactSupport -> HX.ContactTypeSupport
  ContactAdministrative -> HX.ContactTypeAdministrative
  ContactBilling -> HX.ContactTypeBilling
  ContactOther -> HX.ContactTypeOther

importContactType :: HX.ContactType -> ContactType
importContactType = \case
  HX.ContactTypeTechnical -> ContactTechnical
  HX.ContactTypeSupport -> ContactSupport
  HX.ContactTypeAdministrative -> ContactAdministrative
  HX.ContactTypeBilling -> ContactBilling
  HX.ContactTypeOther -> ContactOther

parseIdPMetadata :: (MonadError String m) => Element -> m IdPMetadata
parseIdPMetadata el@(Element tag _ _) = case tag of
  "{urn:oasis:names:tc:SAML:2.0:metadata}EntitiesDescriptor" ->
    (parseIdPMetadataList >=> parseIdPMetadataHead) el
  "{urn:oasis:names:tc:SAML:2.0:metadata}EntityDescriptor" ->
    parseIdPMetadataHead el
  bad ->
    throwError $ "expected <EntitiesDescriptor> or <EntityDescriptor>: " <> show bad

-- | Some IdPs send a list with one element in it.  In that case, we return the child so we
-- can call 'parseIdPMetadataHead' on it.
parseIdPMetadataList :: (MonadError String m) => Element -> m Element
parseIdPMetadataList (Element tag _ chs) = do
  unless (tag == "{urn:oasis:names:tc:SAML:2.0:metadata}EntitiesDescriptor") $
    throwError "expected <EntitiesDescriptor>"
  let isElem = \case
        (NodeElement _) -> True
        _ -> False
  case filter isElem chs of
    [NodeElement ch] -> pure ch
    bad ->
      let msg = "expected <EntitiesDescriptor> with exactly one child element"
       in throwError $ msg <> "; found " <> show (length bad)

findSome :: (MonadError String m) => String -> (Cursor -> [a]) -> [Cursor] -> m [a]
findSome descr axis cursors =
  case concatMap axis cursors of
    [] -> throwError ("Couldnt find any matches for: " <> descr)
    xs -> pure xs

getSingleton :: (MonadError String m) => String -> [a] -> m a
getSingleton _ [x] = pure x
getSingleton descr [] = throwError ("Couldnt find any matches for: " <> descr)
getSingleton descr _ = throwError ("Expected only one but found multiple matches for: " <> descr)

-- | Case insensitive version fo 'attributeIs'.  NB: this is generally violating the standard
-- (see below), but in many cases there is clearly no harm in doing so (it's hard to base an
-- attack on being able to say `HTTP-Post` instead of `HTTP-POST`).
--
-- Details:
-- * According to https://docs.oasis-open.org/security/saml/v2.0/saml-bindings-2.0-os.pdf,
--   Section 3.5.1, the binding should be "urn:oasis:names:tc:SAML:2.0:bindings:HTTP-POST",
--   but what you sent is "urn:oasis:names:tc:SAML:2.0:bindings:HTTP-Post".
-- * According to https://tools.ietf.org/html/rfc8141, page 17, URNs are case sensitive in the
--   position of "HTTP-Post".  All SAML IdPs that wire supports, including microsoft azure,
--   okta, and centrify are following this line of reasoning.
attributeIsCI :: Name -> CI ST -> (Cursor -> [Cursor])
attributeIsCI name attValue = checkNode $ \case
  NodeElement (Element _ as _) ->
    case Map.lookup name as of
      Nothing -> False
      Just (CI.mk -> elAttValue) ->
        elAttValue == attValue
  _ -> False

-- | This is the sane case: since we only want one element, just send that.
parseIdPMetadataHead :: (MonadError String m) => Element -> m IdPMetadata
parseIdPMetadataHead el@(Element tag attrs _) = do
  unless (tag == "{urn:oasis:names:tc:SAML:2.0:metadata}EntityDescriptor") $
    throwError "expected <EntityDescriptor>"
  _edIssuer :: Issuer <- do
    issueruri :: ST <- maybe (throwError "no issuer") pure (Map.lookup "entityID" attrs)
    Issuer <$> parseURI' issueruri
  _edRequestURI :: URI <- do
    target :: ST <-
      let bindingDescr = "\"Binding\" attribute with value \"urn:oasis:names:tc:SAML:2.0:bindings:HTTP-POST\""
       in [fromNode (NodeElement el)]
            & ( findSome "IDPSSODescriptor element" (descendant >=> element "{urn:oasis:names:tc:SAML:2.0:metadata}IDPSSODescriptor")
                  >=> findSome "SingleSignOnService element" (child >=> element "{urn:oasis:names:tc:SAML:2.0:metadata}SingleSignOnService")
                  >=> findSome bindingDescr (attributeIsCI "Binding" "urn:oasis:names:tc:SAML:2.0:bindings:HTTP-POST")
                  >=> getSingleton bindingDescr
                  >=> attribute "Location"
                  >>> getSingleton "\"Location\""
              )
    case parseURI' target of
      Right uri -> pure uri
      Left msg -> throwError $ "bad request uri: " <> msg

  let cursorToKeyInfo :: (MonadError String m) => Cursor -> m X509.SignedCertificate
      cursorToKeyInfo = \case
        (node -> NodeElement key) -> parseKeyInfo False . renderText def . mkDocument $ key
        bad -> throwError $ "unexpected: could not parse x509 cert: " <> show bad
  -- some metadata documents really have more than one of these.  since there is no way of knowing
  -- which one is correct, we accept all of them.
  _edCertAuthnResponse :: NonEmpty X509.SignedCertificate <- do
    let cur = fromNode $ NodeElement el
        target :: [Cursor]
        target =
          cur
            $// element "{urn:oasis:names:tc:SAML:2.0:metadata}IDPSSODescriptor"
            &/ element "{urn:oasis:names:tc:SAML:2.0:metadata}KeyDescriptor"
            >=> attributeIsNot "use" "encryption" -- [4/2.4.1.1]
            &/ element "{http://www.w3.org/2000/09/xmldsig#}KeyInfo"
    (cursorToKeyInfo `mapM` target) >>= \case
      [] -> throwError $ "could not find any AuthnResponse signature certificates."
      (x : xs) -> pure $ x :| xs
  pure IdPMetadata {..}

renderIdPMetadata :: (HasCallStack) => IdPMetadata -> Element
renderIdPMetadata (IdPMetadata issuer requri (NL.toList -> certs)) = nodesToElem $ repairNamespaces nodes
  where
    nodes =
      [xml|
      <EntityDescriptor
        ID="#{descID}"
        entityID="#{entityID}"
        xmlns="urn:oasis:names:tc:SAML:2.0:metadata">
          <IDPSSODescriptor protocolSupportEnumeration="urn:oasis:names:tc:SAML:2.0:protocol">
              <KeyDescriptor use="signing">
                  ^{certNodes}
              <SingleSignOnService Binding="urn:oasis:names:tc:SAML:2.0:bindings:HTTP-POST" Location="#{authnUrl}">
      |]
    descID = "_0c29ba62-a541-11e8-8042-873ef87bdcba"
    entityID = renderURI $ issuer ^. fromIssuer
    authnUrl = renderURI $ requri
    certNodes = mconcat $ mkCertNode <$> certs
    mkCertNode =
      either (error . show) (docToNodes)
        . parseLBS def
        . cs
        . renderKeyInfo

----------------------------------------------------------------------
-- instances

instance HasXMLImport AuthnRequest HX.AuthnRequest where
  importXml = importAuthnRequest
  exportXml = exportAuthnRequest

instance HasXML AuthnRequest where
  parse = wrapParse importAuthnRequest

instance HasXMLRoot AuthnRequest where
  renderRoot = wrapRenderRoot exportAuthnRequest

instance HasXMLImport NameIdPolicy HX.NameIDPolicy where
  importXml = importNameIDPolicy
  exportXml = exportNameIDPolicy

instance HasXMLImport AuthnResponse HX.Response where
  importXml = importAuthnResponse
  exportXml = exportAuthnResponse

instance HasXML AuthnResponse where
  parse = wrapParse importAuthnResponse

instance HasXMLRoot AuthnResponse where
  renderRoot = wrapRenderRoot exportAuthnResponse

instance HasXMLImport Assertion (HX.PossiblyEncrypted HX.Assertion) where
  importXml = importPossiblyEncryptedAssertion
  exportXml = exportPossiblyEncryptedAssertion

instance HasXML Assertion where
  parse = wrapParse importAssertion

instance HasXMLRoot Assertion where
  renderRoot = wrapRenderRoot exportAssertion

instance HasXML Subject where
  parse = wrapParse importSubject
  render = wrapRender exportSubject

instance HasXMLImport Subject HX.Subject where
  importXml = importSubject
  exportXml = exportSubject

instance HasXMLImport SubjectConfirmationData HX.SubjectConfirmationData where
  importXml = importSubjectConfirmationData
  exportXml = exportSubjectConfirmationData

instance HasXMLImport IP HX.IP where
  importXml = importIP
  exportXml = exportIP

instance HasXMLImport Conditions HX.Conditions where
  importXml = importConditions
  exportXml = exportConditions

instance HasXML Conditions where
  parse = wrapParse importConditions
  render = wrapRender exportConditions

instance HasXMLImport (Maybe Statement) HX.Statement where
  importXml = importStatement
  exportXml = exportStatement . (undefined :: Maybe Statement -> Statement)

instance HasXMLImport Locality HX.SubjectLocality where
  importXml = importLocality
  exportXml = exportLocality

instance HasXMLImport (ID a) HX.ID where
  importXml = importID
  exportXml = exportID

instance HasXMLImport NameID HX.NameID where
  importXml = importNameID
  exportXml = exportNameID

instance HasXML NameID where
  parse = wrapParse importNameID
  render = wrapRender exportNameID

instance HasXMLImport () HX.SAMLVersion where
  importXml = importVersion
  exportXml () = exportVersion

instance HasXMLImport Time HX.DateTime where
  importXml = importTime
  exportXml = exportTime

instance HasXMLImport URI HX.URI where
  importXml = importURI
  exportXml = exportURI

instance HasXMLImport Status HX.Status where
  importXml = importStatus
  exportXml = exportStatus

instance HasXMLImport Issuer HX.Issuer where
  importXml = importIssuer
  exportXml = exportIssuer

instance HasXML Issuer where
  parse = wrapParse importIssuer
  render = wrapRender exportIssuer

instance HasXML SPMetadata where
  parse = wrapParse importSPMetadata

instance HasXMLRoot SPMetadata where
  renderRoot = wrapRenderRoot exportSPMetadata

instance HasXML IdPMetadata where
  parse [NodeElement el] = parseIdPMetadata el
  parse bad = die (Proxy @IdPMetadata) bad

instance HasXMLRoot IdPMetadata where
  renderRoot = renderIdPMetadata
