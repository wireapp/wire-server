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
import Data.Text qualified as ST
import Data.Text.Lazy.Encoding
import Data.Time
import Data.Typeable (Proxy (Proxy), Typeable)
import Data.X509 qualified as X509
import GHC.Stack
import Network.URI qualified as HS
import SAML2.Bindings.Identifiers qualified as HS
import SAML2.Core qualified as HS
import SAML2.Metadata.Metadata qualified as HS
import SAML2.Profiles qualified as HS
import SAML2.Util
import SAML2.WebSSO.SP
import SAML2.WebSSO.Types
import SAML2.WebSSO.Types.Email qualified as Email
import SAML2.WebSSO.XML.Hack qualified as Hack
import SAML2.XML qualified as HS
import SAML2.XML qualified as HX
import SAML2.XML.Schema.Datatypes qualified as HX (Boolean, Duration, UnsignedShort)
import SAML2.XML.Signature.Types qualified as HX (Signature)
import Text.Hamlet.XML (xml)
import Text.XML
import Text.XML.Cursor
import Text.XML.DSig (parseKeyInfo, renderKeyInfo)
import Text.XML.HXT.Arrow.Pickle.Xml qualified as HS
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
  doc :: Document <- decode . decodeUtf8 $ Hack.docToXMLWithRoot raw
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

defAuthnRequest :: HS.ProtocolType -> HS.AuthnRequest
defAuthnRequest proto =
  HS.AuthnRequest
    { HS.authnRequest = HS.RequestAbstractType proto,
      HS.authnRequestForceAuthn = False,
      HS.authnRequestIsPassive = False,
      HS.authnRequestAssertionConsumerService = HS.AssertionConsumerServiceURL Nothing Nothing,
      HS.authnRequestAssertionConsumingServiceIndex = Nothing,
      HS.authnRequestProviderName = Nothing,
      HS.authnRequestSubject = Nothing,
      HS.authnRequestNameIDPolicy = Nothing,
      HS.authnRequestConditions = Nothing,
      HS.authnRequestRequestedAuthnContext = Nothing,
      HS.authnRequestScoping = Nothing
    }

defProtocolType :: HS.ID -> HS.DateTime -> HS.ProtocolType
defProtocolType pid iinst =
  HS.ProtocolType
    { HS.protocolID = pid,
      HS.protocolVersion = HS.SAML20,
      HS.protocolIssueInstant = iinst,
      HS.protocolDestination = Nothing,
      HS.protocolConsent = HS.Identified HS.ConsentUnspecified,
      HS.protocolIssuer = Nothing,
      HS.protocolSignature = Nothing,
      HS.protocolExtensions = [],
      HS.relayState = Nothing
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
  (HasCallStack, MonadError String m, HS.XmlPickler them, HasXML us, Typeable us) =>
  (them -> m us) ->
  [Node] ->
  m us
wrapParse imprt [NodeElement el] =
  either (die (Proxy @us)) imprt $
    HS.xmlToSAML (renderLBS def $ Document defPrologue el defMiscellaneous)
wrapParse _ badxml = error $ "internal error: " <> show badxml

wrapRender ::
  forall them us.
  (HasCallStack, HS.XmlPickler them, HasXML us) =>
  (us -> them) ->
  us ->
  [Node]
wrapRender exprt = parseElement . HS.samlToXML . exprt
  where
    parseElement lbs = case parseLBS def lbs of
      Right (Document _ el _) -> [NodeElement el]
      Left msg -> error $ show (Proxy @us, msg)

wrapRenderRoot ::
  forall them us.
  (HasCallStack, HS.XmlPickler them, HasXMLRoot us) =>
  (us -> them) ->
  us ->
  Element
wrapRenderRoot exprt = parseElement . HS.samlToXML . exprt
  where
    parseElement lbs = case parseLBS def lbs of
      Right (Document _ el _) -> el
      Left msg -> error $ show (Proxy @us, msg)

----------------------------------------------------------------------
-- map individual types from hsaml2 to saml2-web-sso

importAuthnRequest :: (MonadError String m) => HS.AuthnRequest -> m AuthnRequest
importAuthnRequest req = do
  let proto = HS.requestProtocol $ HS.authnRequest req
  () <- importVersion $ HS.protocolVersion proto
  _rqID <- importID $ HS.protocolID proto
  _rqIssueInstant <- importTime $ HS.protocolIssueInstant proto
  _rqIssuer <- importRequiredIssuer $ HS.protocolIssuer proto
  _rqNameIDPolicy <- traverse importNameIDPolicy $ HS.authnRequestNameIDPolicy req
  traverse importURI (HS.protocolDestination proto) >>= \case
    Nothing -> pure ()
    Just dest -> die (Proxy @AuthnRequest) ("protocol destination not allowed: " <> show dest)
  pure AuthnRequest {..}

exportAuthnRequest :: AuthnRequest -> HS.AuthnRequest
exportAuthnRequest req =
  (defAuthnRequest proto)
    { HS.authnRequestNameIDPolicy = exportNameIDPolicy <$> req ^. rqNameIDPolicy
    }
  where
    proto =
      (defProtocolType (exportID $ req ^. rqID) (exportTime $ req ^. rqIssueInstant))
        { HS.protocolVersion = exportVersion,
          HS.protocolIssuer = exportRequiredIssuer $ req ^. rqIssuer,
          HS.protocolDestination = Nothing
        }

importNameIDPolicy :: (HasCallStack, MonadError String m) => HS.NameIDPolicy -> m NameIdPolicy
importNameIDPolicy nip = do
  _nidFormat <- importNameIDFormat $ HS.nameIDPolicyFormat nip
  let _nidSpNameQualifier = mkXmlText . cs <$> HS.nameIDPolicySPNameQualifier nip
      _nidAllowCreate = HS.nameIDPolicyAllowCreate nip
  pure $ NameIdPolicy _nidFormat _nidSpNameQualifier _nidAllowCreate

exportNameIDPolicy :: (HasCallStack) => NameIdPolicy -> HS.NameIDPolicy
exportNameIDPolicy nip =
  HS.NameIDPolicy
    { HS.nameIDPolicyFormat = exportNameIDFormat $ nip ^. nidFormat,
      HS.nameIDPolicySPNameQualifier = cs . escapeXmlText <$> nip ^. nidSpNameQualifier,
      HS.nameIDPolicyAllowCreate = nip ^. nidAllowCreate
    }

importNameIDFormat :: (HasCallStack, MonadError String m) => HS.IdentifiedURI HS.NameIDFormat -> m NameIDFormat
importNameIDFormat = \case
  HS.Identified HS.NameIDFormatUnspecified -> pure NameIDFUnspecified
  HS.Identified HS.NameIDFormatEmail -> pure NameIDFEmail
  HS.Identified HS.NameIDFormatX509 -> pure NameIDFX509
  HS.Identified HS.NameIDFormatWindows -> pure NameIDFWindows
  HS.Identified HS.NameIDFormatKerberos -> pure NameIDFKerberos
  HS.Identified HS.NameIDFormatEntity -> pure NameIDFEntity
  HS.Identified HS.NameIDFormatPersistent -> pure NameIDFPersistent
  HS.Identified HS.NameIDFormatTransient -> pure NameIDFTransient
  bad@(HS.Identified HS.NameIDFormatEncrypted) -> throwError $ "unsupported: " <> show bad
  bad@(HS.Unidentified _) -> throwError $ "unsupported: " <> show bad

exportNameIDFormat :: NameIDFormat -> HS.IdentifiedURI HS.NameIDFormat
exportNameIDFormat = \case
  NameIDFUnspecified -> HS.Identified HS.NameIDFormatUnspecified
  NameIDFEmail -> HS.Identified HS.NameIDFormatEmail
  NameIDFX509 -> HS.Identified HS.NameIDFormatX509
  NameIDFWindows -> HS.Identified HS.NameIDFormatWindows
  NameIDFKerberos -> HS.Identified HS.NameIDFormatKerberos
  NameIDFEntity -> HS.Identified HS.NameIDFormatEntity
  NameIDFPersistent -> HS.Identified HS.NameIDFormatPersistent
  NameIDFTransient -> HS.Identified HS.NameIDFormatTransient

importAuthnResponse :: (HasCallStack, MonadError String m) => HS.Response -> m AuthnResponse
importAuthnResponse rsp = do
  let rsptyp :: HS.StatusResponseType = HS.response rsp
      proto :: HS.ProtocolType = HS.statusProtocol rsptyp
  () <- importVersion $ HS.protocolVersion proto
  _rspID <- importID $ HS.protocolID proto
  _rspInRespTo <- (importID . cs) `traverse` HS.statusInResponseTo rsptyp
  _rspIssueInstant <- importTime $ HS.protocolIssueInstant proto
  _rspDestination <- traverse importURI $ HS.protocolDestination proto
  _rspIssuer <- traverse importIssuer $ HS.protocolIssuer proto
  _rspStatus <- importStatus $ HS.status rsptyp
  _rspPayload <- maybe (throwError "no assertions") pure . NL.nonEmpty =<< (importPossiblyEncryptedAssertion `mapM` HS.responseAssertions rsp)
  -- ignore: @HS.protocolSignature proto :: Maybe SAML2.XML.Signature.Types.Signature@
  -- ignore: @HS.relayState proto :: Maybe SAML2.Bindings.General.RelayState@

  pure Response {..}

exportAuthnResponse :: (HasCallStack) => AuthnResponse -> HS.Response
exportAuthnResponse rsp =
  HS.Response
    { HS.response =
        HS.StatusResponseType
          { HS.statusProtocol =
              HS.ProtocolType
                { HS.protocolID = exportID (rsp ^. rspID),
                  HS.protocolVersion = exportVersion,
                  HS.protocolIssueInstant = exportTime (rsp ^. rspIssueInstant),
                  HS.protocolDestination = exportURI <$> (rsp ^. rspDestination),
                  HS.protocolConsent = HS.Identified HS.ConsentUnspecified, -- [1/8.4.1] there are no rules how to process the consent value.
                  HS.protocolIssuer = exportIssuer <$> (rsp ^. rspIssuer) :: Maybe HS.Issuer,
                  HS.protocolSignature = Nothing,
                  HS.protocolExtensions = [],
                  HS.relayState = Nothing
                },
            HS.statusInResponseTo = exportID <$> (rsp ^. rspInRespTo),
            HS.status = exportStatus (rsp ^. rspStatus)
          },
      HS.responseAssertions = toList $ exportPossiblyEncryptedAssertion <$> (rsp ^. rspPayload)
    }

importPossiblyEncryptedAssertion :: (HasCallStack, MonadError String m) => HS.PossiblyEncrypted HS.Assertion -> m Assertion
importPossiblyEncryptedAssertion bad@(HS.SoEncrypted _) = die (Proxy @Assertion) bad
importPossiblyEncryptedAssertion (HS.NotEncrypted ass) = importAssertion ass

importAssertion :: (HasCallStack, MonadError String m) => HS.Assertion -> m Assertion
importAssertion ass = do
  () <- importVersion $ HS.assertionVersion ass
  _assID <- importID $ HS.assertionID ass
  _assIssueInstant <- importTime $ HS.assertionIssueInstant ass
  _assIssuer <- importIssuer $ HS.assertionIssuer ass
  _assConditions <- traverse importConditions $ HS.assertionConditions ass
  _assContents <- do
    subj <- importSubject $ HS.assertionSubject ass
    when (null $ HS.assertionStatement ass) $
      die (Proxy @Assertion) ("no statements" :: String)
    mstmts <- importStatement `mapM` HS.assertionStatement ass
    case catMaybes mstmts of
      stmt : stmts -> pure $ SubjectAndStatements subj (stmt :| stmts)
      [] -> die (Proxy @Assertion) ("no statements" :: String)
  unless (null $ HS.assertionAdvice ass) $
    die (Proxy @Assertion) (HS.assertionAdvice ass)
  pure Assertion {..}

exportPossiblyEncryptedAssertion :: (HasCallStack) => Assertion -> HS.PossiblyEncrypted HS.Assertion
exportPossiblyEncryptedAssertion = HS.NotEncrypted . exportAssertion

exportAssertion :: (HasCallStack) => Assertion -> HS.Assertion
exportAssertion ass =
  HS.Assertion
    { HS.assertionVersion = exportVersion,
      HS.assertionID = exportID (ass ^. assID),
      HS.assertionIssueInstant = exportTime (ass ^. assIssueInstant),
      HS.assertionIssuer = exportIssuer (ass ^. assIssuer),
      HS.assertionSignature = Nothing, -- signatures are handled before parsing.
      HS.assertionSubject = exportSubject $ ass ^. assContents . sasSubject,
      HS.assertionConditions = exportConditions <$> (ass ^. assConditions),
      HS.assertionAdvice = Nothing,
      HS.assertionStatement = exportStatement <$> (ass ^. assContents . sasStatements . to toList)
    }

importSubject :: (HasCallStack, MonadError String m) => HS.Subject -> m Subject
importSubject (HS.Subject Nothing _) = die (Proxy @Subject) ("Subject NameID is missing" :: String)
importSubject (HS.Subject (Just (HS.SoEncrypted _)) _) = die (Proxy @Subject) ("encrypted subjects not supported" :: String)
importSubject (HS.Subject (Just (HS.NotEncrypted sid)) scs) = case sid of
  HS.IdentifierName nameid -> do
    nameid' <- importNameID nameid
    Subject nameid' <$> importSubjectConfirmation nameid' `mapM` scs
  bad@(HS.IdentifierBase _baseid) -> do
    die (Proxy @Subject) ("unsupported subject identifier: " <> show bad)

exportSubject :: (HasCallStack) => Subject -> HS.Subject
exportSubject subj = HS.Subject (Just (HS.NotEncrypted sid)) scs
  where
    sid :: HS.Identifier
    sid = HS.IdentifierName $ exportNameID (subj ^. subjectID)
    scs :: [HS.SubjectConfirmation]
    scs = exportSubjectConfirmation <$> subj ^. subjectConfirmations

importSubjectConfirmation :: (HasCallStack, MonadError String m) => NameID -> HS.SubjectConfirmation -> m SubjectConfirmation
importSubjectConfirmation = go
  where
    go _ (HS.SubjectConfirmation meth _ _)
      | meth /= HS.Identified HS.ConfirmationMethodBearer =
          die (Proxy @SubjectConfirmation) ("unsupported confirmation method: " <> show meth)
    go uid (HS.SubjectConfirmation _ (Just (HS.NotEncrypted (HS.IdentifierName uid'))) _)
      | Right uid /= fmapL (const ()) (importNameID uid') =
          die (Proxy @SubjectConfirmation) ("uid mismatch: " <> show (uid, uid'))
    go _ (HS.SubjectConfirmation _ (Just bad) _) =
      die (Proxy @SubjectConfirmation) ("unsupported identifier: " <> show bad)
    go _ (HS.SubjectConfirmation _ _ confdata) =
      SubjectConfirmation SubjectConfirmationMethodBearer <$> importSubjectConfirmationData `mapM` confdata

exportSubjectConfirmation :: (HasCallStack) => SubjectConfirmation -> HS.SubjectConfirmation
exportSubjectConfirmation (SubjectConfirmation SubjectConfirmationMethodBearer scd) =
  HS.SubjectConfirmation (HS.Identified HS.ConfirmationMethodBearer) Nothing $ exportSubjectConfirmationData <$> scd

importSubjectConfirmationData :: (HasCallStack, MonadError String m) => HS.SubjectConfirmationData -> m SubjectConfirmationData
importSubjectConfirmationData (HS.SubjectConfirmationData notbefore (Just notonorafter) (Just recipient) inresp confaddr _ _) =
  SubjectConfirmationData
    <$> importTime `traverse` notbefore
    <*> importTime notonorafter
    <*> importURI recipient
    <*> importID `traverse` inresp
    <*> importIP `traverse` confaddr
-- ignore: 'HS.subjectConfirmationKeyInfo' (this is only required for holder of key subjects
-- [3/3.1], [1/2.4.1.2], [1/2.4.1.4])
-- ignore: 'HS.subjectConfirmationXML' (there is nothing we can assume about it's semantics)

importSubjectConfirmationData bad@(HS.SubjectConfirmationData _ Nothing _ _ _ _ _) =
  die (Proxy @SubjectConfirmationData) ("missing NotOnOrAfter: " <> show bad)
importSubjectConfirmationData bad@(HS.SubjectConfirmationData _ _ Nothing _ _ _ _) =
  die (Proxy @SubjectConfirmationData) ("missing Recipient: " <> show bad)

exportSubjectConfirmationData :: (HasCallStack) => SubjectConfirmationData -> HS.SubjectConfirmationData
exportSubjectConfirmationData scd =
  HS.SubjectConfirmationData
    { HS.subjectConfirmationNotBefore = exportTime <$> scd ^. scdNotBefore,
      HS.subjectConfirmationNotOnOrAfter = Just . exportTime $ scd ^. scdNotOnOrAfter,
      HS.subjectConfirmationRecipient = Just . exportURI $ scd ^. scdRecipient,
      HS.subjectConfirmationInResponseTo = cs . escapeXmlText . fromID <$> scd ^. scdInResponseTo,
      HS.subjectConfirmationAddress = exportIP <$> scd ^. scdAddress,
      HS.subjectConfirmationKeyInfo = mempty,
      HS.subjectConfirmationXML = mempty
    }

importIP :: (HasCallStack, MonadError String m) => HS.IP -> m IP
importIP = mkIP . cs

exportIP :: (HasCallStack) => IP -> HS.IP
exportIP = cs . ipToST

importConditions :: forall m. (HasCallStack, MonadError String m) => HS.Conditions -> m Conditions
importConditions conds = do
  _condNotBefore <- traverse importTime $ HS.conditionsNotBefore conds
  _condNotOnOrAfter <- traverse importTime $ HS.conditionsNotOnOrAfter conds
  let _condOneTimeUse = False
      _condAudienceRestriction = []
      go :: Conditions -> HS.Condition -> m Conditions
      go conds' HS.OneTimeUse =
        pure $ conds' & condOneTimeUse .~ True
      go conds' (HS.AudienceRestriction hsrs) = do
        rs :: NonEmpty URI <- (importURI . HS.audience) `mapM` hsrs
        pure $ conds' & condAudienceRestriction %~ (rs :)
      go _ badcond = die (Proxy @Conditions) ("unsupported condition" :: String, badcond)
  foldM go (Conditions {..}) (HS.conditions conds)

exportConditions :: (HasCallStack) => Conditions -> HS.Conditions
exportConditions conds =
  HS.Conditions
    { HS.conditionsNotBefore = exportTime <$> conds ^. condNotBefore,
      HS.conditionsNotOnOrAfter = exportTime <$> conds ^. condNotOnOrAfter,
      HS.conditions =
        [HS.OneTimeUse | conds ^. condOneTimeUse]
          <> [ HS.AudienceRestriction (HS.Audience . exportURI <$> hsrs)
               | hsrs <- conds ^. condAudienceRestriction
             ]
    }

-- | Attribute statements are silently ignored.
importStatement ::
  (HasCallStack, MonadError String m) =>
  HS.Statement ->
  m (Maybe Statement)
importStatement (HS.StatementAttribute _) = pure Nothing
importStatement (HS.StatementAuthn st) =
  Just <$> do
    _astAuthnInstant <- importTime $ HS.authnStatementInstant st
    let _astSessionIndex = mkXmlText . cs <$> HS.authnStatementSessionIndex st
    _astSessionNotOnOrAfter <- traverse importTime $ HS.authnStatementSessionNotOnOrAfter st
    _astSubjectLocality <- traverse importLocality $ HS.authnStatementSubjectLocality st
    -- NB: @HS.authnStatementContext st@ is ignored [1/2.7.2.2].
    pure $ AuthnStatement _astAuthnInstant _astSessionIndex _astSessionNotOnOrAfter _astSubjectLocality
importStatement bad = die (Proxy @Statement) bad

exportStatement :: (HasCallStack) => Statement -> HS.Statement
exportStatement stm =
  HS.StatementAuthn
    HS.AuthnStatement
      { HS.authnStatementInstant = exportTime $ stm ^. astAuthnInstant,
        HS.authnStatementSessionIndex = cs . escapeXmlText <$> (stm ^. astSessionIndex),
        HS.authnStatementSessionNotOnOrAfter = exportTime <$> (stm ^. astSessionNotOnOrAfter),
        HS.authnStatementSubjectLocality = exportLocality <$> (stm ^. astSubjectLocality),
        HS.authnStatementContext = HS.AuthnContext Nothing Nothing []
      }

importLocality :: (HasCallStack, MonadError String m) => HS.SubjectLocality -> m Locality
importLocality loc =
  Locality
    <$> traverse importIP (HS.subjectLocalityAddress loc)
    <*> pure ((mkDNSName . cs) <$> HS.subjectLocalityDNSName loc)

exportLocality :: (HasCallStack) => Locality -> HS.SubjectLocality
exportLocality loc =
  HS.SubjectLocality
    { HS.subjectLocalityAddress = exportIP <$> loc ^. localityAddress,
      HS.subjectLocalityDNSName = cs . escapeXmlText . fromDNSName <$> loc ^. localityDNSName
    }

importID :: (HasCallStack, MonadError String m) => HS.ID -> m (ID a)
importID = pure . mkID . cs

exportID :: (HasCallStack) => ID a -> HS.ID
exportID = cs . escapeXmlText . fromID

importNameID :: (HasCallStack, MonadError String m) => HS.NameID -> m NameID
importNameID bad@(HS.NameID (HS.BaseID {}) (HS.Unidentified _) _) =
  die (Proxy @NameID) (show bad)
importNameID (HS.NameID (HS.BaseID m1 m2 nid) (HS.Identified hsNameIDFormat) m3) =
  either (die (Proxy @NameID)) pure $
    form hsNameIDFormat (cs nid) >>= \nid' -> mkNameID nid' (cs <$> m1) (cs <$> m2) (cs <$> m3)
  where
    form :: (MonadError String m) => HS.NameIDFormat -> ST -> m UnqualifiedNameID
    form HS.NameIDFormatUnspecified = pure . UNameIDUnspecified . mkXmlText
    form HS.NameIDFormatEmail = mkUNameIDEmail
    form HS.NameIDFormatX509 = pure . UNameIDX509 . mkXmlText
    form HS.NameIDFormatWindows = pure . UNameIDWindows . mkXmlText
    form HS.NameIDFormatKerberos = pure . UNameIDKerberos . mkXmlText
    form HS.NameIDFormatEntity = fmap UNameIDEntity . parseURI'
    form HS.NameIDFormatPersistent = pure . UNameIDPersistent . mkXmlText
    form HS.NameIDFormatTransient = pure . UNameIDTransient . mkXmlText
    form b@HS.NameIDFormatEncrypted = \_ -> die (Proxy @NameID) (show b)

exportNameID :: NameID -> HS.NameID
exportNameID name =
  HS.NameID
    { HS.nameBaseID =
        HS.BaseID
          (ST.unpack . escapeXmlText <$> name ^. nameIDNameQ)
          (ST.unpack . escapeXmlText <$> name ^. nameIDSPNameQ)
          (ST.unpack nid),
      HS.nameIDFormat = fmt,
      HS.nameSPProvidedID = ST.unpack . escapeXmlText <$> name ^. nameIDSPProvidedID
    }
  where
    (fmt, nid) = unform (name ^. nameID)
    unform :: UnqualifiedNameID -> (HS.IdentifiedURI HS.NameIDFormat, ST)
    unform (UNameIDUnspecified n) = (HS.Identified HS.NameIDFormatUnspecified, escapeXmlText n)
    unform (UNameIDEmail n) =
      ( HS.Identified HS.NameIDFormatEmail,
        escapeXmlText . mkXmlText . Email.render . CI.original $ n
      )
    unform (UNameIDX509 n) = (HS.Identified HS.NameIDFormatX509, escapeXmlText n)
    unform (UNameIDWindows n) = (HS.Identified HS.NameIDFormatWindows, escapeXmlText n)
    unform (UNameIDKerberos n) = (HS.Identified HS.NameIDFormatKerberos, escapeXmlText n)
    unform (UNameIDEntity n) =
      ( HS.Identified HS.NameIDFormatEntity,
        escapeXmlText . mkXmlText $ renderURI n
      )
    unform (UNameIDPersistent n) = (HS.Identified HS.NameIDFormatPersistent, escapeXmlText n)
    unform (UNameIDTransient n) = (HS.Identified HS.NameIDFormatTransient, escapeXmlText n)

importVersion :: (HasCallStack, MonadError String m) => HS.SAMLVersion -> m ()
importVersion HS.SAML20 = pure ()
importVersion bad = die (Proxy @HS.SAMLVersion) bad

exportVersion :: (HasCallStack) => HS.SAMLVersion
exportVersion = HS.SAML20

importTime :: (HasCallStack, MonadError String m) => HS.DateTime -> m Time
importTime = pure . Time

exportTime :: (HasCallStack) => Time -> HS.DateTime
exportTime = fromTime

importURI :: (HasCallStack, MonadError String m) => HS.URI -> m URI
importURI uri = parseURI' . cs $ HS.uriToString id uri mempty

exportURI :: (HasCallStack) => URI -> HS.URI
exportURI uri = fromMaybe err . HS.parseURIReference . cs . renderURI $ uri
  where
    err = error $ "internal error: " <> show uri

-- | [1/3.2.2.1;3.2.2.2]
importStatus :: (HasCallStack, Monad m) => HS.Status -> m Status
importStatus =
  pure . \case
    HS.Status (HS.StatusCode HS.StatusSuccess _) _ _ -> StatusSuccess
    _ -> StatusFailure

exportStatus :: (HasCallStack) => Status -> HS.Status
exportStatus = \case
  StatusSuccess -> HS.Status (HS.StatusCode HS.StatusSuccess []) Nothing Nothing
  StatusFailure -> HS.Status (HS.StatusCode HS.StatusRequester []) Nothing Nothing

importIssuer :: (HasCallStack, MonadError String m) => HS.Issuer -> m Issuer
importIssuer = fmap Issuer . (nameIDToURI <=< importNameID) . HS.issuer
  where
    nameIDToURI nameid@(view nameID -> UNameIDEntity uri)
      | ( isNothing (nameid ^. nameIDNameQ)
            && isNothing (nameid ^. nameIDSPNameQ)
            && isNothing (nameid ^. nameIDSPProvidedID)
        ) =
          pure uri
    nameIDToURI bad = die (Proxy @Issuer) bad

exportIssuer :: (HasCallStack) => Issuer -> HS.Issuer
exportIssuer = HS.Issuer . exportNameID . entityNameID . _fromIssuer

importRequiredIssuer :: (HasCallStack, MonadError String m) => Maybe HS.Issuer -> m Issuer
importRequiredIssuer = maybe (die (Proxy @AuthnRequest) ("no issuer id" :: String)) importIssuer

exportRequiredIssuer :: (HasCallStack) => Issuer -> Maybe HS.Issuer
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
mkSPMetadata' mid now (mkXmlText -> nick) org resp contacts =
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
importSPMetadata :: (HasCallStack, MonadError String m) => HS.Metadata -> m SPMetadata
importSPMetadata (NL.head . HS.descriptors . HS.entityDescriptors -> desc) = do
  case desc of
    HS.SPSSODescriptor {} -> pure ()
    bad -> throwError $ "malformed HS.Descriptor: " <> show bad
  _spID <-
    let raw = HS.roleDescriptorID . HS.descriptorRole $ desc
     in maybe (throwError ("malformed descriptorID: " <> show raw)) (pure . mkID . cs) raw
  _spValidUntil <-
    let raw = HS.roleDescriptorValidUntil . HS.descriptorRole $ desc
     in maybe (throwError $ "bad validUntil: " <> show raw) (fmap fromTime . importXml) raw
  _spCacheDuration <-
    let raw = HS.roleDescriptorCacheDuration . HS.descriptorRole $ desc
     in maybe (throwError $ "bad cacheDuration: " <> show raw) pure raw
  _spOrgName :: XmlText <-
    let raw = case fmap HS.organizationName . HS.roleDescriptorOrganization . HS.descriptorRole $ desc of
          Just (HS.Localized "EN" x :| []) -> Just x
          _ -> Nothing
     in maybe (throwError $ "bad orgName: " <> show raw) (pure . mkXmlText . cs) raw
  _spOrgDisplayName :: XmlText <-
    let raw = case fmap HS.organizationDisplayName . HS.roleDescriptorOrganization . HS.descriptorRole $ desc of
          Just (HS.Localized "EN" x :| []) -> Just x
          _ -> Nothing
     in maybe (throwError $ "bad orgDisplayName: " <> show raw) (pure . mkXmlText . cs) raw
  _spOrgURL <-
    let raw = fmap HS.organizationURL . HS.roleDescriptorOrganization . HS.descriptorRole $ desc
     in case raw of
          Just (HS.Localized "EN" u :| []) -> importURI u
          bad -> throwError $ "bad or no organizationURL" <> show bad
  _spResponseURL <-
    importURI
      . HS.endpointLocation
      . HS.indexedEndpoint
      . NL.head
      . HS.descriptorAssertionConsumerService
      $ desc
  _spContacts <- mapM importContactPerson . HS.roleDescriptorContactPerson . HS.descriptorRole $ desc
  pure SPMetadata {..}

exportSPMetadata :: (HasCallStack) => SPMetadata -> HS.Metadata
exportSPMetadata spdesc =
  HS.EntityDescriptor
    { HS.entityID = exportURI (spdesc ^. spOrgURL) :: HS.EntityID,
      HS.metadataID = Nothing :: Maybe HX.ID,
      HS.metadataValidUntil = Nothing :: Maybe HX.DateTime,
      HS.metadataCacheDuration = Nothing :: Maybe HX.Duration,
      HS.entityAttrs = mempty :: HX.Nodes,
      HS.metadataSignature = Nothing :: Maybe HX.Signature,
      HS.metadataExtensions = mempty :: HS.Extensions,
      HS.entityDescriptors = HS.Descriptors (exportSPMetadata' spdesc :| []),
      HS.entityOrganization = Nothing :: Maybe HS.Organization,
      HS.entityContactPerson = mempty :: [HS.Contact],
      HS.entityAditionalMetadataLocation = mempty :: [HS.AdditionalMetadataLocation]
    }

-- | [4/2.6], [4/2]
exportSPMetadata' :: (HasCallStack) => SPMetadata -> HS.Descriptor
exportSPMetadata' spdesc =
  HS.SPSSODescriptor
    { HS.descriptorRole =
        HS.RoleDescriptor
          { HS.roleDescriptorID = Just (cs . escapeXmlText . fromID $ spdesc ^. spID) :: Maybe HX.ID,
            HS.roleDescriptorValidUntil = Just (spdesc ^. spValidUntil) :: Maybe HX.DateTime,
            HS.roleDescriptorCacheDuration = Just (spdesc ^. spCacheDuration) :: Maybe HX.Duration,
            HS.roleDescriptorProtocolSupportEnumeration = [HS.samlURN HS.SAML20 ["protocol"]] :: [HX.AnyURI],
            HS.roleDescriptorErrorURL = Nothing :: Maybe HX.AnyURI,
            HS.roleDescriptorAttrs = [] :: HX.Nodes,
            HS.roleDescriptorSignature = Nothing :: Maybe HX.Signature,
            HS.roleDescriptorExtensions = HS.Extensions [],
            HS.roleDescriptorKeyDescriptor = [] :: [HS.KeyDescriptor],
            HS.roleDescriptorOrganization =
              Just
                HS.Organization
                  { HS.organizationAttrs = [],
                    HS.organizationExtensions = HS.Extensions [],
                    HS.organizationName = HS.Localized "EN" (cs . escapeXmlText $ spdesc ^. spOrgName) :| [],
                    HS.organizationDisplayName = HS.Localized "EN" (cs . escapeXmlText $ spdesc ^. spOrgDisplayName) :| [],
                    HS.organizationURL = HS.Localized "EN" (exportURI $ spdesc ^. spOrgURL) :| [] :: HX.List1 HS.LocalizedURI
                  },
            HS.roleDescriptorContactPerson = exportContactPerson <$> (spdesc ^. spContacts)
          },
      HS.descriptorSSO =
        HS.SSODescriptor
          { HS.ssoDescriptorArtifactResolutionService = [] :: [HS.IndexedEndpoint],
            HS.ssoDescriptorSingleLogoutService = [] :: [HS.Endpoint],
            HS.ssoDescriptorManageNameIDService = [] :: [HS.Endpoint],
            HS.ssoDescriptorNameIDFormat = [HX.Identified HS.NameIDFormatUnspecified, HX.Identified HS.NameIDFormatEntity] -- [1/8.3]
          },
      HS.descriptorAuthnRequestsSigned = False,
      HS.descriptorWantAssertionsSigned = True,
      HS.descriptorAssertionConsumerService =
        HS.IndexedEndpoint
          { HS.indexedEndpoint =
              HS.Endpoint
                { HS.endpointBinding = HX.Identified HS.BindingHTTPPOST :: HX.IdentifiedURI HS.Binding,
                  HS.endpointLocation = exportURI $ spdesc ^. spResponseURL :: HX.AnyURI,
                  HS.endpointResponseLocation = Nothing :: Maybe HX.AnyURI,
                  HS.endpointAttrs = [] :: HX.Nodes,
                  HS.endpointXML = [] :: HX.Nodes
                },
            HS.indexedEndpointIndex = 0 :: HX.UnsignedShort,
            HS.indexedEndpointIsDefault = True :: HX.Boolean
          }
          :| [] ::
          HX.List1 HS.IndexedEndpoint,
      HS.descriptorAttributeConsumingService = [] :: [HS.AttributeConsumingService]
      -- (for identification we do not need any attributes, but can use the 'SubjectID' that is
      -- always included in the response.)
    }

exportContactPerson :: ContactPerson -> HS.Contact
exportContactPerson contact =
  HS.ContactPerson
    { HS.contactType = exportContactType $ contact ^. cntType,
      HS.contactAttrs = [],
      HS.contactExtensions = HS.Extensions [],
      HS.contactCompany = cs . escapeXmlText <$> contact ^. cntCompany,
      HS.contactGivenName = cs . escapeXmlText <$> contact ^. cntGivenName,
      HS.contactSurName = cs . escapeXmlText <$> contact ^. cntSurname,
      HS.contactEmailAddress = maybeToList $ exportURI <$> contact ^. cntEmail :: [HX.AnyURI],
      HS.contactTelephoneNumber = maybeToList $ cs . escapeXmlText <$> contact ^. cntPhone
    }

importContactPerson :: (MonadError String m) => HS.Contact -> m ContactPerson
importContactPerson contact = do
  let _cntType = importContactType $ HS.contactType contact
      _cntCompany = mkXmlText . cs <$> HS.contactCompany contact
      _cntGivenName = mkXmlText . cs <$> HS.contactGivenName contact
      _cntSurname = mkXmlText . cs <$> HS.contactSurName contact
      _cntPhone = listToMaybe $ mkXmlText . cs <$> HS.contactTelephoneNumber contact
  _cntEmail <- traverse importURI $ listToMaybe (HS.contactEmailAddress contact)
  pure ContactPerson {..}

exportContactType :: ContactType -> HS.ContactType
exportContactType = \case
  ContactTechnical -> HS.ContactTypeTechnical
  ContactSupport -> HS.ContactTypeSupport
  ContactAdministrative -> HS.ContactTypeAdministrative
  ContactBilling -> HS.ContactTypeBilling
  ContactOther -> HS.ContactTypeOther

importContactType :: HS.ContactType -> ContactType
importContactType = \case
  HS.ContactTypeTechnical -> ContactTechnical
  HS.ContactTypeSupport -> ContactSupport
  HS.ContactTypeAdministrative -> ContactAdministrative
  HS.ContactTypeBilling -> ContactBilling
  HS.ContactTypeOther -> ContactOther

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

instance HasXMLImport AuthnRequest HS.AuthnRequest where
  importXml = importAuthnRequest
  exportXml = exportAuthnRequest

instance HasXML AuthnRequest where
  parse = wrapParse importAuthnRequest

instance HasXMLRoot AuthnRequest where
  renderRoot = wrapRenderRoot exportAuthnRequest

instance HasXMLImport NameIdPolicy HS.NameIDPolicy where
  importXml = importNameIDPolicy
  exportXml = exportNameIDPolicy

instance HasXMLImport AuthnResponse HS.Response where
  importXml = importAuthnResponse
  exportXml = exportAuthnResponse

instance HasXML AuthnResponse where
  parse = wrapParse importAuthnResponse

instance HasXMLRoot AuthnResponse where
  renderRoot = wrapRenderRoot exportAuthnResponse

instance HasXMLImport Assertion (HS.PossiblyEncrypted HS.Assertion) where
  importXml = importPossiblyEncryptedAssertion
  exportXml = exportPossiblyEncryptedAssertion

instance HasXML Assertion where
  parse = wrapParse importAssertion

instance HasXMLRoot Assertion where
  renderRoot = wrapRenderRoot exportAssertion

instance HasXML Subject where
  parse = wrapParse importSubject
  render = wrapRender exportSubject

instance HasXMLImport Subject HS.Subject where
  importXml = importSubject
  exportXml = exportSubject

instance HasXMLImport SubjectConfirmationData HS.SubjectConfirmationData where
  importXml = importSubjectConfirmationData
  exportXml = exportSubjectConfirmationData

instance HasXMLImport IP HS.IP where
  importXml = importIP
  exportXml = exportIP

instance HasXMLImport Conditions HS.Conditions where
  importXml = importConditions
  exportXml = exportConditions

instance HasXML Conditions where
  parse = wrapParse importConditions
  render = wrapRender exportConditions

instance HasXMLImport (Maybe Statement) HS.Statement where
  importXml = importStatement
  exportXml = exportStatement . (undefined :: Maybe Statement -> Statement)

instance HasXMLImport Locality HS.SubjectLocality where
  importXml = importLocality
  exportXml = exportLocality

instance HasXMLImport (ID a) HS.ID where
  importXml = importID
  exportXml = exportID

instance HasXMLImport NameID HS.NameID where
  importXml = importNameID
  exportXml = exportNameID

instance HasXML NameID where
  parse = wrapParse importNameID
  render = wrapRender exportNameID

instance HasXMLImport () HS.SAMLVersion where
  importXml = importVersion
  exportXml () = exportVersion

instance HasXMLImport Time HS.DateTime where
  importXml = importTime
  exportXml = exportTime

instance HasXMLImport URI HS.URI where
  importXml = importURI
  exportXml = exportURI

instance HasXMLImport Status HS.Status where
  importXml = importStatus
  exportXml = exportStatus

instance HasXMLImport Issuer HS.Issuer where
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
