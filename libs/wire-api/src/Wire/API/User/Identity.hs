{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StrictData #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.User.Identity
  ( -- * UserIdentity
    UserIdentity (..),
    newIdentity,
    emailIdentity,
    phoneIdentity,
    ssoIdentity,
    userIdentityObjectSchema,
    maybeUserIdentityObjectSchema,
    maybeUserIdentityFromComponents,

    -- * Email
    Email (..),
    fromEmail,
    parseEmail,
    validateEmail,

    -- * Phone
    Phone (..),
    parsePhone,
    isValidPhone,

    -- * UserSSOId
    UserSSOId (..),
    emailFromSAML,
    emailToSAML,
    emailToSAMLNameID,
    emailFromSAMLNameID,
    mkSampleUref,
    mkSimpleSampleUref,
  )
where

import Control.Applicative (optional)
import Control.Lens (dimap, over, (.~), (?~), (^.))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Data.Attoparsec.Text
import Data.Bifunctor (first)
import Data.ByteString.Conversion
import Data.CaseInsensitive qualified as CI
import Data.OpenApi (ToParamSchema (..))
import Data.OpenApi qualified as S
import Data.Schema
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time.Clock
import Data.Tuple.Extra (fst3, snd3, thd3)
import Imports
import SAML2.WebSSO.Test.Arbitrary ()
import SAML2.WebSSO.Types qualified as SAML
import SAML2.WebSSO.Types.Email qualified as SAMLEmail
import SAML2.WebSSO.XML qualified as SAML
import Servant
import Servant.API qualified as S
import System.FilePath ((</>))
import Test.QuickCheck qualified as QC
import Text.Email.Validate qualified as Email.V
import URI.ByteString qualified as URI
import URI.ByteString.QQ (uri)
import Wire.API.User.Profile (fromName, mkName)
import Wire.Arbitrary (Arbitrary (arbitrary), GenericUniform (..))

--------------------------------------------------------------------------------
-- UserIdentity

-- | The private unique user identity that is used for login and
-- account recovery.
data UserIdentity
  = FullIdentity Email Phone
  | EmailIdentity Email
  | PhoneIdentity Phone
  | SSOIdentity UserSSOId (Maybe Email) (Maybe Phone)
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform UserIdentity)

userIdentityObjectSchema :: ObjectSchema SwaggerDoc UserIdentity
userIdentityObjectSchema =
  Just .= withParser maybeUserIdentityObjectSchema (maybe (fail "Missing 'email' or 'phone' or 'sso_id'.") pure)

maybeUserIdentityObjectSchema :: ObjectSchema SwaggerDoc (Maybe UserIdentity)
maybeUserIdentityObjectSchema =
  dimap maybeUserIdentityToComponents maybeUserIdentityFromComponents userIdentityComponentsObjectSchema

type UserIdentityComponents = (Maybe Email, Maybe Phone, Maybe UserSSOId)

userIdentityComponentsObjectSchema :: ObjectSchema SwaggerDoc UserIdentityComponents
userIdentityComponentsObjectSchema =
  (,,)
    <$> fst3 .= maybe_ (optField "email" schema)
    <*> snd3 .= maybe_ (optField "phone" schema)
    <*> thd3 .= maybe_ (optField "sso_id" genericToSchema)

maybeUserIdentityFromComponents :: UserIdentityComponents -> Maybe UserIdentity
maybeUserIdentityFromComponents = \case
  (maybeEmail, maybePhone, Just ssoid) -> Just $ SSOIdentity ssoid maybeEmail maybePhone
  (Just email, Just phone, Nothing) -> Just $ FullIdentity email phone
  (Just email, Nothing, Nothing) -> Just $ EmailIdentity email
  (Nothing, Just phone, Nothing) -> Just $ PhoneIdentity phone
  (Nothing, Nothing, Nothing) -> Nothing

maybeUserIdentityToComponents :: Maybe UserIdentity -> UserIdentityComponents
maybeUserIdentityToComponents Nothing = (Nothing, Nothing, Nothing)
maybeUserIdentityToComponents (Just (FullIdentity email phone)) = (Just email, Just phone, Nothing)
maybeUserIdentityToComponents (Just (EmailIdentity email)) = (Just email, Nothing, Nothing)
maybeUserIdentityToComponents (Just (PhoneIdentity phone)) = (Nothing, Just phone, Nothing)
maybeUserIdentityToComponents (Just (SSOIdentity ssoid m_email m_phone)) = (m_email, m_phone, Just ssoid)

newIdentity :: Maybe Email -> Maybe Phone -> Maybe UserSSOId -> Maybe UserIdentity
newIdentity email phone (Just sso) = Just $! SSOIdentity sso email phone
newIdentity Nothing Nothing Nothing = Nothing
newIdentity (Just e) Nothing Nothing = Just $! EmailIdentity e
newIdentity Nothing (Just p) Nothing = Just $! PhoneIdentity p
newIdentity (Just e) (Just p) Nothing = Just $! FullIdentity e p

emailIdentity :: UserIdentity -> Maybe Email
emailIdentity (FullIdentity email _) = Just email
emailIdentity (EmailIdentity email) = Just email
emailIdentity (PhoneIdentity _) = Nothing
emailIdentity (SSOIdentity _ (Just email) _) = Just email
emailIdentity (SSOIdentity _ Nothing _) = Nothing

phoneIdentity :: UserIdentity -> Maybe Phone
phoneIdentity (FullIdentity _ phone) = Just phone
phoneIdentity (PhoneIdentity phone) = Just phone
phoneIdentity (EmailIdentity _) = Nothing
phoneIdentity (SSOIdentity _ _ (Just phone)) = Just phone
phoneIdentity (SSOIdentity _ _ Nothing) = Nothing

ssoIdentity :: UserIdentity -> Maybe UserSSOId
ssoIdentity (SSOIdentity ssoid _ _) = Just ssoid
ssoIdentity _ = Nothing

--------------------------------------------------------------------------------
-- Email

-- FUTUREWORK: replace this type with 'EmailAddress'
data Email = Email
  { emailLocal :: Text,
    emailDomain :: Text
  }
  deriving stock (Eq, Ord, Generic)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema Email

instance ToParamSchema Email where
  toParamSchema _ = toParamSchema (Proxy @Text)

instance ToSchema Email where
  schema =
    fromEmail
      .= parsedText
        "Email"
        ( maybe
            (Left "Invalid email. Expected '<local>@<domain>'.")
            pure
            . parseEmail
        )

instance Show Email where
  show = Text.unpack . fromEmail

instance ToByteString Email where
  builder = builder . fromEmail

instance FromByteString Email where
  parser = parser >>= maybe (fail "Invalid email") pure . parseEmail

instance S.FromHttpApiData Email where
  parseUrlPiece = maybe (Left "Invalid email") Right . fromByteString . cs

instance S.ToHttpApiData Email where
  toUrlPiece = cs . toByteString'

instance Arbitrary Email where
  arbitrary = do
    localPart <- Text.filter (/= '@') <$> arbitrary
    domain <- Text.filter (/= '@') <$> arbitrary
    pure $ Email localPart domain

fromEmail :: Email -> Text
fromEmail (Email loc dom) = loc <> "@" <> dom

-- | Parses an email address of the form <local-part>@<domain>.
parseEmail :: Text -> Maybe Email
parseEmail t = case Text.split (== '@') t of
  [localPart, domain] -> Just $! Email localPart domain
  _ -> Nothing

-- |
-- FUTUREWORK:
--
-- * Enforce these constrains during parsing already or use a separate type, see
--   [Parse, don't validate](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate).
--
-- * Check for differences to validation of `Data.Domain.Domain` and decide whether to
--   align/de-duplicate the two.
--
-- * Drop dependency on email-validate? We do our own email domain validation anyways,
--   is the dependency worth it just for validating the local part?
validateEmail :: Email -> Either String Email
validateEmail =
  pure . uncurry Email
    <=< validateDomain
    <=< validateExternalLib
    <=< validateLength . fromEmail
  where
    validateLength e
      | len <= 100 = Right e
      | otherwise = Left $ "length " <> show len <> " exceeds 100"
      where
        len = Text.length e
    validateExternalLib e = do
      email <- Email.V.validate $ encodeUtf8 e
      l <- first show . decodeUtf8' $ Email.V.localPart email
      d <- first show . decodeUtf8' $ Email.V.domainPart email
      pure (l, d)
    -- cf. https://en.wikipedia.org/wiki/Email_address#Domain
    -- n.b. We do not allow IP address literals, comments or non-ASCII
    --      characters, mostly because SES (and probably many other mail
    --      systems) don't support that (yet?) either.
    validateDomain (l, d) = parseOnly domain d
      where
        domain = (label *> many1 (char '.' *> label) *> endOfInput) $> (l, d)
        label =
          satisfy (inClass "a-zA-Z0-9")
            *> count 61 (optional (satisfy (inClass "-a-zA-Z0-9")))
            *> optional (satisfy (inClass "a-zA-Z0-9"))

--------------------------------------------------------------------------------
-- Phone

newtype Phone = Phone {fromPhone :: Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema Phone)

instance ToParamSchema Phone where
  toParamSchema _ = toParamSchema (Proxy @Text)

instance ToSchema Phone where
  schema =
    over doc (S.description ?~ "E.164 phone number") $
      fromPhone .= parsedText "PhoneNumber" (maybe (Left "Invalid phone number. Expected E.164 format.") Right . parsePhone)

instance ToByteString Phone where
  builder = builder . fromPhone

instance FromByteString Phone where
  parser = parser >>= maybe (fail "Invalid phone") pure . parsePhone

instance S.FromHttpApiData Phone where
  parseUrlPiece = maybe (Left "Invalid phone") Right . fromByteString . cs

instance S.ToHttpApiData Phone where
  toUrlPiece = cs . toByteString'

instance Arbitrary Phone where
  arbitrary =
    Phone . Text.pack <$> do
      let mkdigits n = replicateM n (QC.elements ['0' .. '9'])
      mini <- mkdigits 8
      maxi <- mkdigits =<< QC.chooseInt (0, 7)
      pure $ '+' : mini <> maxi

-- | Parses a phone number in E.164 format with a mandatory leading '+'.
parsePhone :: Text -> Maybe Phone
parsePhone p
  | isValidPhone p = Just $! Phone p
  | otherwise = Nothing

-- | Checks whether a phone number is valid, i.e. it is in E.164 format
-- with a mandatory leading '+' followed by 10-15 digits.
isValidPhone :: Text -> Bool
isValidPhone = either (const False) (const True) . parseOnly e164
  where
    e164 = char '+' *> count 8 digit *> count 7 (optional digit) *> endOfInput

--------------------------------------------------------------------------------
-- UserSSOId

-- | User's external identity.
--
-- NB: this type is serialized to the full xml encoding of the `SAML.UserRef` components, but
-- deserialiation is more lenient: it also allows for the `Issuer` to be a plain URL (without
-- xml around it), and the `NameID` to be an email address (=> format "email") or an arbitrary
-- text (=> format "unspecified").  This is for backwards compatibility and general
-- robustness.
--
-- FUTUREWORK: we should probably drop this entirely and store saml and scim data in separate
-- database columns.
data UserSSOId
  = UserSSOId SAML.UserRef
  | UserScimExternalId Text
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform UserSSOId)

-- | FUTUREWORK: This schema should ideally be a choice of either tenant+subject, or scim_external_id
-- but this is currently not possible to derive in swagger2
-- Maybe this becomes possible with swagger 3?
instance S.ToSchema UserSSOId where
  declareNamedSchema _ = do
    tenantSchema <- S.declareSchemaRef (Proxy @Text) -- FUTUREWORK: 'Issuer'
    subjectSchema <- S.declareSchemaRef (Proxy @Text) -- FUTUREWORK: 'NameID'
    scimSchema <- S.declareSchemaRef (Proxy @Text)
    pure $
      S.NamedSchema (Just "UserSSOId") $
        mempty
          & S.type_ ?~ S.OpenApiObject
          & S.properties
            .~ [ ("tenant", tenantSchema),
                 ("subject", subjectSchema),
                 ("scim_external_id", scimSchema)
               ]

instance ToJSON UserSSOId where
  toJSON = \case
    UserSSOId (SAML.UserRef tenant subject) -> A.object ["tenant" A..= SAML.encodeElem tenant, "subject" A..= SAML.encodeElem subject]
    UserScimExternalId eid -> A.object ["scim_external_id" A..= eid]

instance FromJSON UserSSOId where
  parseJSON = A.withObject "UserSSOId" $ \obj -> do
    mtenant <- lenientlyParseSAMLIssuer =<< (obj A..:? "tenant")
    msubject <- lenientlyParseSAMLNameID =<< (obj A..:? "subject")
    meid <- obj A..:? "scim_external_id"
    case (mtenant, msubject, meid) of
      (Just tenant, Just subject, Nothing) -> pure $ UserSSOId (SAML.UserRef tenant subject)
      (Nothing, Nothing, Just eid) -> pure $ UserScimExternalId eid
      _ -> fail "either need tenant and subject, or scim_external_id, but not both"

-- | If the budget for SMS and voice calls for a phone number
-- has been exhausted within a certain time frame, this timeout
-- indicates in seconds when another attempt may be made.
newtype PhoneBudgetTimeout = PhoneBudgetTimeout
  {phoneBudgetTimeout :: NominalDiffTime}
  deriving stock (Eq, Show, Generic)
  deriving newtype (Arbitrary)

instance FromJSON PhoneBudgetTimeout where
  parseJSON = A.withObject "PhoneBudgetTimeout" $ \o ->
    PhoneBudgetTimeout <$> o A..: "expires_in"

instance ToJSON PhoneBudgetTimeout where
  toJSON (PhoneBudgetTimeout t) = A.object ["expires_in" A..= t]

lenientlyParseSAMLIssuer :: Maybe LText -> A.Parser (Maybe SAML.Issuer)
lenientlyParseSAMLIssuer mbtxt = forM mbtxt $ \txt -> do
  let asxml :: Either String SAML.Issuer
      asxml = SAML.decodeElem txt

      asurl :: Either String SAML.Issuer
      asurl =
        bimap show SAML.Issuer $
          URI.parseURI URI.laxURIParserOptions (cs txt)

      err :: String
      err = "lenientlyParseSAMLIssuer: " <> show (asxml, asurl, mbtxt)

  either (const $ fail err) pure $ asxml <|> asurl

lenientlyParseSAMLNameID :: Maybe LText -> A.Parser (Maybe SAML.NameID)
lenientlyParseSAMLNameID Nothing = pure Nothing
lenientlyParseSAMLNameID (Just txt) = do
  let asxml :: Either String SAML.NameID
      asxml = SAML.decodeElem txt

      asemail :: Either String SAML.NameID
      asemail =
        maybe
          (Left "not an email")
          (fmap emailToSAMLNameID . validateEmail)
          (parseEmail (cs txt))

      astxt :: Either String SAML.NameID
      astxt = do
        nm <- mkName (cs txt)
        SAML.mkNameID (SAML.mkUNameIDUnspecified (fromName nm)) Nothing Nothing Nothing

      err :: String
      err = "lenientlyParseSAMLNameID: " <> show (asxml, asemail, astxt, txt)

  either
    (const $ fail err)
    (pure . Just)
    (asxml <|> asemail <|> astxt)

emailFromSAML :: HasCallStack => SAMLEmail.Email -> Email
emailFromSAML = fromJust . parseEmail . SAMLEmail.render

emailToSAML :: HasCallStack => Email -> SAMLEmail.Email
emailToSAML = CI.original . fromRight (error "emailToSAML") . SAMLEmail.validate . toByteString

-- | FUTUREWORK(fisx): if saml2-web-sso exported the 'NameID' constructor, we could make this
-- function total without all that praying and hoping.
emailToSAMLNameID :: HasCallStack => Email -> SAML.NameID
emailToSAMLNameID = fromRight (error "impossible") . SAML.emailNameID . fromEmail

emailFromSAMLNameID :: HasCallStack => SAML.NameID -> Maybe Email
emailFromSAMLNameID nid = case nid ^. SAML.nameID of
  SAML.UNameIDEmail email -> Just . emailFromSAML . CI.original $ email
  _ -> Nothing

-- | For testing.  Create a sample 'SAML.UserRef' value with random seeds to make 'Issuer' and
-- 'NameID' unique.  FUTUREWORK: move to saml2-web-sso.
mkSampleUref :: Text -> Text -> SAML.UserRef
mkSampleUref iseed nseed = SAML.UserRef issuer nameid
  where
    issuer :: SAML.Issuer
    issuer = SAML.Issuer ([uri|http://example.com/|] & URI.pathL .~ cs ("/" </> cs iseed))

    nameid :: SAML.NameID
    nameid = fromRight (error "impossible") $ do
      unqualified <- SAML.mkUNameIDEmail $ "me" <> nseed <> "@example.com"
      SAML.mkNameID unqualified Nothing Nothing Nothing

-- | @mkSampleUref "" ""@
mkSimpleSampleUref :: SAML.UserRef
mkSimpleSampleUref = mkSampleUref "" ""
