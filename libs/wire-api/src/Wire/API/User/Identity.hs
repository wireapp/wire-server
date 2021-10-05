{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StrictData #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

    -- * Swagger
  )
where

import Control.Applicative (optional)
import Control.Lens ((.~), (?~))
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Attoparsec.Text
import Data.Bifunctor (first, second)
import Data.ByteString.Conversion
import Data.Proxy (Proxy (..))
import Data.Schema
import Data.String.Conversions (cs)
import qualified Data.Swagger as S
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time.Clock
import Imports
import SAML2.WebSSO.Test.Arbitrary ()
import qualified SAML2.WebSSO.Types as SAML
import qualified SAML2.WebSSO.XML as SAML
import qualified Test.QuickCheck as QC
import qualified Text.Email.Validate as Email.V
import qualified URI.ByteString as URI
import Wire.API.Arbitrary (Arbitrary (arbitrary), GenericUniform (..))

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

instance S.ToSchema UserIdentity where
  declareNamedSchema _ = do
    emailSchema <- S.declareSchemaRef (Proxy @Email)
    phoneSchema <- S.declareSchemaRef (Proxy @Phone)
    ssoSchema <- S.declareSchemaRef (Proxy @UserSSOId)
    return $
      S.NamedSchema (Just "userIdentity") $
        mempty
          & S.type_ ?~ S.SwaggerObject
          & S.properties
            .~ [ ("email", emailSchema),
                 ("phone", phoneSchema),
                 ("sso_id", ssoSchema)
               ]

instance ToJSON UserIdentity where
  toJSON = \case
    FullIdentity em ph -> go (Just em) (Just ph) Nothing
    EmailIdentity em -> go (Just em) Nothing Nothing
    PhoneIdentity ph -> go Nothing (Just ph) Nothing
    SSOIdentity si em ph -> go em ph (Just si)
    where
      go :: Maybe Email -> Maybe Phone -> Maybe UserSSOId -> A.Value
      go em ph si = A.object ["email" A..= em, "phone" A..= ph, "sso_id" A..= si]

instance FromJSON UserIdentity where
  parseJSON = A.withObject "UserIdentity" $ \o -> do
    email <- o A..:? "email"
    phone <- o A..:? "phone"
    ssoid <- o A..:? "sso_id"
    maybe
      (fail "Missing 'email' or 'phone' or 'sso_id'.")
      return
      (newIdentity email phone ssoid)

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
  parser = parser >>= maybe (fail "Invalid email") return . parseEmail

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
        domain = label *> many1 (char '.' *> label) *> endOfInput *> pure (l, d)
        label =
          satisfy (inClass "a-zA-Z0-9")
            *> count 61 (optional (satisfy (inClass "-a-zA-Z0-9")))
            *> optional (satisfy (inClass "a-zA-Z0-9"))

--------------------------------------------------------------------------------
-- Phone

newtype Phone = Phone {fromPhone :: Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, S.ToSchema)

instance FromJSON Phone where
  parseJSON (A.String s) = case parsePhone s of
    Just p -> return p
    Nothing -> fail "Invalid phone number. Expected E.164 format."
  parseJSON _ = mempty

instance ToByteString Phone where
  builder = builder . fromPhone

instance FromByteString Phone where
  parser = parser >>= maybe (fail "Invalid phone") return . parsePhone

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
    () <- error "TODO: do we *always* xml-encode subject and tenant?"
    () <- error "TODO: in case that hasn't always been true: make sure we allow subject and tenant to be either stripped content or the full xml thing!"
    tenantSchema <- S.declareSchemaRef (Proxy @Text)
    subjectSchema <- S.declareSchemaRef (Proxy @Text)
    scimSchema <- S.declareSchemaRef (Proxy @Text)
    return $
      S.NamedSchema (Just "UserSSOId") $
        mempty
          & S.type_ ?~ S.SwaggerObject
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
        first show
          . second SAML.Issuer
          $ URI.parseURI URI.laxURIParserOptions (cs txt)

      err :: String
      err = "lenientlyParseSAMLIssuer: " <> show (asxml, asurl, mbtxt)

  either (const $ fail err) pure $ asxml <|> asurl

lenientlyParseSAMLNameID :: Maybe LText -> A.Parser (Maybe SAML.NameID)
lenientlyParseSAMLNameID mbtxt = forM mbtxt $ \txt -> do
  let asxml :: Either String SAML.NameID
      asxml = SAML.decodeElem txt

      asemail :: Either String SAML.NameID
      asemail =
        -- (parseEmail >=> validateEmail) (cs txt)
        error "TODO: check spar for more helpber functions.  i feel like i've done this a million times."

      astxt :: SAML.NameID
      astxt = error "TODO: are we sure we don't have any constraints on this?  if so, drop 'err' below."

      err :: String
      err = "lenientlyParseSAMLNameID: " <> show (asxml, asemail, astxt, mbtxt)

  either (const $ fail err) pure $ asxml <|> asemail <|> Right astxt
