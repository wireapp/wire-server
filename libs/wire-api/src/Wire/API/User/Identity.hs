{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Data.Aeson hiding ((<?>))
import Data.Attoparsec.Text
import Data.Bifunctor (first)
import Data.ByteString.Conversion
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time.Clock
import Imports
import qualified Test.QuickCheck as QC
import qualified Text.Email.Validate as Email.V
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

instance ToJSON UserIdentity where
  toJSON = \case
    FullIdentity em ph -> go (Just em) (Just ph) Nothing
    EmailIdentity em -> go (Just em) Nothing Nothing
    PhoneIdentity ph -> go Nothing (Just ph) Nothing
    SSOIdentity si em ph -> go em ph (Just si)
    where
      go :: Maybe Email -> Maybe Phone -> Maybe UserSSOId -> Value
      go em ph si = object ["email" .= em, "phone" .= ph, "sso_id" .= si]

instance FromJSON UserIdentity where
  parseJSON = withObject "UserIdentity" $ \o -> do
    email <- o .:? "email"
    phone <- o .:? "phone"
    ssoid <- o .:? "sso_id"
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

instance Show Email where
  show = Text.unpack . fromEmail

instance ToByteString Email where
  builder = builder . fromEmail

instance FromByteString Email where
  parser = parser >>= maybe (fail "Invalid email") return . parseEmail

instance ToJSON Email where
  toJSON = String . fromEmail

instance FromJSON Email where
  parseJSON =
    withText "email" $
      maybe (fail "Invalid email. Expected '<local>@<domain>'.") return
        . parseEmail

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
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON)

instance FromJSON Phone where
  parseJSON (String s) = case parsePhone s of
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
-- Morally this is the same thing as 'SAML.UserRef', but we forget the
-- structure -- i.e. we just store XML-encoded SAML blobs. If the structure
-- of those blobs changes, Brig won't have to deal with it, only Spar will.
data UserSSOId = UserSSOId
  { -- | An XML blob pointing to the identity provider that can confirm
    -- user's identity.
    userSSOIdTenant :: Text,
    -- | An XML blob specifying the user's ID on the identity provider's side.
    userSSOIdSubject :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform UserSSOId)

instance ToJSON UserSSOId where
  toJSON (UserSSOId tenant subject) = object ["tenant" .= tenant, "subject" .= subject]

instance FromJSON UserSSOId where
  parseJSON = withObject "UserSSOId" $ \obj ->
    UserSSOId
      <$> obj .: "tenant"
      <*> obj .: "subject"

-- | If the budget for SMS and voice calls for a phone number
-- has been exhausted within a certain time frame, this timeout
-- indicates in seconds when another attempt may be made.
newtype PhoneBudgetTimeout = PhoneBudgetTimeout
  {phoneBudgetTimeout :: NominalDiffTime}
  deriving stock (Eq, Show, Generic)
  deriving newtype (Arbitrary)

instance FromJSON PhoneBudgetTimeout where
  parseJSON = withObject "PhoneBudgetTimeout" $ \o ->
    PhoneBudgetTimeout <$> o .: "expires_in"

instance ToJSON PhoneBudgetTimeout where
  toJSON (PhoneBudgetTimeout t) = object ["expires_in" .= t]
