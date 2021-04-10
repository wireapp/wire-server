{-# LANGUAGE DerivingVia #-}
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
    parseMaybeUserIdentity,
    newIdentity,
    emailIdentity,
    phoneIdentity,
    sparAuthIdentity,

    -- * Phone
    Phone (..),
    parsePhone,
    isValidPhone,

    -- * Re-exports
    module Wire.API.User.Identity.AuthId,
    module Wire.API.User.Identity.Email,

    -- * Swagger
  )
where

import Control.Applicative (optional)
import Control.Lens ((.~), (?~))
import Data.Aeson hiding ((<?>))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Attoparsec.Text
import Data.ByteString.Conversion
import qualified Data.HashMap.Strict as HashMap
import Data.Proxy (Proxy (..))
import Data.Swagger (NamedSchema (..), SwaggerType (..), ToSchema (..), declareSchemaRef, properties, type_)
import qualified Data.Text as Text
import Data.Time.Clock
import Imports
import qualified Test.QuickCheck as QC
import Wire.API.Arbitrary (Arbitrary (arbitrary), GenericUniform (..))
import Wire.API.User.Identity.AuthId
import Wire.API.User.Identity.Email

--------------------------------------------------------------------------------
-- UserIdentity

-- | The private unique user identity that is used for login and
-- account recovery.
data UserIdentity
  = FullIdentity Email Phone
  | EmailIdentity Email
  | PhoneIdentity Phone
  | SparAuthIdentity AuthId (Maybe Email) (Maybe Phone)
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform UserIdentity)

instance ToSchema UserIdentity where
  declareNamedSchema _ = do
    emailSchema <- declareSchemaRef (Proxy @Email)
    phoneSchema <- declareSchemaRef (Proxy @Phone)
    ssoSchema <- declareSchemaRef (Proxy @UserSSOId)
    authIdSchema <- declareSchemaRef (Proxy @AuthId)
    return $
      NamedSchema (Just "userIdentity") $
        mempty
          & type_ ?~ SwaggerObject
          & properties
            .~ [ ("email", emailSchema),
                 ("phone", phoneSchema),
                 ("sso_id", ssoSchema),
                 ("auth_id", authIdSchema)
               ]

-- | NB: there is an extra field "sso_id" that does not add information, but is expected by old
-- clients.  See also: https://wearezeta.atlassian.net/browse/SQSERVICES-264,
-- https://wearezeta.atlassian.net/browse/SQSERVICES-306
instance ToJSON UserIdentity where
  toJSON = \case
    FullIdentity em ph -> go (Just em) (Just ph) Nothing
    EmailIdentity em -> go (Just em) Nothing Nothing
    PhoneIdentity ph -> go Nothing (Just ph) Nothing
    SparAuthIdentity si em ph -> go em ph (Just si)
    where
      go :: Maybe Email -> Maybe Phone -> Maybe AuthId -> Value
      go em ph si =
        object $
          ["email" .= em, "phone" .= ph, "auth_id" .= si]
            <> maybe [] ((: []) . ("sso_id" .=)) (authIdToLegacyAuthId =<< si)

instance FromJSON UserIdentity where
  parseJSON = withObject "UserIdentity" $ \o -> do
    email <- o .:? "email"
    phone <- o .:? "phone"
    authid <- o .:? "auth_id"
    maybe
      (fail "Missing 'email' or 'phone' or 'auth_id'.")
      return
      (newIdentity email phone authid)

parseMaybeUserIdentity :: Aeson.Value -> Aeson.Parser (Maybe UserIdentity)
parseMaybeUserIdentity v = flip (withObject "UserIdenity object") v $ \ob ->
  if (any (flip HashMap.member ob) (["phone", "email", "auth_id"] :: [Text]))
    then Just <$> parseJSON v
    else pure Nothing

newIdentity :: Maybe Email -> Maybe Phone -> Maybe AuthId -> Maybe UserIdentity
newIdentity email phone (Just authid) = Just $! SparAuthIdentity authid email phone
newIdentity Nothing Nothing Nothing = Nothing
newIdentity (Just e) Nothing Nothing = Just $! EmailIdentity e
newIdentity Nothing (Just p) Nothing = Just $! PhoneIdentity p
newIdentity (Just e) (Just p) Nothing = Just $! FullIdentity e p

emailIdentity :: UserIdentity -> Maybe Email
emailIdentity (FullIdentity email _) = Just email
emailIdentity (EmailIdentity email) = Just email
emailIdentity (PhoneIdentity _) = Nothing
emailIdentity (SparAuthIdentity _ (Just email) _) = Just email
emailIdentity (SparAuthIdentity _ Nothing _) = Nothing

phoneIdentity :: UserIdentity -> Maybe Phone
phoneIdentity (FullIdentity _ phone) = Just phone
phoneIdentity (PhoneIdentity phone) = Just phone
phoneIdentity (EmailIdentity _) = Nothing
phoneIdentity (SparAuthIdentity _ _ (Just phone)) = Just phone
phoneIdentity (SparAuthIdentity _ _ Nothing) = Nothing

sparAuthIdentity :: UserIdentity -> Maybe AuthId
sparAuthIdentity (SparAuthIdentity authid _ _) = Just authid
sparAuthIdentity _ = Nothing

--------------------------------------------------------------------------------
-- Phone

newtype Phone = Phone {fromPhone :: Text}
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, ToSchema)

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
