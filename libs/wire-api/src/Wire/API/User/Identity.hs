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
    isSSOIdentity,
    newIdentity,
    emailIdentity,
    ssoIdentity,
    userIdentityObjectSchema,
    maybeUserIdentityObjectSchema,
    maybeUserIdentityFromComponents,

    -- * Phone
    Phone (..),
    parsePhone,
    isValidPhone,

    -- * Email
    module Wire.API.User.EmailAddress,

    -- * UserSSOId
    UserSSOId (..),
    mkSampleUref,
    mkSimpleSampleUref,
  )
where

import Cassandra qualified as C
import Control.Error (hush)
import Control.Lens (dimap, (.~), (?~))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Data.ByteString (fromStrict, toStrict)
import Data.ByteString.UTF8 qualified as UTF8
import Data.OpenApi qualified as S
import Data.Schema
import Data.Text qualified as Text
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Data.Text.Lazy qualified as LT
import Data.These.Combinators (justThat)
import Imports
import SAML2.WebSSO.Test.Arbitrary ()
import SAML2.WebSSO.Types qualified as SAML
import SAML2.WebSSO.XML qualified as SAML
import Servant
import System.FilePath ((</>))
import Text.Email.Parser
import URI.ByteString qualified as URI
import URI.ByteString.QQ (uri)
import Web.Scim.Schema.User.Email ()
import Wire.API.User.EmailAddress
import Wire.API.User.Phone
import Wire.API.User.Profile (fromName, mkName)
import Wire.API.User.Scim
import Wire.Arbitrary (Arbitrary, GenericUniform (..))

--------------------------------------------------------------------------------
-- UserIdentity

-- | The private unique user identity that is used for login and
-- account recovery.
data UserIdentity
  = EmailIdentity EmailAddress
  | -- todo(leif): 2nd param (Maybe EmailAddress) should be part of valid scim ID
    SSOIdentity UserSSOId (Maybe EmailAddress)
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform UserIdentity)

isSSOIdentity :: UserIdentity -> Bool
isSSOIdentity (SSOIdentity _ _) = True
isSSOIdentity _ = False

userIdentityObjectSchema :: ObjectSchema SwaggerDoc UserIdentity
userIdentityObjectSchema =
  Just .= withParser maybeUserIdentityObjectSchema (maybe (fail "Missing 'email' or 'sso_id'.") pure)

maybeUserIdentityObjectSchema :: ObjectSchema SwaggerDoc (Maybe UserIdentity)
maybeUserIdentityObjectSchema =
  dimap maybeUserIdentityToComponents maybeUserIdentityFromComponents userIdentityComponentsObjectSchema

type UserIdentityComponents = (Maybe EmailAddress, Maybe UserSSOId)

userIdentityComponentsObjectSchema :: ObjectSchema SwaggerDoc UserIdentityComponents
userIdentityComponentsObjectSchema =
  (,)
    <$> fst .= maybe_ (optField "email" schema)
    <*> snd .= maybe_ (optField "sso_id" genericToSchema)

maybeUserIdentityFromComponents :: UserIdentityComponents -> Maybe UserIdentity
maybeUserIdentityFromComponents = \case
  (maybeEmail, Just ssoid) -> Just $ SSOIdentity ssoid maybeEmail
  (Just email, Nothing) -> Just $ EmailIdentity email
  (Nothing, Nothing) -> Nothing

maybeUserIdentityToComponents :: Maybe UserIdentity -> UserIdentityComponents
maybeUserIdentityToComponents Nothing = (Nothing, Nothing)
maybeUserIdentityToComponents (Just (EmailIdentity email)) = (Just email, Nothing)
maybeUserIdentityToComponents (Just (SSOIdentity ssoid m_email)) = (m_email, Just ssoid)

newIdentity :: Maybe EmailAddress -> Maybe UserSSOId -> Maybe UserIdentity
newIdentity email (Just sso) = Just $! SSOIdentity sso email
newIdentity (Just e) Nothing = Just $! EmailIdentity e
newIdentity Nothing Nothing = Nothing

emailIdentity :: UserIdentity -> Maybe EmailAddress
emailIdentity (EmailIdentity email) = Just email
emailIdentity (SSOIdentity _ (Just email)) = Just email
emailIdentity (SSOIdentity _ _) = Nothing

ssoIdentity :: UserIdentity -> Maybe UserSSOId
ssoIdentity (SSOIdentity ssoid _) = Just ssoid
ssoIdentity _ = Nothing

--------------------------------------------------------------------------------
-- UserSSOId

-- | User's external identity.
--
--
-- TODO: the NB is outdated.
-- NB: this type is serialized to the full xml encoding of the `SAML.UserRef` components, but
-- deserialiation is more lenient: it also allows for the `Issuer` to be a plain URL (without
-- xml around it), and the `NameID` to be an email address (=> format "email") or an arbitrary
-- text (=> format "unspecified").  This is for backwards compatibility and general
-- robustness.
--
-- FUTUREWORK: we should probably drop this entirely and store saml and scim data in separate
-- database columns.
newtype UserSSOId = UserSSOId ValidScimId
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform UserSSOId)

instance C.Cql UserSSOId where
  ctype = C.Tagged C.TextColumn

  fromCql (C.CqlText t) = case A.eitherDecode $ fromStrict (encodeUtf8 t) of
    Right i -> pure i
    Left msg -> Left $ "fromCql: Invalid UserSSOId: " ++ msg
  fromCql _ = Left "fromCql: UserSSOId: CqlText expected"

  toCql = C.toCql . decodeUtf8With lenientDecode . toStrict . A.encode

-- | FUTUREWORK: This schema should ideally be a choice of either tenant+subject, or scim_external_id
-- but this is currently not possible to derive in swagger2
-- Maybe this becomes possible with swagger 3?
-- TODO: migrate to schema-profunctor
instance S.ToSchema UserSSOId where
  declareNamedSchema _ = do
    tenantSchema <- S.declareSchemaRef (Proxy @Text) -- FUTUREWORK: 'Issuer'
    subjectSchema <- S.declareSchemaRef (Proxy @Text) -- FUTUREWORK: 'NameID'
    scimSchema <- S.declareSchemaRef (Proxy @Text)
    pure $
      S.NamedSchema (Just "UserSSOId") $
        mempty
          & S.type_
            ?~ S.OpenApiObject
          & S.properties
            .~ [ ("tenant", tenantSchema),
                 ("subject", subjectSchema),
                 ("scim_external_id", scimSchema)
               ]

instance ToJSON UserSSOId where
  toJSON (UserSSOId validScimId) = case justThat validScimId.validScimIdAuthInfo of
    Just (SAML.UserRef issuer nameid) ->
      A.object
        [ "tenant" A..= SAML.encodeElem issuer,
          "subject" A..= SAML.encodeElem nameid
        ]
    Nothing -> A.object ["scim_external_id" A..= validScimId.validScimIdExternal]

instance FromJSON UserSSOId where
  parseJSON _ = error "todo(leif)"

-- instance ToJSON UserSSOId where
--   toJSON = \case
--     UserSSOId (SAML.UserRef tenant subject) -> A.object ["tenant" A..= SAML.encodeElem tenant, "subject" A..= SAML.encodeElem subject]
--     UserScimExternalId eid -> A.object ["scim_external_id" A..= eid]

-- instance FromJSON UserSSOId where
--   parseJSON = A.withObject "UserSSOId" $ \obj -> do
--     mtenant <- lenientlyParseSAMLIssuer =<< (obj A..:? "tenant")
--     msubject <- lenientlyParseSAMLNameID =<< (obj A..:? "subject")
--     meid <- obj A..:? "scim_external_id"
--     case (mtenant, msubject, meid) of
--       (Just tenant, Just subject, Nothing) -> pure $ UserSSOId (SAML.UserRef tenant subject)
--       (Nothing, Nothing, Just eid) -> pure $ UserScimExternalId eid
--       _ -> fail "either need tenant and subject, or scim_external_id, but not both"

_lenientlyParseSAMLIssuer :: Maybe LText -> A.Parser (Maybe SAML.Issuer)
_lenientlyParseSAMLIssuer mbtxt = forM mbtxt $ \txt -> do
  let asxml :: Either String SAML.Issuer
      asxml = SAML.decodeElem txt

      asurl :: Either String SAML.Issuer
      asurl =
        bimap show SAML.Issuer $
          URI.parseURI URI.laxURIParserOptions (encodeUtf8 . LT.toStrict $ txt)

      err :: String
      err = "lenientlyParseSAMLIssuer: " <> show (asxml, asurl, mbtxt)

  maybe (fail err) pure $ hush asxml <|> hush asurl

_lenientlyParseSAMLNameID :: Maybe LText -> A.Parser (Maybe SAML.NameID)
_lenientlyParseSAMLNameID Nothing = pure Nothing
_lenientlyParseSAMLNameID (Just txt) = do
  let asxml :: Either String SAML.NameID
      asxml = SAML.decodeElem txt

      asemail :: Either String SAML.NameID
      asemail =
        maybe
          (Left "not an email")
          emailToSAMLNameID
          (emailAddressText . LT.toStrict $ txt)

      astxt :: Either String SAML.NameID
      astxt = do
        nm <- mkName . LT.toStrict $ txt
        SAML.mkNameID (SAML.mkUNameIDUnspecified (fromName nm)) Nothing Nothing Nothing

      err :: String
      err = "lenientlyParseSAMLNameID: " <> show (asxml, asemail, astxt, txt)

  maybe
    (fail err)
    (pure . Just)
    (hush asxml <|> hush asemail <|> hush astxt)

-- | For testing.  Create a sample 'SAML.UserRef' value with random seeds to make 'Issuer' and
-- 'NameID' unique.  FUTUREWORK: move to saml2-web-sso.
mkSampleUref :: Text -> Text -> SAML.UserRef
mkSampleUref iseed nseed = SAML.UserRef issuer nameid
  where
    issuer :: SAML.Issuer
    issuer =
      SAML.Issuer
        ( [uri|http://example.com/|]
            & URI.pathL
              .~ UTF8.fromString ("/" </> Text.unpack iseed)
        )

    nameid :: SAML.NameID
    nameid = fromRight (error "impossible") $ do
      unqualified <- SAML.mkUNameIDEmail $ "me" <> nseed <> "@example.com"
      SAML.mkNameID unqualified Nothing Nothing Nothing

-- | @mkSampleUref "" ""@
mkSimpleSampleUref :: SAML.UserRef
mkSimpleSampleUref = mkSampleUref "" ""
