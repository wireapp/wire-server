{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}

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

-- | Spar needs scim 'externalId's and saml 'UserRef's to identify a user: depending on the
-- context, one, the other, or both.  'externalId' is scoped inside a 'TeamId' (same
-- 'externalId' in different teams identifies different users).  'AuthId' is the type that
-- captures all allowed combinations of these, while trying to make illegal combinations
-- unrepresentable.
--
-- Since 'AuthId' needs to be stored with the user, and spar stores users in brig, 'AuthId' is
-- defined here.
module Wire.API.User.Identity.AuthId
  ( AuthId (..),
    ScimDetails (..),
    ExternalId (..),
    EmailWithSource (..),
    EmailSource (..),
    LegacyAuthId (..),
    authIdEmail,
    runAuthId,
    authIdToLegacyAuthId,
    authIdEmailWithSource,
    authIdUref,
  )
where

import Control.Lens ((.~), (?~), (^.))
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Id
import Data.Proxy (Proxy (Proxy))
import Data.String.Conversions (cs)
import Data.Swagger
  ( NamedSchema (..),
    SwaggerType (..),
    ToSchema (..),
    declareSchemaRef,
    enum_,
    properties,
    required,
    type_,
  )
import Data.Typeable (typeRep)
import Data.UUID
import Imports
import qualified SAML2.WebSSO as SAML
-- (for `instance Arbitrary SAML.UserRef`)
import SAML2.WebSSO.Test.Arbitrary ()
import Test.QuickCheck
import qualified Text.Email.Parser as Email
import Wire.API.Arbitrary (GenericUniform (GenericUniform))
import Wire.API.User.Identity.Email

data AuthId
  = AuthSAML SAML.UserRef
  | AuthSCIM ScimDetails
  | AuthBoth TeamId SAML.UserRef (Maybe EmailWithSource)
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform AuthId)

data ScimDetails = ScimDetails ExternalId EmailWithSource
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ScimDetails)

instance FromJSON ScimDetails

instance ToJSON ScimDetails

data ExternalId
  = ExternalId TeamId Text
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ExternalId)

instance FromJSON ExternalId

instance ToJSON ExternalId

data EmailWithSource = EmailWithSource
  { ewsEmail :: Email,
    ewsEmailSource :: EmailSource
  }
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform EmailWithSource)

instance FromJSON EmailWithSource

instance ToJSON EmailWithSource

data EmailSource
  = EmailFromExternalIdField
  | EmailFromEmailField
  deriving (Eq, Show, Bounded, Enum, Generic)
  deriving (Arbitrary) via (GenericUniform EmailSource)

-- | Before 'AuthId', data was stored in brig's cassandra in a type called 'UserSSOId' that
-- didn't correctly scope 'externalId's in their resp. 'TeamId's.  'LegacyAuthId' is used to
-- support reading these old values.
--
-- This can be done safely: the correct 'TeamId' is stored in the user (just not in the same
-- column), and this type only needs to be used in "Brig.Data.*".
--
-- 'LegacyAuthId' only has a 'FromJSON' instance; it is never written to the database.
--
-- FUTUREWORK: once UserSSOId has been migrated away from all cassandra instances, remove this
-- data type and everyhing that's using it.
newtype LegacyAuthId = LegacyAuthId {fromLegacyAuthId :: TeamId -> AuthId}

authIdEmailWithSource :: AuthId -> Maybe EmailWithSource
authIdEmailWithSource = \case
  AuthSAML _ -> Nothing
  AuthSCIM (ScimDetails _ ews) -> Just ews
  AuthBoth _ _ mbEws -> mbEws

-- | Internal; only needed for aeson instances.
data AuthIdTyp = AuthIdTypSAML | AuthIdTypSCIM | AuthIdTypBoth
  deriving (Eq, Show, Bounded, Enum, Generic)

instance ToSchema AuthId where
  declareNamedSchema _ = do
    typeSchema <- declareSchemaRef (Proxy @AuthIdTyp)

    -- FUTUREWORK: Which properties are valid depends on the `type` attribute value, but this
    -- is currently not possible to derive in swagger2.  Maybe this becomes possible with
    -- swagger3?  At least we can make `type` mandatory, and the rest optional, and maybe
    -- explain things in 3 examples or a long description text.
    tenantSchema <- declareSchemaRef (Proxy @Text)
    subjectSchema <- declareSchemaRef (Proxy @Text) -- FUTUREWORK: explain that this is an XML NameID as string; at least in a comment.
    teamSchema <- declareSchemaRef (Proxy @UUID)
    externalIdSchema <- declareSchemaRef (Proxy @Text)
    emailSchema <- declareSchemaRef (Proxy @Email)
    emailSourceSchema <- declareSchemaRef (Proxy @EmailSource)
    return $
      NamedSchema (Just "AuthId") $
        mempty
          & type_ ?~ SwaggerObject
          & properties
            .~ [ ("type", typeSchema),
                 ("tenant", tenantSchema),
                 ("subject", subjectSchema),
                 ("team", teamSchema),
                 ("external_id", externalIdSchema),
                 ("email", emailSchema),
                 ("email_source", emailSourceSchema)
               ]
          & required
            .~ ["type"]

instance ToJSON AuthId where
  toJSON =
    object . \case
      AuthSAML (SAML.UserRef tenant subject) ->
        [ "type" .= AuthIdTypSAML,
          "tenant" .= tenant,
          "subject" .= SAML.encodeElem subject
        ]
      AuthSCIM (ScimDetails (ExternalId team extStr) ews) ->
        [ "type" .= AuthIdTypSCIM,
          "team" .= team,
          "external_id" .= extStr,
          "email" .= ewsEmail ews,
          "email_source" .= ewsEmailSource ews
        ]
      AuthBoth team (SAML.UserRef tenant subject) mbews ->
        [ "type" .= AuthIdTypBoth,
          "team" .= team,
          "tenant" .= tenant,
          "subject" .= SAML.encodeElem subject,
          "email" .= (ewsEmail <$> mbews),
          "email_source" .= (ewsEmailSource <$> mbews)
        ]

instance FromJSON AuthId where
  parseJSON = withObject "AuthId" $ \ob -> do
    typ <- ob .: "type"
    case typ of
      AuthIdTypSAML -> do
        tenant <- ob .: "tenant"
        subject <- (ob .: "subject") >>= either fail pure . (SAML.decodeElem @SAML.NameID)
        pure $ AuthSAML (SAML.UserRef tenant subject)
      AuthIdTypSCIM -> do
        team <- ob .: "team"
        external_id <- ob .: "external_id"
        email <- ob .: "email"
        email_source <- ob .: "email_source"
        pure $ AuthSCIM (ScimDetails (ExternalId team external_id) (EmailWithSource email email_source))
      AuthIdTypBoth -> do
        team <- ob .: "team"
        tenant <- ob .: "tenant"
        subject <- (ob .: "subject") >>= either fail pure . (SAML.decodeElem @SAML.NameID)
        mbews <- do
          mbemail <- ob .:? "email"
          mbemail_source <- ob .:? "email_source"
          case (mbemail, mbemail_source) of
            (Just email, Just email_source) -> pure . Just $ EmailWithSource email email_source
            (Nothing, Nothing) -> pure Nothing
            bad -> fail $ "AuthId: need either email *and* source or neither: " <> show bad
        pure $ AuthBoth team (SAML.UserRef tenant subject) mbews

instance FromJSON LegacyAuthId where
  parseJSON val = withObject "LegacyAuthId" switch val
    where
      switch :: Aeson.Object -> Aeson.Parser LegacyAuthId
      switch = (.:? "type") >=> maybe (legacy val) (\(_ :: AuthIdTyp) -> current val)

      current :: Aeson.Value -> Aeson.Parser LegacyAuthId
      current = fmap (LegacyAuthId . const) . parseJSON

      legacy :: Aeson.Value -> Aeson.Parser LegacyAuthId
      legacy = withObject "LegacyAuthId" $ \obj -> do
        mtenant <- obj .:? "tenant"
        msubject <- obj .:? "subject"
        meid <- obj .:? "scim_external_id"
        case (mtenant, msubject, meid) of
          (Just tenant, Just subject, Nothing) -> do
            nameid <- either fail pure $ SAML.decodeElem @SAML.NameID subject
            pure . LegacyAuthId $
              (\_ -> AuthSAML (SAML.UserRef tenant nameid))
          (Nothing, Nothing, Just eid) -> do
            email <- maybe (fail "scim_external_id must be a valid email address") pure (parseEmail eid)
            pure . LegacyAuthId $
              (\tid -> AuthSCIM (ScimDetails (ExternalId tid eid) (EmailWithSource email EmailFromExternalIdField)))
          _ -> do
            fail "either need tenant and subject, or scim_external_id, but not both"

-- | Construct 'UserSSOId' values to support existing clients.  For 'UserScimExternalId',
-- render `"sso_id": null`, since this is expected by the clients; this fixes
-- https://wearezeta.atlassian.net/browse/SQSERVICES-306.
authIdToLegacyAuthId :: AuthId -> Maybe Aeson.Value
authIdToLegacyAuthId = runAuthId uref email
  where
    uref (SAML.UserRef tenant subject) = Just $ object ["tenant" .= tenant, "subject" .= SAML.encodeElem subject]
    email _ = Nothing

-- | FUTUREWORK(fisx): I'm almost certain I've written this before at least once.  but where?
enumSchema :: forall a m. (Typeable a, Bounded a, Enum a, ToJSON a, Applicative m) => m NamedSchema
enumSchema =
  pure . NamedSchema (Just . cs . show $ typeRep (Proxy @a)) $
    mempty
      & type_ ?~ SwaggerString
      & enum_ ?~ (Aeson.toJSON <$> [minBound @a ..])

instance ToSchema EmailSource where
  declareNamedSchema _ = enumSchema @EmailSource

instance ToJSON EmailSource

instance FromJSON EmailSource

instance ToSchema AuthIdTyp where
  declareNamedSchema _ = enumSchema @AuthIdTyp

instance FromJSON AuthIdTyp where
  parseJSON (Aeson.String "saml") = pure AuthIdTypSAML
  parseJSON (Aeson.String "scim") = pure AuthIdTypSCIM
  parseJSON (Aeson.String "both") = pure AuthIdTypBoth
  parseJSON bad = fail $ "unknown AuthIdTyp: " <> show bad

instance ToJSON AuthIdTyp where
  toJSON AuthIdTypSAML = "saml"
  toJSON AuthIdTypSCIM = "scim"
  toJSON AuthIdTypBoth = "both"

-- | Take apart a 'AuthId', using 'SAML.UserRef' if available, otherwise 'ScimDetails'.
-- replace runAuthId in wire-api with this
runAuthId :: (SAML.UserRef -> a) -> (ScimDetails -> a) -> AuthId -> a
runAuthId doUref doScim = \case
  AuthSAML uref -> doUref uref
  AuthSCIM scimDetails -> doScim scimDetails
  AuthBoth _ uref _ -> doUref uref

authIdUref :: AuthId -> Maybe SAML.UserRef
authIdUref = runAuthId (Just . id) (const Nothing)

authIdEmail :: AuthId -> Maybe Email
authIdEmail = \case
  AuthSAML uref -> urefToEmail uref
  other -> authIdScimEmail other
  where
    urefToEmail :: SAML.UserRef -> Maybe Email
    urefToEmail uref = case uref ^. SAML.uidSubject . SAML.nameID of
      SAML.UNameIDEmail email -> Just $ emailFromSAML email
      _ -> Nothing

    emailFromSAML :: HasCallStack => SAML.Email -> Email
    emailFromSAML = fromJust . parseEmail . cs . Email.toByteString . SAML.fromEmail

authIdScimEmail :: AuthId -> Maybe Email
authIdScimEmail = fmap ewsEmail . authIdScimEmailWithSource

authIdScimEmailWithSource :: AuthId -> Maybe EmailWithSource
authIdScimEmailWithSource =
  \case
    AuthSAML _ -> Nothing
    AuthSCIM (ScimDetails _ ews) -> Just ews
    AuthBoth _ _ mbEws -> mbEws
