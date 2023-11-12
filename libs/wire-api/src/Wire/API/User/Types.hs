{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Wire.API.User.Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Conversion
import Data.CaseInsensitive as CI
import Data.EitherR (fmapL)
import Data.Id (TeamId)
import Data.Kind (Type)
import Data.OpenApi qualified as OA
import Data.Proxy
import Data.Schema
import Data.Text qualified as Text
import GHC.Generics
import Imports
import SAML2.WebSSO qualified as SAML
import Servant qualified as SE
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import URI.ByteString
import Wire.Arbitrary (GenericUniform (..))

--------------------------------------------------------------------------------
-- Email

-- FUTUREWORK: replace this type with 'EmailAddress'
data Email = Email
  { emailLocal :: Text,
    emailDomain :: Text
  }
  deriving stock (Eq, Ord, Generic)
  deriving (FromJSON, ToJSON, OA.ToSchema) via Schema Email

instance OA.ToParamSchema Email where
  toParamSchema _ = OA.toParamSchema (Proxy @Text)

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

instance SE.FromHttpApiData Email where
  parseUrlPiece = maybe (Left "Invalid email") Right . fromByteString . cs

instance SE.ToHttpApiData Email where
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

-- | in order to be able to reconstruct scim records, we need to know where in the scim record
-- the email came from.
data EmailWithSource = EmailWithSource
  { ewsEmail :: Email,
    ewsEmailSource :: EmailSource
  }
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform EmailWithSource)

instance ToSchema EmailWithSource where
  schema =
    object "EmailWithSource" $
      EmailWithSource
        <$> ewsEmail .= field "email" schema
        <*> ewsEmailSource .= field "source" emailSourceSchema

data EmailSource
  = EmailFromScimExternalIdField
  | EmailFromScimEmailsField
  | -- | saml auto-provisioning (no scim).  deprecated, but we need to support this for the foreseeable future.
    EmailFromSamlNameId
  deriving (Eq, Show, Bounded, Enum, Generic)
  deriving (Arbitrary) via (GenericUniform EmailSource)

emailSourceSchema :: ValueSchema NamedSwaggerDoc EmailSource
emailSourceSchema =
  enum @Text "EmailSource" $
    mconcat
      [ element "scim_external_id" EmailFromScimExternalIdField,
        element "scim_emails" EmailFromScimEmailsField,
        element "saml_name_id" EmailFromSamlNameId
      ]

-- | Konst exists to make the next set of declarations easier to write
type Konst = Const ()

-- | Collection of user ids for saml sso, scim, and required extra info (email, team id).
-- Type parameters let caller decide which fields are required (`Identity`) / optional
-- (`Maybe`) / missing and ignored (`Konst`).  Team is always required, so no type parameter
-- necessary.
--
-- Read `test/unit/Test/Wire/API/User.hs` to get some intuition of allowed values and
-- semantics.
data UAuthId (a :: Type -> Type) (b :: Type -> Type) (c :: Type -> Type) = UAuthId
  { uaSamlId :: a SAML.UserRef,
    uaScimExternalId :: b Text,
    -- | In contrast to the `email` field in `User`, this is the email address coming from
    -- SAML or SCIM.  If the user has a confirmed email address stored in `User.email` and
    -- gets an update here, the two will be different until reconfirmation.
    uaEmail :: c EmailWithSource,
    -- | Only team users support saml and/or scim.  `externalId` is scoped in `teamId`, so
    -- once we have parsed a scim user record, the `externalId` should never occur anywhere
    -- without its `teamId`.  `samlId` *could* under certain conditions be meaningful without
    -- explicit `teamId`, but it's much easier to enforce it here, too.
    uaTeamId :: TeamId
  }
  deriving (Generic)

-- | In brig, we don't really care about these values and never have to validate them.  We
-- just get them from spar, write them to the database, and later communicate them back to
-- spar or to team-management or to clients.  In these contexts it's ok to allow any
-- combination of fields present.
--
-- FUTUREWORK: make scim external id mandatory (dropping old discouraged saml-only use cases)
-- and drop `ScimUAuthId` in favor of `PartialUAuthId`.
type PartialUAuthId = UAuthId Maybe Maybe Maybe

-- | In large parts of the application logic we know that the scim identity is strictly
-- required.
--
-- FUTUREWORK: we should probably introduce and use more types like `ScimUAuthId`, like
-- `ScimOnlyUAuthId = UAuthId Konst Identity Identity`, ...
type ScimUAuthId = UAuthId Maybe Identity Maybe

partialToScimUAuthId :: PartialUAuthId -> Maybe ScimUAuthId
partialToScimUAuthId (UAuthId saml (Just eid) eml tid) = Just $ UAuthId saml (Identity eid) eml tid
partialToScimUAuthId (UAuthId _ Nothing _ _) = Nothing

scimToPartialUAuthId :: ScimUAuthId -> PartialUAuthId
scimToPartialUAuthId (UAuthId saml (Identity eid) eml tid) = UAuthId saml (Just eid) eml tid

instance ToSchema PartialUAuthId where
  schema = withParser scm $ \case
    UAuthId Nothing Nothing _ _ -> fail "at least one of saml_id, scim_external_id must be present"
    UAuthId Nothing (Just _) Nothing _ -> fail "scim_external_id requires either email address or saml_id to be present"
    ok -> pure ok
    where
      scm =
        object "PartialUAuthId" $
          UAuthId
            <$> uaSamlId .= maybe_ (optField "saml_id" userRefSchema)
            <*> uaScimExternalId .= maybe_ (optField "scim_external_id" schema)
            <*> uaEmail .= maybe_ (optField "email" schema)
            <*> uaTeamId .= field "team" schema

instance ToSchema ScimUAuthId where
  schema =
    object "ScimUAuthId" $
      UAuthId
        <$> uaSamlId .= maybe_ (optField "saml_id" userRefSchema)
        <*> (runIdentity . uaScimExternalId) .= (Identity <$> field "scim_external_id" schema)
        <*> uaEmail .= maybe_ (optField "email" schema)
        <*> uaTeamId .= field "team" schema

userRefSchema :: ValueSchema NamedSwaggerDoc SAML.UserRef
userRefSchema =
  object "UserRef" $
    SAML.UserRef
      <$> SAML._uidTenant .= field "tenant" issuerSchema
      <*> SAML._uidSubject .= field "subject" nameIdSchema

-- | FUTUREWORK: partially redundant, see lenientlyParseSAMLIssuer.
issuerSchema :: ValueSchema NamedSwaggerDoc SAML.Issuer
issuerSchema = rdr .= parsedText "SAML.Issuer" prs
  where
    rdr :: SAML.Issuer -> Text
    rdr = cs . SAML.encodeElem
      where
        _justTxt :: SAML.Issuer -> Text
        _justTxt = cs . normalizeURIRef' noNormalization . SAML._fromIssuer

    prs :: Text -> Either String SAML.Issuer
    prs txt = SAML.decodeElem (cs txt) <|> justTxt txt
      where
        justTxt :: Text -> Either String SAML.Issuer
        justTxt = fmap SAML.Issuer . fmapL show . parseURI laxURIParserOptions . cs

-- | FUTUREWORK: partially redundant, see lenientlyParseSAMLNameID.
nameIdSchema :: ValueSchema NamedSwaggerDoc SAML.NameID
nameIdSchema = rdr .= parsedText "SAML.NameID" prs
  where
    rdr :: SAML.NameID -> Text
    rdr = cs . SAML.encodeElem
      where
        _justTxt :: SAML.NameID -> Text
        _justTxt = CI.original . SAML.nameIDToST

    prs :: Text -> Either String SAML.NameID
    prs txt = SAML.decodeElem (cs txt) <|> justTxt txt
      where
        justTxt :: Text -> Either String SAML.NameID
        justTxt = Right . either (const $ SAML.unspecifiedNameID txt) id . SAML.emailNameID

deriving via (Schema (UAuthId a b c)) instance (Typeable a, Typeable b, Typeable c, ToSchema (UAuthId a b c)) => OA.ToSchema (UAuthId a b c)

deriving via (Schema (UAuthId a b c)) instance (ToSchema (UAuthId a b c)) => ToJSON (UAuthId a b c)

deriving via (Schema (UAuthId a b c)) instance (ToSchema (UAuthId a b c)) => FromJSON (UAuthId a b c)

deriving via
  (GenericUniform (UAuthId a b c))
  instance
    (Arbitrary (a SAML.UserRef), Arbitrary (b Text), Arbitrary (c EmailWithSource)) =>
    Arbitrary (UAuthId a b c)

deriving stock instance (Eq (a SAML.UserRef), Eq (b Text), Eq (c EmailWithSource)) => Eq (UAuthId a b c)

deriving stock instance (Show (a SAML.UserRef), Show (b Text), Show (c EmailWithSource)) => Show (UAuthId a b c)
