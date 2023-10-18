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

data EmailSource
  = EmailFromScimExternalIdField
  | EmailFromScimEmailsField
  | -- | saml, but no scim.  deprecated, but we need to support this for the foreseeable future.
    EmailFromSamlNameId
  deriving (Eq, Show, Bounded, Enum, Generic)
  deriving (Arbitrary) via (GenericUniform EmailSource)

-- | Konst exists to make the next set of declarations easier to write
type Konst = Const ()

-- | Collection of user ids for saml sso, scim, and required extra info (email, team id).
-- Type parameters let caller decide which fields are required (`Identity`) / optional
-- (`Maybe`) / missing and ignored (`Konst`).
data UAuthId (a :: Type -> Type) (b :: Type -> Type) (c :: Type -> Type) = UAuthId
  { uaSamlId :: a SAML.UserRef,
    uaScimExternalId :: b Text,
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
type PartialUAuthId = UAuthId Maybe Maybe Maybe

instance ToSchema (UAuthId Identity Identity Identity) where
  schema = undefined

instance ToSchema (UAuthId Identity Identity Konst) where
  schema = undefined

instance ToSchema (UAuthId Identity Konst Identity) where
  schema = undefined

instance ToSchema (UAuthId Identity Konst Konst) where
  schema = undefined

instance ToSchema (UAuthId Konst Identity Identity) where
  schema = undefined

instance ToSchema (UAuthId Konst Identity Konst) where
  schema = undefined

instance ToSchema (UAuthId Maybe Maybe Maybe) where
  schema = undefined

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
