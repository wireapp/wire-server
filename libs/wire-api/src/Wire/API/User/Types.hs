{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Wire.API.User.Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Conversion
import Data.Id (TeamId)
import Data.Proxy
import Data.Schema
import Data.Swagger (ToParamSchema (..))
import Data.Swagger qualified as S
import Data.Text qualified as Text
import GHC.Generics
import Imports
import SAML2.WebSSO qualified as SAML
import Servant qualified as S
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Wire.Arbitrary (GenericUniform (..))

data EmailSource
  = EmailFromScimExternalIdField
  | EmailFromScimEmailsField
  | EmailFromSamlNameId -- saml, but no scim.  deprecated, but we need to support this for the foreseeable future.
  deriving (Eq, Show, Bounded, Enum, Generic)
  deriving (Arbitrary) via (GenericUniform EmailSource)

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

-- | in order to be able to reconstruct scim records, we need to know where in the scim record
-- the email came from (externalId?  emails field?)
data EmailWithSource = EmailWithSource
  { ewsEmail :: Email,
    ewsEmailSource :: EmailSource
  }
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform EmailWithSource)

-- | This is used in `ValidUAuthIdF f` with Const () and Identity functors.
-- Const () means that there is no value in that position, and it can be ignored.
-- Identity means that there is a value in that position, and it needs to be considered.
--
-- NOTE(fisx): we're using dot syntax for records these days, so ambiguous field names are ok!
data UAuthIdF a b c = UAuthIdF
  { samlId :: a SAML.UserRef,
    scimExternalId :: b Text,
    email :: c EmailWithSource,
    -- | only team users support saml and/or scim.  externalId is scoped in
    -- team, so once we have parsed a scim user record, the externalId should
    -- never occur anywhere without team it!
    --
    -- NOTE(owen) This is a little ambiguous to me, is it correct that we can have a teamId
    -- without an externalId, but never an externalId without a teamId?
    -- Similarly, what if we are dealing with a SAML only record? My gut feeling is that this
    -- should probably be a `b TeamId` to tie to to the same existance type as scimExternalId
    teamId :: TeamId
  }
  deriving (Generic)

deriving via
  (GenericUniform (UAuthIdF a b c))
  instance
    (Arbitrary (a SAML.UserRef), Arbitrary (b Text), Arbitrary (c EmailWithSource)) =>
    Arbitrary (UAuthIdF a b c)

deriving instance
  (Eq (a SAML.UserRef), Eq (b Text), Eq (c EmailWithSource)) =>
  Eq (UAuthIdF a b c)

deriving instance
  (Show (a SAML.UserRef), Show (b Text), Show (c EmailWithSource)) =>
  Show (UAuthIdF a b c)

-- | In brig, we don't really care about these values and never have to validate them.  We
-- just get them from spar, and write them to the database and later communicate them back to
-- spar or to team-management or to clients.  So in order to keep things from getting out of
-- hand, we decide the presence of all fields at run time.
type PartialUAuthId = UAuthIdF Maybe Maybe Maybe
