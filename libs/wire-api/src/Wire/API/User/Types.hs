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
import Data.Proxy
import Data.Schema
import Data.Singletons.TH
import Data.Swagger qualified as SW
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
  deriving (FromJSON, ToJSON, SW.ToSchema) via Schema Email

instance SW.ToParamSchema Email where
  toParamSchema _ = SW.toParamSchema (Proxy @Text)

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
  | EmailFromSamlNameId -- saml, but no scim.  deprecated, but we need to support this for the foreseeable future.
  deriving (Eq, Show, Bounded, Enum, Generic)
  deriving (Arbitrary) via (GenericUniform EmailSource)

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

-- | Konst exists to make the next set of declarations easier to write
type Konst = Const ()

-- | We currently support the following user authentication and provisioning setups.
--
-- (there is a sixth case: no scim, no saml, email and password, but that does not involve
-- spar, so we don't need to consider that setup here.)
--
-- (We also sometimes validate email addresses by sending an email with a code from brig, and
-- sometimes not, but i think this should be documented and considered elsewhere.)
data UAuthIdTag
  = -- | scim with saml and email (distinguishable by location: nameid + externalId vs. scim emails field)
    UAScimSamlEmail
  | -- | scim with saml and no email
    UAScimSamlNoEmail
  | -- | scim, email and password (via traditional invitation email, emulating the team-settings flow), no saml
    UAScimEmailNoSaml
  | -- | no scim, saml with email in nameid
    UASamlEmailNoScim
  | -- | no scim, saml with no email
    UASamlNoScimNoEmail
  deriving (Show, Eq, Generic, Bounded, Enum)

$(genSingletons [''UAuthIdTag])
$(singDecideInstance ''UAuthIdTag)

-- | The types we actually want to use.
-- This is using the closed form, giving us a single place to define
-- what is and is not a valid combination of functors in `UAuthIdF`
--
-- NOTE(fisx): this may not be as useful as we'd hope, but `UAuthIdF` may be more useful in
-- return.  example: a function that cares about email, but not about samlId or
-- scimExternalId, should be typed using `UAuthIdF a b Konst`.  But there are probably a few
-- places where we want to limit the number of legal combinations, especially when parsing
-- incoming data.
-- NOTE(owen): Yes, that was part of the goal. This should be used most of the time, and code and
-- drop down to using the bare representation when it is needed. Basically we should try to use
-- this type family where we can to limit what representations are passed around to only valid
-- combinations, but code that only cares about one field can set its types based on that, kind
-- of like `HasFoo` classes if you squint a bit.
-- However, this is getting close to the point where it won't be limiting anything. Currently there
-- are 8 (2^3) options of Const and Identity.
{- ORMOLU_DISABLE -}
type family ValidUAuthIdF (f :: UAuthIdTag) where
  ValidUAuthIdF 'UAScimSamlEmail     = UAuthIdF Identity Identity Identity
  ValidUAuthIdF 'UAScimSamlNoEmail   = UAuthIdF Identity Identity Konst
  ValidUAuthIdF 'UAScimEmailNoSaml   = UAuthIdF Konst    Identity Identity
  ValidUAuthIdF 'UASamlEmailNoScim   = UAuthIdF Identity Konst    Identity
  ValidUAuthIdF 'UASamlNoScimNoEmail = UAuthIdF Identity Konst    Konst
{- ORMOLU_ENABLE -}

{-

these functions should be replaced by something that makes a lot more sense.  what that is
depends on what's going on on in the context of the calling line.

runValidUAuthIdFEither ::
  forall tag a.
  SingI tag =>
  (SAML.UserRef -> a) ->
  (Text -> a) ->
  (Email -> a) ->
  ValidUAuthIdF tag ->
  a
runValidUAuthIdFEither doUref doExternal doEmail extId = case tag of
  SScimExternalId -> doExternal $ runIdentity extId.eScim
  SSaml -> doUref $ runIdentity extId.eSaml
  SSamlAndEmail -> doUref $ runIdentity extId.eSaml
  SSamlAndPassword -> doUref $ runIdentity extId.eSaml
  SEmail -> doEmail $ runIdentity extId.eEmail
  where
    tag = sing @tag

runValidUAuthIdFBoth ::
  forall tag a.
  SingI tag =>
  (a -> a -> a) ->
  (SAML.UserRef -> a) ->
  (Text -> a) ->
  (Email -> a) ->
  ValidUAuthIdF tag ->
  a
runValidUAuthIdFBoth merge doUref doExternal doEmail extId = case tag of
  SScim -> doText $ runIdentity extId.eScim
  SSaml -> doUref $ runIdentity extId.eSaml
  SSamlAndEmail ->
    doUref (runIdentity extId.eSaml)
      `merge` doEmail (runIdentity extId.eEmail)
  SSamlAndPassword -> doUref $ runIdentity extId.eSaml
  SEmail -> doEmail $ runIdentity extId.eEmail
  SExternalId -> doExternal $ runIdentity extId.eExternalId
  where
    tag = sing @tag
-}
