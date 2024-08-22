{-# LANGUAGE RecordWildCards #-}

module Wire.UserSearch.Types where

import Cassandra qualified as C
import Cassandra.Util
import Data.Aeson
import Data.Attoparsec.ByteString
import Data.ByteString.Builder
import Data.ByteString.Conversion
import Data.ByteString.Lazy
import Data.Handle
import Data.Id
import Data.Json.Util
import Data.Text.Encoding
import Database.Bloodhound.Types
import Imports
import Test.QuickCheck
import Wire.API.Team.Feature
import Wire.API.Team.Role
import Wire.API.User
import Wire.API.User.Search
import Wire.Arbitrary

newtype IndexVersion = IndexVersion {docVersion :: DocVersion}

mkIndexVersion :: [Maybe (Writetime x)] -> IndexVersion
mkIndexVersion writetimes =
  let maxVersion = getMax . mconcat . fmap (Max . writetimeToInt64) $ catMaybes writetimes
   in -- This minBound case would only get triggered when the maxVersion is <= 0
      -- or >= 9.2e+18. First case can happen when the writetimes list is empty
      -- or contains a timestamp before the unix epoch, which is unlikely.
      -- Second case will happen in a few billion years. It is also not really a
      -- restriction in ES, Bloodhound's authors' interpretation of the the ES
      -- documentation caused this limiation, otherwise `maxBound :: Int64`,
      -- would be acceptable by ES.
      IndexVersion . fromMaybe minBound . mkDocVersion . fromIntegral $ maxVersion

-- | Represents an ES *document*, ie. the subset of user attributes stored in ES.
-- See also 'IndexUser'.
--
-- If a user is not searchable, e.g. because the account got
-- suspended, all fields except for the user id are set to 'Nothing' and
-- consequently removed from the index.
data UserDoc = UserDoc
  { udId :: UserId,
    udTeam :: Maybe TeamId,
    udName :: Maybe Name,
    udNormalized :: Maybe Text,
    udHandle :: Maybe Handle,
    udEmail :: Maybe EmailAddress,
    udColourId :: Maybe ColourId,
    udAccountStatus :: Maybe AccountStatus,
    udSAMLIdP :: Maybe Text,
    udManagedBy :: Maybe ManagedBy,
    udCreatedAt :: Maybe UTCTimeMillis,
    udRole :: Maybe Role,
    udSearchVisibilityInbound :: Maybe SearchVisibilityInbound,
    udScimExternalId :: Maybe Text,
    udSso :: Maybe Sso,
    udEmailUnvalidated :: Maybe EmailAddress
  }
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform UserDoc)

-- Note: Keep this compatible with the FromJSON instances
-- of 'Contact' and 'TeamContact' from 'Wire.API.User.Search
instance ToJSON UserDoc where
  toJSON ud =
    object
      [ "id" .= udId ud,
        "team" .= udTeam ud,
        "name" .= udName ud,
        "normalized" .= udNormalized ud,
        "handle" .= udHandle ud,
        "email" .= udEmail ud,
        "accent_id" .= udColourId ud,
        "account_status" .= udAccountStatus ud,
        "saml_idp" .= udSAMLIdP ud,
        "managed_by" .= udManagedBy ud,
        "created_at" .= udCreatedAt ud,
        "role" .= udRole ud,
        searchVisibilityInboundFieldName .= udSearchVisibilityInbound ud,
        "scim_external_id" .= udScimExternalId ud,
        "sso" .= udSso ud,
        "email_unvalidated" .= udEmailUnvalidated ud
      ]

instance FromJSON UserDoc where
  parseJSON = withObject "UserDoc" $ \o ->
    UserDoc
      <$> o .: "id"
      <*> o .:? "team"
      <*> o .:? "name"
      <*> o .:? "normalized"
      <*> o .:? "handle"
      <*> o .:? "email"
      <*> o .:? "accent_id"
      <*> o .:? "account_status"
      <*> o .:? "saml_idp"
      <*> o .:? "managed_by"
      <*> o .:? "created_at"
      <*> o .:? "role"
      <*> o .:? searchVisibilityInboundFieldName
      <*> o .:? "scim_external_id"
      <*> o .:? "sso"
      <*> o .:? "email_unvalidated"

searchVisibilityInboundFieldName :: Key
searchVisibilityInboundFieldName = "search_visibility_inbound"

userDocToTeamContact :: UserDoc -> TeamContact
userDocToTeamContact UserDoc {..} =
  TeamContact
    { teamContactUserId = udId,
      teamContactTeam = udTeam,
      teamContactSso = udSso,
      teamContactScimExternalId = udScimExternalId,
      teamContactSAMLIdp = udSAMLIdP,
      teamContactRole = udRole,
      teamContactName = maybe "" fromName udName,
      teamContactManagedBy = udManagedBy,
      teamContactHandle = fromHandle <$> udHandle,
      teamContactEmailUnvalidated = udEmailUnvalidated,
      teamContactEmail = udEmail,
      teamContactCreatedAt = udCreatedAt,
      teamContactColorId = fromIntegral . fromColourId <$> udColourId
    }

-- | Outbound search restrictions configured by team admin of the searcher. This
-- value restricts the set of user that are searched.
--
-- See 'optionallySearchWithinTeam' for the effect on full-text search.
--
-- See 'mkTeamSearchInfo' for the business logic that defines the TeamSearchInfo
-- value.
--
-- Search results might be affected by the inbound search restriction settings of
-- the searched user. ('SearchVisibilityInbound')
data TeamSearchInfo
  = -- | Only users that are not part of any team are searched
    NoTeam
  | -- | Only users from the same team as the searcher are searched
    TeamOnly TeamId
  | -- | No search restrictions, all users are searched
    AllUsers

-- | Inbound search restrictions configured by team to-be-searched. Affects only
-- full-text search (i.e. search on the display name and the handle), not exact
-- handle search.
data SearchVisibilityInbound
  = -- | The user can only be found by users from the same team
    SearchableByOwnTeam
  | -- | The user can by found by any user of any team
    SearchableByAllTeams
  deriving (Eq, Show)

instance Arbitrary SearchVisibilityInbound where
  arbitrary = elements [SearchableByOwnTeam, SearchableByAllTeams]

instance ToByteString SearchVisibilityInbound where
  builder SearchableByOwnTeam = "searchable-by-own-team"
  builder SearchableByAllTeams = "searchable-by-all-teams"

instance FromByteString SearchVisibilityInbound where
  parser =
    SearchableByOwnTeam
      <$ string "searchable-by-own-team"
        <|> SearchableByAllTeams
      <$ string "searchable-by-all-teams"

instance C.Cql SearchVisibilityInbound where
  ctype = C.Tagged C.IntColumn

  toCql SearchableByOwnTeam = C.CqlInt 0
  toCql SearchableByAllTeams = C.CqlInt 1

  fromCql (C.CqlInt 0) = pure SearchableByOwnTeam
  fromCql (C.CqlInt 1) = pure SearchableByAllTeams
  fromCql n = Left $ "Unexpected SearchVisibilityInbound: " ++ show n

defaultSearchVisibilityInbound :: SearchVisibilityInbound
defaultSearchVisibilityInbound = SearchableByOwnTeam

searchVisibilityInboundFromFeatureStatus :: FeatureStatus -> SearchVisibilityInbound
searchVisibilityInboundFromFeatureStatus FeatureStatusDisabled = SearchableByOwnTeam
searchVisibilityInboundFromFeatureStatus FeatureStatusEnabled = SearchableByAllTeams

instance ToJSON SearchVisibilityInbound where
  toJSON = String . decodeUtf8 . toStrict . toLazyByteString . builder

instance FromJSON SearchVisibilityInbound where
  parseJSON = withText "SearchVisibilityInbound" $ \str ->
    case runParser (parser @SearchVisibilityInbound) (encodeUtf8 str) of
      Left err -> fail err
      Right result -> pure result

data IndexQuery r = IndexQuery Query Filter [DefaultSort]

data BrowseTeamFilters = BrowseTeamFilters
  { teamId :: TeamId,
    mQuery :: Maybe Text,
    mRoleFilter :: Maybe RoleFilter,
    mSortBy :: Maybe TeamUserSearchSortBy,
    mSortOrder :: Maybe TeamUserSearchSortOrder
  }
