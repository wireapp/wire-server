{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

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

module Wire.API.User
  ( ListUsersById (..),
    UserIdList (..),
    UserIds (..),
    QualifiedUserIdList (..),
    qualifiedUserIdListObjectSchema,
    LimitedQualifiedUserIdList (..),
    ScimUserInfo (..),
    ScimUserInfos (..),
    UserSet (..),
    -- Profiles
    UserProfile (..),
    SelfProfile (..),
    -- User (should not be here)
    User (..),
    userId,
    userEmail,
    userSSOId,
    userIssuer,
    userSCIMExternalId,
    scimExternalId,
    ssoIssuerAndNameId,
    mkUserProfile,
    mkUserProfileWithEmail,
    userObjectSchema,

    -- * NewUser
    NewUserPublic (..),
    RegisterError (..),
    RegisterSuccess (..),
    RegisterResponses,
    RegisterInternalResponses,
    NewUser (..),
    emptyNewUser,
    NewUserSpar (..),
    CreateUserSparError (..),
    CreateUserSparInternalResponses,
    newUserFromSpar,
    urefToExternalId,
    urefToExternalIdUnsafe,
    urefToEmail,
    ExpiresIn,
    newUserInvitationCode,
    newUserTeam,
    newUserEmail,
    newUserSSOId,
    isNewUserEphemeral,
    isNewUserTeamMember,

    -- * NewUserOrigin
    NewUserOrigin (..),
    InvitationCode (..),
    NewTeamUser (..),
    BindingNewTeamUser (..),

    -- * Profile Updates
    UserUpdate (..),
    UpdateProfileError (..),
    PutSelfResponses,
    PasswordChange (..),
    ChangePasswordError (..),
    ChangePasswordResponses,
    LocaleUpdate (..),
    EmailUpdate (..),
    PhoneUpdate (..),
    ChangePhoneError (..),
    ChangePhoneResponses,
    RemoveIdentityError (..),
    RemoveIdentityResponses,
    HandleUpdate (..),
    ChangeHandleError (..),
    ChangeHandleResponses,
    NameUpdate (..),
    ChangeEmailResponse (..),

    -- * Account Deletion
    DeleteUser (..),
    mkDeleteUser,
    VerifyDeleteUser (..),
    mkVerifyDeleteUser,
    DeletionCodeTimeout (..),
    DeleteUserResponse (..),
    DeleteUserResult (..),

    -- * Account Status
    AccountStatus (..),
    AccountStatusUpdate (..),
    AccountStatusResp (..),

    -- * Account
    UserAccount (..),

    -- * Scim invitations
    NewUserScimInvitation (..),

    -- * List Users
    ListUsersQuery (..),

    -- * misc internal
    GetActivationCodeResp (..),
    GetPasswordResetCodeResp (..),
    CheckBlacklistResponse (..),
    PhoneBudgetTimeout (..),
    ManagedByUpdate (..),
    HavePendingInvitations (..),
    RichInfoUpdate (..),
    PasswordResetPair,
    UpdateSSOIdResponse (..),
    CheckHandleResponse (..),
    UpdateConnectionsInternal (..),
    EmailVisibility (..),
    EmailVisibilityConfig,
    EmailVisibilityConfigWithViewer,

    -- * re-exports
    module Wire.API.Locale,
    module Wire.API.User.Identity,
    module Wire.API.User.Profile,

    -- * 2nd factor auth
    VerificationAction (..),
    SendVerificationCode (..),

    -- * Protocol preferences
    BaseProtocolTag (..),
    baseProtocolToProtocol,
    SupportedProtocolUpdate (..),
    defSupportedProtocols,
    protocolSetBits,
    protocolSetFromBits,
  )
where

import Cassandra qualified as C
import Control.Applicative
import Control.Arrow ((&&&))
import Control.Error.Safe (rightMay)
import Control.Lens (makePrisms, over, view, (.~), (?~), (^.))
import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import Data.Aeson.Types qualified as A
import Data.Attoparsec.ByteString qualified as Parser
import Data.Bits
import Data.ByteString (toStrict)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Conversion
import Data.CaseInsensitive qualified as CI
import Data.Code qualified as Code
import Data.Currency qualified as Currency
import Data.Domain (Domain (Domain))
import Data.Either.Extra (maybeToEither)
import Data.Handle (Handle)
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Id
import Data.Json.Util (UTCTimeMillis, (#))
import Data.LegalHold (UserLegalHoldStatus)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Misc (PlainTextPassword6, PlainTextPassword8)
import Data.OpenApi qualified as S
import Data.Qualified
import Data.Range
import Data.SOP
import Data.Schema
import Data.Schema qualified as Schema
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Ascii
import Data.Text.Encoding qualified as T
import Data.Text.Encoding.Error
import Data.Time.Clock (NominalDiffTime)
import Data.UUID (UUID, nil)
import Data.UUID qualified as UUID
import Deriving.Swagger
import GHC.TypeLits
import Generics.SOP qualified as GSOP
import Imports
import SAML2.WebSSO qualified as SAML
import SAML2.WebSSO.Types.Email qualified as SAMLEmail
import Servant (FromHttpApiData (..), ToHttpApiData (..), type (.++))
import Test.QuickCheck qualified as QC
import URI.ByteString (serializeURIRef)
import Web.Cookie qualified as Web
import Wire.API.Conversation.Protocol
import Wire.API.Error
import Wire.API.Error.Brig
import Wire.API.Error.Brig qualified as E
import Wire.API.Locale
import Wire.API.Provider.Service (ServiceRef)
import Wire.API.Routes.MultiVerb
import Wire.API.Team (BindingNewTeam, bindingNewTeamObjectSchema)
import Wire.API.Team.Member (TeamMember)
import Wire.API.Team.Member qualified as TeamMember
import Wire.API.Team.Role
import Wire.API.User.Activation (ActivationCode, ActivationKey)
import Wire.API.User.Auth (CookieLabel)
import Wire.API.User.Identity
import Wire.API.User.Password
import Wire.API.User.Profile
import Wire.API.User.RichInfo
import Wire.Arbitrary (Arbitrary (arbitrary), GenericUniform (..))

--------------------------------------------------------------------------------
-- UserIdList

-- | This datatype replaces the old `Members` datatype,
-- which has been replaced by `SimpleMembers`. This is
-- needed due to backwards compatible reasons since old
-- clients will break if we switch these types. Also, this
-- definition represents better what information it carries
newtype UserIdList = UserIdList {mUsers :: [UserId]}
  deriving stock (Eq, Show, Generic)
  deriving newtype (Arbitrary)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema UserIdList

instance ToSchema UserIdList where
  schema =
    object "UserIdList" $
      UserIdList
        <$> mUsers
          .= field "user_ids" (array schema)

-- | Response type for endpoints returning lists of users with a specific connection state.
-- E.g. 'getContactList' returns a 'UserIds' containing the list of connections in an
-- 'Accepted' state.
--
-- There really shouldn't be both types `UserIds` and `UserIdList`, but refactoring them
-- away requires changing the api.
newtype UserIds = UserIds
  {cUsers :: [UserId]}
  deriving (Eq, Show, Generic)
  deriving newtype (Arbitrary)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema UserIds

instance ToSchema UserIds where
  schema =
    object "UserIds" $
      UserIds
        <$> cUsers
          .= field "ids" (array schema)

--------------------------------------------------------------------------------
-- misc internal

newtype GetActivationCodeResp = GetActivationCodeResp {fromGetActivationCodeResp :: (ActivationKey, ActivationCode)}
  deriving (Eq, Show, Generic)
  deriving newtype (Arbitrary)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema GetActivationCodeResp

instance ToSchema GetActivationCodeResp where
  schema =
    object "GetActivationCodeResp" $
      curry GetActivationCodeResp
        <$> (fst . fromGetActivationCodeResp) .= field "key" schema
        <*> (snd . fromGetActivationCodeResp) .= field "code" schema

newtype GetPasswordResetCodeResp = GetPasswordResetCodeResp {fromGetPasswordResetCodeResp :: (PasswordResetKey, PasswordResetCode)}
  deriving (Eq, Show, Generic)
  deriving newtype (Arbitrary)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema GetPasswordResetCodeResp

instance ToSchema GetPasswordResetCodeResp where
  schema =
    object "GetPasswordResetCodeResp" $
      curry GetPasswordResetCodeResp
        <$> (fst . fromGetPasswordResetCodeResp) .= field "key" schema
        <*> (snd . fromGetPasswordResetCodeResp) .= field "code" schema

data CheckBlacklistResponse = NotBlacklisted | YesBlacklisted

instance
  AsUnion
    '[ Respond 404 "Not blacklisted" (),
       Respond 200 "Yes blacklisted" ()
     ]
    CheckBlacklistResponse
  where
  toUnion NotBlacklisted = Z (I ())
  toUnion YesBlacklisted = S (Z (I ()))
  fromUnion (Z (I ())) = NotBlacklisted
  fromUnion (S (Z (I ()))) = YesBlacklisted
  fromUnion (S (S x)) = case x of {}

-- | If the budget for SMS and voice calls for a phone number
-- has been exhausted within a certain time frame, this timeout
-- indicates in seconds when another attempt may be made.
newtype PhoneBudgetTimeout = PhoneBudgetTimeout
  {phoneBudgetTimeout :: NominalDiffTime}
  deriving (Eq, Show, Generic)
  deriving newtype (Arbitrary)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema PhoneBudgetTimeout

instance ToSchema PhoneBudgetTimeout where
  schema =
    object "PhoneBudgetTimeout" $
      PhoneBudgetTimeout
        <$> phoneBudgetTimeout .= field "expires_in" nominalDiffTimeSchema

-- | (32bit precision)
nominalDiffTimeSchema :: ValueSchema NamedSwaggerDoc NominalDiffTime
nominalDiffTimeSchema = fromIntegral <$> roundDiffTime .= schema
  where
    roundDiffTime :: NominalDiffTime -> Int32
    roundDiffTime = round

newtype ManagedByUpdate = ManagedByUpdate {mbuManagedBy :: ManagedBy}
  deriving (Eq, Show, Generic)
  deriving newtype (Arbitrary)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ManagedByUpdate

instance ToSchema ManagedByUpdate where
  schema =
    object "ManagedByUpdate" $
      ManagedByUpdate
        <$> mbuManagedBy .= field "managed_by" schema

data HavePendingInvitations
  = WithPendingInvitations
  | NoPendingInvitations
  deriving (Eq, Show, Generic)

newtype RichInfoUpdate = RichInfoUpdate {riuRichInfo :: RichInfoAssocList}
  deriving (Eq, Show, Generic)
  deriving newtype (Arbitrary)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema RichInfoUpdate

instance ToSchema RichInfoUpdate where
  schema =
    object "RichInfoUpdate" $
      RichInfoUpdate
        <$> riuRichInfo .= field "rich_info" schema

type PasswordResetPair = (PasswordResetKey, PasswordResetCode)

-- we recycle that for delete userssoid, too.  can't be bothered.
data UpdateSSOIdResponse = UpdateSSOIdSuccess | UpdateSSOIdNotFound

instance
  AsUnion
    '[ RespondEmpty 200 "UpdateSSOIdSuccess",
       RespondEmpty 404 "UpdateSSOIdNotFound"
     ]
    UpdateSSOIdResponse
  where
  toUnion UpdateSSOIdSuccess = Z (I ())
  toUnion UpdateSSOIdNotFound = S (Z (I ()))
  fromUnion (Z (I ())) = UpdateSSOIdSuccess
  fromUnion (S (Z (I ()))) = UpdateSSOIdNotFound
  fromUnion (S (S x)) = case x of {}

data CheckHandleResponse
  = CheckHandleResponseFound
  | CheckHandleResponseNotFound

instance
  AsUnion
    '[ RespondEmpty 200 "CheckHandleResponseFound",
       RespondEmpty 404 "CheckHandleResponseNotFound"
     ]
    CheckHandleResponse
  where
  toUnion CheckHandleResponseFound = Z (I ())
  toUnion CheckHandleResponseNotFound = S (Z (I ()))
  fromUnion (Z (I ())) = CheckHandleResponseFound
  fromUnion (S (Z (I ()))) = CheckHandleResponseNotFound
  fromUnion (S (S x)) = case x of {}

-- | FUTUREWORK: This needs to get Qualified IDs when implementing
-- Legalhold + Federation, as it's used in the internal
-- putConnectionInternal / galley->Brig "/i/users/connections-status"
-- endpoint.
-- Internal RPCs need to be updated accordingly.
-- See https://wearezeta.atlassian.net/browse/SQCORE-973
data UpdateConnectionsInternal
  = BlockForMissingLHConsent UserId [UserId]
  | RemoveLHBlocksInvolving UserId
  | -- | This must only be used by tests
    CreateConnectionForTest UserId (Qualified UserId)
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform UpdateConnectionsInternal)

$(makePrisms ''UpdateConnectionsInternal)

data UpdateConnectionsInternalTag
  = BlockForMissingLHConsentTag
  | RemoveLHBlocksInvolvingTag
  | CreateConnectionForTestTag
  deriving (Eq, Show, Enum, Bounded)

updateConnectionsInternalTag :: UpdateConnectionsInternal -> UpdateConnectionsInternalTag
updateConnectionsInternalTag (BlockForMissingLHConsent _ _) = BlockForMissingLHConsentTag
updateConnectionsInternalTag (RemoveLHBlocksInvolving _) = RemoveLHBlocksInvolvingTag
updateConnectionsInternalTag (CreateConnectionForTest _ _) = CreateConnectionForTestTag

instance ToSchema UpdateConnectionsInternalTag where
  schema =
    enum @Text "UpdateConnectionsInternalTag" $
      element "BlockForMissingLHConsent" BlockForMissingLHConsentTag
        <> element "RemoveLHBlocksInvolving" RemoveLHBlocksInvolvingTag
        <> element "CreateConnectionForTest" CreateConnectionForTestTag

instance ToSchema UpdateConnectionsInternal where
  schema =
    object "UpdateConnectionsInternal" $
      snd
        <$> (updateConnectionsInternalTag &&& id)
          .= bind
            (fst .= field "tag" tagSchema)
            (snd .= dispatch untaggedSchema)
    where
      tagSchema :: ValueSchema NamedSwaggerDoc UpdateConnectionsInternalTag
      tagSchema = schema

      untaggedSchema ::
        UpdateConnectionsInternalTag ->
        ObjectSchema SwaggerDoc UpdateConnectionsInternal
      untaggedSchema BlockForMissingLHConsentTag =
        tag _BlockForMissingLHConsent $
          (,)
            <$> fst .= field "user" schema
            <*> snd .= field "others" (array schema)
      untaggedSchema RemoveLHBlocksInvolvingTag =
        tag _RemoveLHBlocksInvolving $
          field "user" schema
      untaggedSchema CreateConnectionForTestTag =
        tag _CreateConnectionForTest $
          (,)
            <$> fst .= field "user" schema
            <*> snd .= field "other" schema

deriving via Schema UpdateConnectionsInternal instance (S.ToSchema UpdateConnectionsInternal)

deriving via Schema UpdateConnectionsInternal instance (FromJSON UpdateConnectionsInternal)

deriving via Schema UpdateConnectionsInternal instance (ToJSON UpdateConnectionsInternal)

--------------------------------------------------------------------------------
-- QualifiedUserIdList

newtype QualifiedUserIdList = QualifiedUserIdList {qualifiedUserIdList :: [Qualified UserId]}
  deriving stock (Eq, Show, Generic)
  deriving newtype (Arbitrary)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema QualifiedUserIdList

instance ToSchema QualifiedUserIdList where
  schema =
    object "QualifiedUserIdList" qualifiedUserIdListObjectSchema

qualifiedUserIdListObjectSchema :: ObjectSchema SwaggerDoc QualifiedUserIdList
qualifiedUserIdListObjectSchema =
  QualifiedUserIdList
    <$> qualifiedUserIdList
      .= field "qualified_user_ids" (array schema)
    <* (fmap qUnqualified . qualifiedUserIdList)
      .= field "user_ids" (deprecatedSchema "qualified_user_ids" (array schema))

--------------------------------------------------------------------------------
-- LimitedQualifiedUserIdList

-- | We cannot use 'Wrapped' here because all the instances require proof that 1
-- is less than or equal to 'max'.
newtype LimitedQualifiedUserIdList (max :: Nat) = LimitedQualifiedUserIdList
  {qualifiedUsers :: Range 1 max [Qualified UserId]}
  deriving stock (Eq, Show, Generic)
  deriving (S.ToSchema) via CustomSwagger '[FieldLabelModifier CamelToSnake] (LimitedQualifiedUserIdList max)

instance (KnownNat max, 1 <= max) => Arbitrary (LimitedQualifiedUserIdList max) where
  arbitrary = LimitedQualifiedUserIdList <$> arbitrary

instance (KnownNat max, 1 <= max) => FromJSON (LimitedQualifiedUserIdList max) where
  parseJSON = A.withObject "LimitedQualifiedUserIdList" $ \o ->
    LimitedQualifiedUserIdList <$> o A..: "qualified_users"

instance (1 <= max) => ToJSON (LimitedQualifiedUserIdList max) where
  toJSON e = A.object ["qualified_users" A..= qualifiedUsers e]

--------------------------------------------------------------------------------
-- UserProfile

-- | A subset of the data of an existing 'User' that is returned on the API and is visible to
-- other users. Each user also has access to their own profile in a richer format --
-- 'SelfProfile'.
data UserProfile = UserProfile
  { profileQualifiedId :: Qualified UserId,
    profileName :: Name,
    -- | DEPRECATED
    profilePict :: Pict,
    profileAssets :: [Asset],
    profileAccentId :: ColourId,
    profileDeleted :: Bool,
    -- | Set if the user represents an external service,
    -- i.e. it is a "bot".
    profileService :: Maybe ServiceRef,
    profileHandle :: Maybe Handle,
    profileExpire :: Maybe UTCTimeMillis,
    profileTeam :: Maybe TeamId,
    profileEmail :: Maybe Email,
    profileLegalholdStatus :: UserLegalHoldStatus,
    profileSupportedProtocols :: Set BaseProtocolTag
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform UserProfile)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema UserProfile)

instance ToSchema UserProfile where
  schema =
    object "UserProfile" $
      UserProfile
        <$> profileQualifiedId
          .= field "qualified_id" schema
        <* (qUnqualified . profileQualifiedId)
          .= optional (field "id" (deprecatedSchema "qualified_id" schema))
        <*> profileName
          .= field "name" schema
        <*> profilePict
          .= (field "picture" schema <|> pure noPict)
        <*> profileAssets
          .= (field "assets" (array schema) <|> pure [])
        <*> profileAccentId
          .= field "accent_id" schema
        <*> ((\del -> if del then Just True else Nothing) . profileDeleted)
          .= maybe_ (fromMaybe False <$> optField "deleted" schema)
        <*> profileService
          .= maybe_ (optField "service" schema)
        <*> profileHandle
          .= maybe_ (optField "handle" schema)
        <*> profileExpire
          .= maybe_ (optField "expires_at" schema)
        <*> profileTeam
          .= maybe_ (optField "team" schema)
        <*> profileEmail
          .= maybe_ (optField "email" schema)
        <*> profileLegalholdStatus
          .= field "legalhold_status" schema
        <*> profileSupportedProtocols .= supportedProtocolsObjectSchema

--------------------------------------------------------------------------------
-- SelfProfile

-- | A self profile.
newtype SelfProfile = SelfProfile
  { selfUser :: User
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform SelfProfile)
  deriving newtype (S.ToSchema)

instance ToJSON SelfProfile where
  toJSON (SelfProfile u) = toJSON u

instance FromJSON SelfProfile where
  parseJSON = A.withObject "SelfProfile" $ \o ->
    SelfProfile <$> parseJSON (A.Object o)

--------------------------------------------------------------------------------
-- User
--
-- FUTUREWORK: Move this type somewhere else, it's not part of the client API.

-- | The data of an existing user.
data User = User
  { userQualifiedId :: Qualified UserId,
    -- | User identity. For endpoints like @/self@, it will be present in the response iff
    -- the user is activated, and the email/phone contained in it will be guaranteedly
    -- verified. {#RefActivation}
    userIdentity :: Maybe UserIdentity,
    -- | required; non-unique
    userDisplayName :: Name,
    -- | DEPRECATED
    userPict :: Pict,
    userAssets :: [Asset],
    userAccentId :: ColourId,
    userDeleted :: Bool,
    userLocale :: Locale,
    -- | Set if the user represents an external service,
    -- i.e. it is a "bot".
    userService :: Maybe ServiceRef,
    -- | not required; must be unique if present
    userHandle :: Maybe Handle,
    -- | Set if the user is ephemeral
    userExpire :: Maybe UTCTimeMillis,
    -- | Set if the user is part of a binding team
    userTeam :: Maybe TeamId,
    -- | How is the user profile managed (e.g. if it's via SCIM then the user profile
    -- can't be edited via normal means)
    userManagedBy :: ManagedBy,
    userSupportedProtocols :: Set BaseProtocolTag
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform User)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema User)

userId :: User -> UserId
userId = qUnqualified . userQualifiedId

-- -- FUTUREWORK:
-- -- disentangle json serializations for 'User', 'NewUser', 'UserIdentity', 'NewUserOrigin'.
instance ToSchema User where
  schema = object "User" userObjectSchema

userObjectSchema :: ObjectSchema SwaggerDoc User
userObjectSchema =
  User
    <$> userQualifiedId
      .= field "qualified_id" schema
    <* userId
      .= optional (field "id" (deprecatedSchema "qualified_id" schema))
    <*> userIdentity
      .= maybeUserIdentityObjectSchema
    <*> userDisplayName
      .= field "name" schema
    <*> userPict
      .= (fromMaybe noPict <$> optField "picture" schema)
    <*> userAssets
      .= (fromMaybe [] <$> optField "assets" (array schema))
    <*> userAccentId
      .= field "accent_id" schema
    <*> (fromMaybe False <$> (\u -> if userDeleted u then Just True else Nothing) .= maybe_ (optField "deleted" schema))
    <*> userLocale
      .= field "locale" schema
    <*> userService
      .= maybe_ (optField "service" schema)
    <*> userHandle
      .= maybe_ (optField "handle" schema)
    <*> userExpire
      .= maybe_ (optField "expires_at" schema)
    <*> userTeam
      .= maybe_ (optField "team" schema)
    <*> userManagedBy
      .= (fromMaybe ManagedByWire <$> optField "managed_by" schema)
    <*> userSupportedProtocols .= supportedProtocolsObjectSchema

userEmail :: User -> Maybe Email
userEmail = emailIdentity <=< userIdentity

userSSOId :: User -> Maybe UserSSOId
userSSOId = ssoIdentity <=< userIdentity

userSCIMExternalId :: User -> Maybe Text
userSCIMExternalId usr = scimExternalId (userManagedBy usr) =<< userSSOId usr

-- FUTUREWORK: this is only ignoring case in the email format, and emails should be
-- handled case-insensitively.  https://wearezeta.atlassian.net/browse/SQSERVICES-909
scimExternalId :: ManagedBy -> UserSSOId -> Maybe Text
scimExternalId _ (UserScimExternalId extId) = Just extId
scimExternalId ManagedByScim (UserSSOId (SAML.UserRef _ nameIdXML)) = Just . CI.original . SAML.unsafeShowNameID $ nameIdXML
scimExternalId ManagedByWire (UserSSOId _) = Nothing

ssoIssuerAndNameId :: UserSSOId -> Maybe (Text, Text)
ssoIssuerAndNameId (UserSSOId (SAML.UserRef (SAML.Issuer uri) nameIdXML)) = Just (fromUri uri, fromNameId nameIdXML)
  where
    fromUri =
      T.decodeUtf8With lenientDecode
        . toStrict
        . toLazyByteString
        . serializeURIRef
    fromNameId = CI.original . SAML.unsafeShowNameID
ssoIssuerAndNameId (UserScimExternalId _) = Nothing

userIssuer :: User -> Maybe SAML.Issuer
userIssuer user = userSSOId user >>= fromSSOId
  where
    fromSSOId :: UserSSOId -> Maybe SAML.Issuer
    fromSSOId (UserSSOId (SAML.UserRef issuer _)) = Just issuer
    fromSSOId _ = Nothing

-- | Configurations for whether to show a user's email to others.
data EmailVisibility a
  = -- | Anyone can see the email of someone who is on ANY team.
    --         This may sound strange; but certain on-premise hosters have many different teams
    --         and still want them to see each-other's emails.
    EmailVisibleIfOnTeam
  | -- | Anyone on your team with at least 'Member' privileges can see your email address.
    EmailVisibleIfOnSameTeam a
  | -- | Show your email only to yourself
    EmailVisibleToSelf
  deriving (Eq, Show, Generic, Functor, Foldable, Traversable)
  deriving (Arbitrary) via (GenericUniform (EmailVisibility a))

type EmailVisibilityConfig = EmailVisibility ()

type EmailVisibilityConfigWithViewer = EmailVisibility (Maybe (TeamId, TeamMember))

instance FromJSON (EmailVisibility ()) where
  parseJSON = withText "EmailVisibility" $ \case
    "visible_if_on_team" -> pure EmailVisibleIfOnTeam
    "visible_if_on_same_team" -> pure $ EmailVisibleIfOnSameTeam ()
    "visible_to_self" -> pure EmailVisibleToSelf
    _ -> fail "unexpected value for EmailVisibility settings"

mkUserProfileWithEmail :: Maybe Email -> User -> UserLegalHoldStatus -> UserProfile
mkUserProfileWithEmail memail u legalHoldStatus =
  -- This profile would be visible to any other user. When a new field is
  -- added, please make sure it is OK for other users to have access to it.
  UserProfile
    { profileQualifiedId = userQualifiedId u,
      profileHandle = userHandle u,
      profileName = userDisplayName u,
      profilePict = userPict u,
      profileAssets = userAssets u,
      profileAccentId = userAccentId u,
      profileService = userService u,
      profileDeleted = userDeleted u,
      profileExpire = userExpire u,
      profileTeam = userTeam u,
      profileEmail = memail,
      profileLegalholdStatus = legalHoldStatus,
      profileSupportedProtocols = userSupportedProtocols u
    }

mkUserProfile :: EmailVisibilityConfigWithViewer -> User -> UserLegalHoldStatus -> UserProfile
mkUserProfile emailVisibilityConfigAndViewer u legalHoldStatus =
  let isEmailVisible = case emailVisibilityConfigAndViewer of
        EmailVisibleToSelf -> False
        EmailVisibleIfOnTeam -> isJust (userTeam u)
        EmailVisibleIfOnSameTeam Nothing -> False
        EmailVisibleIfOnSameTeam (Just (viewerTeamId, viewerMembership)) ->
          Just viewerTeamId == userTeam u
            && TeamMember.hasPermission viewerMembership TeamMember.ViewSameTeamEmails
   in mkUserProfileWithEmail (if isEmailVisible then userEmail u else Nothing) u legalHoldStatus

--------------------------------------------------------------------------------
-- NewUser

-- | We use the same 'NewUser' type for the @\/register@ and @\/i\/users@ endpoints. This
-- newtype is used as request body type for the public @\/register@ endpoint, where only a
-- subset of the 'NewUser' functionality should be allowed.
--
-- Specifically, we forbid the following:
--
--   * Setting 'SSOIdentity' (SSO users are created by Spar)
--
--   * Setting the UUID (only needed so that Spar can find the user if Spar crashes before it
--     finishes creating the user).
--
--   * Setting 'ManagedBy' (it should be the default in all cases unless Spar creates a
--     SCIM-managed user)
newtype NewUserPublic = NewUserPublic NewUser
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema NewUserPublic)

instance ToSchema NewUserPublic where
  schema =
    unwrap .= withParser schema (either fail pure . validateNewUserPublic)
    where
      unwrap (NewUserPublic nu) = nu

validateNewUserPublic :: NewUser -> Either String NewUserPublic
validateNewUserPublic nu
  | isJust (newUserSSOId nu) =
      Left "SSO-managed users are not allowed here."
  | isJust (newUserUUID nu) =
      Left "it is not allowed to provide a UUID for the users here."
  | newUserManagedBy nu `notElem` [Nothing, Just ManagedByWire] =
      Left "only managed-by-Wire users can be created here."
  | otherwise =
      Right (NewUserPublic nu)

-- | A user is Ephemeral if she has neither email, phone, nor sso credentials and is not
-- created via scim.  Ephemeral users can be deleted after expires_in or sessionTokenTimeout
-- (whichever comes earlier).
isNewUserEphemeral :: NewUser -> Bool
isNewUserEphemeral u = noId && noScim
  where
    noId = isNothing $ newUserIdentity u
    noScim = case newUserManagedBy u of
      Nothing -> True
      Just ManagedByWire -> True
      Just ManagedByScim -> False

isNewUserTeamMember :: NewUser -> Bool
isNewUserTeamMember u = case newUserTeam u of
  Just (NewTeamMember _) -> True
  Just (NewTeamMemberSSO _) -> True
  Just (NewTeamCreator _) -> False
  Nothing -> False

instance Arbitrary NewUserPublic where
  arbitrary = arbitrary `QC.suchThatMap` (rightMay . validateNewUserPublic)

data RegisterError
  = RegisterErrorAllowlistError
  | RegisterErrorInvalidInvitationCode
  | RegisterErrorMissingIdentity
  | RegisterErrorUserKeyExists
  | RegisterErrorInvalidActivationCodeWrongUser
  | RegisterErrorInvalidActivationCodeWrongCode
  | RegisterErrorInvalidEmail
  | RegisterErrorInvalidPhone
  | RegisterErrorBlacklistedPhone
  | RegisterErrorBlacklistedEmail
  | RegisterErrorTooManyTeamMembers
  | RegisterErrorUserCreationRestricted
  deriving (Show, Generic)
  deriving (AsUnion RegisterErrorResponses) via GenericAsUnion RegisterErrorResponses RegisterError

instance GSOP.Generic RegisterError

type RegisterErrorResponses =
  '[ ErrorResponse 'AllowlistError,
     ErrorResponse 'InvalidInvitationCode,
     ErrorResponse 'MissingIdentity,
     ErrorResponse 'UserKeyExists,
     ErrorResponse 'InvalidActivationCodeWrongUser,
     ErrorResponse 'InvalidActivationCodeWrongCode,
     ErrorResponse 'InvalidEmail,
     ErrorResponse 'InvalidPhone,
     ErrorResponse 'BlacklistedPhone,
     ErrorResponse 'BlacklistedEmail,
     ErrorResponse 'TooManyTeamMembers,
     ErrorResponse 'UserCreationRestricted
   ]

type RegisterResponses =
  RegisterErrorResponses
    .++ '[ WithHeaders
             '[ DescHeader "Set-Cookie" "Cookie" Web.SetCookie,
                DescHeader "Location" "UserId" UserId
              ]
             RegisterSuccess
             (Respond 201 "User created and pending activation" SelfProfile)
         ]

instance AsHeaders '[Web.SetCookie, UserId] SelfProfile RegisterSuccess where
  fromHeaders (I cookie :* (_ :* Nil), sp) = RegisterSuccess cookie sp
  toHeaders (RegisterSuccess cookie sp) = (I cookie :* (I (userId (selfUser sp)) :* Nil), sp)

data RegisterSuccess = RegisterSuccess Web.SetCookie SelfProfile

instance (res ~ RegisterResponses) => AsUnion res (Either RegisterError RegisterSuccess) where
  toUnion = eitherToUnion (toUnion @RegisterErrorResponses) (Z . I)
  fromUnion = eitherFromUnion (fromUnion @RegisterErrorResponses) (unI . unZ)

type RegisterInternalResponses =
  RegisterErrorResponses
    .++ '[ WithHeaders
             '[DescHeader "Location" "UserId" UserId]
             SelfProfile
             (Respond 201 "User created and pending activation" SelfProfile)
         ]

instance AsHeaders '[UserId] SelfProfile SelfProfile where
  fromHeaders (_ :* Nil, sp) = sp
  toHeaders sp = (I (userId (selfUser sp)) :* Nil, sp)

instance (res ~ RegisterInternalResponses) => AsUnion res (Either RegisterError SelfProfile) where
  toUnion = eitherToUnion (toUnion @RegisterErrorResponses) (Z . I)
  fromUnion = eitherFromUnion (fromUnion @RegisterErrorResponses) (unI . unZ)

urefToExternalId :: SAML.UserRef -> Maybe Text
urefToExternalId = fmap CI.original . SAML.shortShowNameID . view SAML.uidSubject

urefToEmail :: SAML.UserRef -> Maybe Email
urefToEmail uref = case uref ^. SAML.uidSubject . SAML.nameID of
  SAML.UNameIDEmail email -> parseEmail . SAMLEmail.render . CI.original $ email
  _ -> Nothing

urefToExternalIdUnsafe :: SAML.UserRef -> Text
urefToExternalIdUnsafe = CI.original . SAML.unsafeShowNameID . view SAML.uidSubject

data CreateUserSparError
  = CreateUserSparHandleError ChangeHandleError
  | CreateUserSparRegistrationError RegisterError
  deriving (Show, Generic)

type CreateUserSparErrorResponses =
  RegisterErrorResponses .++ ChangeHandleErrorResponses

type CreateUserSparResponses =
  CreateUserSparErrorResponses
    .++ '[ WithHeaders
             '[ DescHeader "Set-Cookie" "Cookie" Web.SetCookie,
                DescHeader "Location" "UserId" UserId
              ]
             RegisterSuccess
             (Respond 201 "User created and pending activation" SelfProfile)
         ]

type CreateUserSparInternalResponses =
  CreateUserSparErrorResponses
    .++ '[ WithHeaders
             '[DescHeader "Location" "UserId" UserId]
             SelfProfile
             (Respond 201 "User created and pending activation" SelfProfile)
         ]

instance (res ~ CreateUserSparErrorResponses) => AsUnion res CreateUserSparError where
  toUnion = eitherToUnion (toUnion @ChangeHandleErrorResponses) (toUnion @RegisterErrorResponses) . errToEither
  fromUnion = errFromEither . eitherFromUnion (fromUnion @ChangeHandleErrorResponses) (fromUnion @RegisterErrorResponses)

instance (res ~ CreateUserSparResponses) => AsUnion res (Either CreateUserSparError RegisterSuccess) where
  toUnion = eitherToUnion (toUnion @CreateUserSparErrorResponses) (Z . I)
  fromUnion = eitherFromUnion (fromUnion @CreateUserSparErrorResponses) (unI . unZ)

instance (res ~ CreateUserSparInternalResponses) => AsUnion res (Either CreateUserSparError SelfProfile) where
  toUnion = eitherToUnion (toUnion @CreateUserSparErrorResponses) (Z . I)
  fromUnion = eitherFromUnion (fromUnion @CreateUserSparErrorResponses) (unI . unZ)

errToEither :: CreateUserSparError -> Either ChangeHandleError RegisterError
errToEither (CreateUserSparHandleError e) = Left e
errToEither (CreateUserSparRegistrationError e) = Right e

errFromEither :: Either ChangeHandleError RegisterError -> CreateUserSparError
errFromEither (Left e) = CreateUserSparHandleError e
errFromEither (Right e) = CreateUserSparRegistrationError e

data NewUserSpar = NewUserSpar
  { newUserSparUUID :: UUID,
    newUserSparSSOId :: UserSSOId,
    newUserSparDisplayName :: Name,
    newUserSparTeamId :: TeamId,
    newUserSparManagedBy :: ManagedBy,
    newUserSparHandle :: Maybe Handle,
    newUserSparRichInfo :: Maybe RichInfo,
    newUserSparLocale :: Maybe Locale,
    newUserSparRole :: Role
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema NewUserSpar)

instance ToSchema NewUserSpar where
  schema =
    object "NewUserSpar" $
      NewUserSpar
        <$> newUserSparUUID
          .= field "newUserSparUUID" genericToSchema
        <*> newUserSparSSOId
          .= field "newUserSparSSOId" genericToSchema
        <*> newUserSparDisplayName
          .= field "newUserSparDisplayName" schema
        <*> newUserSparTeamId
          .= field "newUserSparTeamId" schema
        <*> newUserSparManagedBy
          .= field "newUserSparManagedBy" schema
        <*> newUserSparHandle
          .= maybe_ (optField "newUserSparHandle" schema)
        <*> newUserSparRichInfo
          .= maybe_ (optField "newUserSparRichInfo" schema)
        <*> newUserSparLocale
          .= maybe_ (optField "newUserSparLocale" schema)
        <*> newUserSparRole
          .= field "newUserSparRole" schema

newUserFromSpar :: NewUserSpar -> NewUser
newUserFromSpar new =
  NewUser
    { newUserDisplayName = newUserSparDisplayName new,
      newUserUUID = Just $ newUserSparUUID new,
      newUserIdentity = Just $ SSOIdentity (newUserSparSSOId new) Nothing,
      newUserPhone = Nothing,
      newUserPict = Nothing,
      newUserAssets = [],
      newUserAccentId = Nothing,
      newUserEmailCode = Nothing,
      newUserPhoneCode = Nothing,
      newUserOrigin = Just . NewUserOriginTeamUser . NewTeamMemberSSO $ newUserSparTeamId new,
      newUserLabel = Nothing,
      newUserPassword = Nothing,
      newUserExpiresIn = Nothing,
      newUserManagedBy = Just $ newUserSparManagedBy new,
      newUserLocale = newUserSparLocale new,
      newUserSupportedProtocols = Nothing
    }

data NewUser = NewUser
  { newUserDisplayName :: Name,
    -- | use this as 'UserId' (if 'Nothing', call 'Data.UUID.nextRandom').
    newUserUUID :: Maybe UUID,
    newUserIdentity :: Maybe UserIdentity,
    newUserPhone :: Maybe Phone,
    -- | DEPRECATED
    newUserPict :: Maybe Pict,
    newUserAssets :: [Asset],
    newUserAccentId :: Maybe ColourId,
    newUserEmailCode :: Maybe ActivationCode,
    newUserPhoneCode :: Maybe ActivationCode,
    newUserOrigin :: Maybe NewUserOrigin,
    newUserLabel :: Maybe CookieLabel,
    newUserLocale :: Maybe Locale,
    newUserPassword :: Maybe PlainTextPassword8,
    newUserExpiresIn :: Maybe ExpiresIn,
    newUserManagedBy :: Maybe ManagedBy,
    newUserSupportedProtocols :: Maybe (Set BaseProtocolTag)
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema NewUser)

emptyNewUser :: Name -> NewUser
emptyNewUser name =
  NewUser
    { newUserDisplayName = name,
      newUserUUID = Nothing,
      newUserIdentity = Nothing,
      newUserPhone = Nothing,
      newUserPict = Nothing,
      newUserAssets = [],
      newUserAccentId = Nothing,
      newUserEmailCode = Nothing,
      newUserPhoneCode = Nothing,
      newUserOrigin = Nothing,
      newUserLabel = Nothing,
      newUserLocale = Nothing,
      newUserPassword = Nothing,
      newUserExpiresIn = Nothing,
      newUserManagedBy = Nothing,
      newUserSupportedProtocols = Nothing
    }

-- | 1 second - 1 week
type ExpiresIn = Range 1 604800 Integer

-- | Raw representation of 'NewUser' to help with writing Schema instances.
data NewUserRaw = NewUserRaw
  { newUserRawDisplayName :: Name,
    newUserRawUUID :: Maybe UUID,
    newUserRawEmail :: Maybe Email,
    -- | This is deprecated and it should always be 'Nothing'.
    newUserRawPhone :: Maybe Phone,
    newUserRawSSOId :: Maybe UserSSOId,
    -- | DEPRECATED
    newUserRawPict :: Maybe Pict,
    newUserRawAssets :: [Asset],
    newUserRawAccentId :: Maybe ColourId,
    newUserRawEmailCode :: Maybe ActivationCode,
    -- | This is deprecated and it should always be 'Nothing'.
    newUserRawPhoneCode :: Maybe ActivationCode,
    newUserRawInvitationCode :: Maybe InvitationCode,
    newUserRawTeamCode :: Maybe InvitationCode,
    newUserRawTeam :: Maybe BindingNewTeamUser,
    newUserRawTeamId :: Maybe TeamId,
    newUserRawLabel :: Maybe CookieLabel,
    newUserRawLocale :: Maybe Locale,
    newUserRawPassword :: Maybe PlainTextPassword8,
    newUserRawExpiresIn :: Maybe ExpiresIn,
    newUserRawManagedBy :: Maybe ManagedBy,
    newUserRawSupportedProtocols :: Maybe (Set BaseProtocolTag)
  }

newUserRawObjectSchema :: ObjectSchema SwaggerDoc NewUserRaw
newUserRawObjectSchema =
  NewUserRaw
    <$> newUserRawDisplayName
      .= field "name" schema
    <*> newUserRawUUID
      .= maybe_ (optField "uuid" genericToSchema)
    <*> newUserRawEmail
      .= maybe_ (optField "email" schema)
    <*> newUserRawPhone
      .= maybe_ (optField "phone" schema)
    <*> newUserRawSSOId
      .= maybe_ (optField "sso_id" genericToSchema)
    <*> newUserRawPict
      .= maybe_ (optField "picture" schema)
    <*> newUserRawAssets
      .= (fromMaybe [] <$> optField "assets" (array schema))
    <*> newUserRawAccentId
      .= maybe_ (optField "accent_id" schema)
    <*> newUserRawEmailCode
      .= maybe_ (optField "email_code" schema)
    <*> newUserRawPhoneCode
      .= maybe_ (optField "phone_code" schema)
    <*> newUserRawInvitationCode
      .= maybe_ (optField "invitation_code" schema)
    <*> newUserRawTeamCode
      .= maybe_ (optField "team_code" schema)
    <*> newUserRawTeam
      .= maybe_ (optField "team" schema)
    <*> newUserRawTeamId
      .= maybe_ (optField "team_id" schema)
    <*> newUserRawLabel
      .= maybe_ (optField "label" schema)
    <*> newUserRawLocale
      .= maybe_ (optField "locale" schema)
    <*> newUserRawPassword
      .= maybe_ (optField "password" schema)
    <*> newUserRawExpiresIn
      .= maybe_ (optField "expires_in" schema)
    <*> newUserRawManagedBy
      .= maybe_ (optField "managed_by" schema)
    <*> newUserRawSupportedProtocols
      .= maybe_ (optField "supported_protocols" (set schema))

instance ToSchema NewUser where
  schema =
    object "NewUser" $ newUserToRaw .= withParser newUserRawObjectSchema newUserFromRaw

newUserToRaw :: NewUser -> NewUserRaw
newUserToRaw NewUser {..} =
  let maybeOriginNTU = newUserOriginNewTeamUser =<< newUserOrigin
   in NewUserRaw
        { newUserRawDisplayName = newUserDisplayName,
          newUserRawUUID = newUserUUID,
          newUserRawEmail = emailIdentity =<< newUserIdentity,
          newUserRawPhone = newUserPhone,
          newUserRawSSOId = ssoIdentity =<< newUserIdentity,
          newUserRawPict = newUserPict,
          newUserRawAssets = newUserAssets,
          newUserRawAccentId = newUserAccentId,
          newUserRawEmailCode = newUserEmailCode,
          newUserRawPhoneCode = newUserPhoneCode,
          newUserRawInvitationCode = newUserOriginInvitationCode =<< newUserOrigin,
          newUserRawTeamCode = newTeamUserCode =<< maybeOriginNTU,
          newUserRawTeam = newTeamUserCreator =<< maybeOriginNTU,
          newUserRawTeamId = newTeamUserTeamId =<< maybeOriginNTU,
          newUserRawLabel = newUserLabel,
          newUserRawLocale = newUserLocale,
          newUserRawPassword = newUserPassword,
          newUserRawExpiresIn = newUserExpiresIn,
          newUserRawManagedBy = newUserManagedBy,
          newUserRawSupportedProtocols = newUserSupportedProtocols
        }

newUserFromRaw :: NewUserRaw -> A.Parser NewUser
newUserFromRaw NewUserRaw {..} = do
  origin <-
    either fail pure $
      maybeNewUserOriginFromComponents
        (isJust newUserRawPassword)
        (isJust newUserRawSSOId)
        (newUserRawInvitationCode, newUserRawTeamCode, newUserRawTeam, newUserRawTeamId)
  let identity =
        maybeUserIdentityFromComponents
          (newUserRawEmail, newUserRawSSOId)
  expiresIn <-
    case (newUserRawExpiresIn, identity) of
      (Just _, Just _) -> fail "Only users without an identity can expire"
      _ -> pure newUserRawExpiresIn
  pure $
    NewUser
      { newUserDisplayName = newUserRawDisplayName,
        newUserUUID = newUserRawUUID,
        newUserIdentity = identity,
        newUserPhone = newUserRawPhone,
        newUserPict = newUserRawPict,
        newUserAssets = newUserRawAssets,
        newUserAccentId = newUserRawAccentId,
        newUserEmailCode = newUserRawEmailCode,
        newUserPhoneCode = newUserRawPhoneCode,
        newUserOrigin = origin,
        newUserLabel = newUserRawLabel,
        newUserLocale = newUserRawLocale,
        newUserPassword = newUserRawPassword,
        newUserExpiresIn = expiresIn,
        newUserManagedBy = newUserRawManagedBy,
        newUserSupportedProtocols = newUserRawSupportedProtocols
      }

-- FUTUREWORK: align more with FromJSON instance?
instance Arbitrary NewUser where
  arbitrary = do
    newUserIdentity <- arbitrary
    newUserPhone <- arbitrary
    newUserOrigin <- genUserOrigin newUserIdentity
    newUserDisplayName <- arbitrary
    newUserUUID <- QC.elements [Just nil, Nothing]
    newUserPict <- arbitrary
    newUserAssets <- arbitrary
    newUserAccentId <- arbitrary
    newUserEmailCode <- arbitrary
    newUserPhoneCode <- arbitrary
    newUserLabel <- arbitrary
    newUserLocale <- arbitrary
    newUserPassword <- genUserPassword newUserIdentity newUserOrigin
    newUserExpiresIn <- genUserExpiresIn newUserIdentity
    newUserManagedBy <- arbitrary
    newUserSupportedProtocols <- arbitrary
    pure NewUser {..}
    where
      genUserOrigin newUserIdentity = do
        teamid <- arbitrary
        let hasSSOId = case newUserIdentity of
              Just SSOIdentity {} -> True
              _ -> False
            ssoOrigin = Just (NewUserOriginTeamUser (NewTeamMemberSSO teamid))
            isSsoOrigin (Just (NewUserOriginTeamUser (NewTeamMemberSSO _))) = True
            isSsoOrigin _ = False
        if hasSSOId
          then pure ssoOrigin
          else arbitrary `QC.suchThat` (not . isSsoOrigin)
      genUserPassword newUserIdentity newUserOrigin = do
        let hasSSOId = case newUserIdentity of
              Just SSOIdentity {} -> True
              _ -> False
            isTeamUser = case newUserOrigin of
              Just (NewUserOriginTeamUser _) -> True
              _ -> False
        if isTeamUser && not hasSSOId then Just <$> arbitrary else arbitrary
      genUserExpiresIn newUserIdentity =
        if isJust newUserIdentity then pure Nothing else arbitrary

newUserInvitationCode :: NewUser -> Maybe InvitationCode
newUserInvitationCode nu = case newUserOrigin nu of
  Just (NewUserOriginInvitationCode ic) -> Just ic
  _ -> Nothing

newUserTeam :: NewUser -> Maybe NewTeamUser
newUserTeam nu = case newUserOrigin nu of
  Just (NewUserOriginTeamUser tu) -> Just tu
  _ -> Nothing

newUserEmail :: NewUser -> Maybe Email
newUserEmail = emailIdentity <=< newUserIdentity

newUserSSOId :: NewUser -> Maybe UserSSOId
newUserSSOId = ssoIdentity <=< newUserIdentity

--------------------------------------------------------------------------------
-- NewUserOrigin

data NewUserOrigin
  = NewUserOriginInvitationCode InvitationCode
  | NewUserOriginTeamUser NewTeamUser
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform NewUserOrigin)

type NewUserOriginComponents = (Maybe InvitationCode, Maybe InvitationCode, Maybe BindingNewTeamUser, Maybe TeamId)

newUserOriginInvitationCode :: NewUserOrigin -> Maybe InvitationCode
newUserOriginInvitationCode = \case
  NewUserOriginInvitationCode ic -> Just ic
  NewUserOriginTeamUser _ -> Nothing

newUserOriginNewTeamUser :: NewUserOrigin -> Maybe NewTeamUser
newUserOriginNewTeamUser = \case
  NewUserOriginInvitationCode _ -> Nothing
  NewUserOriginTeamUser ntu -> Just ntu

maybeNewUserOriginFromComponents ::
  -- | Does the user have a password
  Bool ->
  -- | Does the user have an SSO Identity
  Bool ->
  NewUserOriginComponents ->
  Either String (Maybe NewUserOrigin)
maybeNewUserOriginFromComponents hasPassword hasSSO (invcode, teamcode, team, teamid) = do
  result <- case (invcode, teamcode, team, hasSSO, teamid) of
    (Just a, Nothing, Nothing, False, Nothing) -> Right . Just . NewUserOriginInvitationCode $ a
    (Nothing, Just a, Nothing, False, Nothing) -> Right . Just . NewUserOriginTeamUser $ NewTeamMember a
    (Nothing, Nothing, Just a, False, Nothing) -> Right . Just . NewUserOriginTeamUser $ NewTeamCreator a
    (Nothing, Nothing, Nothing, True, Just t) -> Right . Just . NewUserOriginTeamUser $ NewTeamMemberSSO t
    (Nothing, Nothing, Nothing, False, Nothing) -> Right Nothing
    (_, _, _, True, Nothing) -> Left "sso_id, team_id must be either both present or both absent."
    (_, _, _, False, Just _) -> Left "sso_id, team_id must be either both present or both absent."
    _ -> Left "team_code, team, invitation_code, sso_id, and the pair (sso_id, team_id) are mutually exclusive"
  case (result, hasPassword, hasSSO) of
    (_, _, True) -> Right result
    (Just (NewUserOriginTeamUser _), False, _) -> Left "all team users must set a password on creation"
    _ -> pure result

-- | A random invitation code for use during registration
newtype InvitationCode = InvitationCode
  {fromInvitationCode :: AsciiBase64Url}
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToSchema, ToByteString, FromByteString, Arbitrary)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema InvitationCode

instance S.ToParamSchema InvitationCode where
  toParamSchema _ = S.toParamSchema (Proxy @Text)

instance FromHttpApiData InvitationCode where
  parseQueryParam = bimap T.pack InvitationCode . validateBase64Url

instance ToHttpApiData InvitationCode where
  toQueryParam =
    T.decodeUtf8With lenientDecode
      . toStrict
      . toByteString
      . fromInvitationCode

deriving instance C.Cql InvitationCode

--------------------------------------------------------------------------------
-- NewTeamUser

data NewTeamUser
  = -- | requires email address
    NewTeamMember InvitationCode
  | NewTeamCreator BindingNewTeamUser
  | -- | sso: users with saml credentials and/or created via scim
    NewTeamMemberSSO TeamId
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform NewTeamUser)

newTeamUserCode :: NewTeamUser -> Maybe InvitationCode
newTeamUserCode = \case
  NewTeamMember ic -> Just ic
  NewTeamCreator _ -> Nothing
  NewTeamMemberSSO _ -> Nothing

newTeamUserCreator :: NewTeamUser -> Maybe BindingNewTeamUser
newTeamUserCreator = \case
  NewTeamMember _ -> Nothing
  NewTeamCreator bntu -> Just bntu
  NewTeamMemberSSO _ -> Nothing

newTeamUserTeamId :: NewTeamUser -> Maybe TeamId
newTeamUserTeamId = \case
  NewTeamMember _ -> Nothing
  NewTeamCreator _ -> Nothing
  NewTeamMemberSSO tid -> Just tid

data BindingNewTeamUser = BindingNewTeamUser
  { bnuTeam :: BindingNewTeam,
    bnuCurrency :: Maybe Currency.Alpha
    -- FUTUREWORK:
    -- Remove Currency selection once billing supports currency changes after team creation
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform BindingNewTeamUser)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema BindingNewTeamUser)

instance ToSchema BindingNewTeamUser where
  schema =
    object "BindingNewTeamUser" $
      BindingNewTeamUser
        <$> bnuTeam
          .= bindingNewTeamObjectSchema
        <*> bnuCurrency
          .= maybe_ (optField "currency" genericToSchema)

--------------------------------------------------------------------------------
-- SCIM User Info

data ScimUserInfo = ScimUserInfo
  { suiUserId :: UserId,
    suiCreatedOn :: Maybe UTCTimeMillis
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ScimUserInfo)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema ScimUserInfo)

instance ToSchema ScimUserInfo where
  schema =
    object "ScimUserInfo" $
      ScimUserInfo
        <$> suiUserId
          .= field "id" schema
        <*> suiCreatedOn
          .= maybe_ (optField "created_on" schema)

newtype ScimUserInfos = ScimUserInfos {scimUserInfos :: [ScimUserInfo]}
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ScimUserInfos)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema ScimUserInfos)

instance ToSchema ScimUserInfos where
  schema =
    object "ScimUserInfos" $
      ScimUserInfos
        <$> scimUserInfos
          .= field "scim_user_infos" (array schema)

-------------------------------------------------------------------------------
-- UserSet

-- | Set of user ids, can be used for different purposes (e.g., used on the internal
-- APIs for listing user's clients)
newtype UserSet = UserSet
  { usUsrs :: Set UserId
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Arbitrary)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema UserSet)

instance ToSchema UserSet where
  schema =
    object "UserSet" $
      UserSet
        <$> usUsrs
          .= field "users" (set schema)

--------------------------------------------------------------------------------
-- Profile Updates

data UserUpdate = UserUpdate
  { uupName :: Maybe Name,
    -- | DEPRECATED
    uupPict :: Maybe Pict,
    uupAssets :: Maybe [Asset],
    uupAccentId :: Maybe ColourId
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema UserUpdate)
  deriving (Arbitrary) via (GenericUniform UserUpdate)

instance ToSchema UserUpdate where
  schema =
    object "UserUpdate" $
      UserUpdate
        <$> uupName
          .= maybe_ (optField "name" schema)
        <*> uupPict
          .= maybe_ (optField "picture" schema)
        <*> uupAssets
          .= maybe_ (optField "assets" (array schema))
        <*> uupAccentId
          .= maybe_ (optField "accent_id" schema)

data UpdateProfileError
  = DisplayNameManagedByScim
  | ProfileNotFound
  deriving (Generic)
  deriving (AsUnion PutSelfErrorResponses) via GenericAsUnion PutSelfErrorResponses UpdateProfileError

instance GSOP.Generic UpdateProfileError

type PutSelfErrorResponses = '[ErrorResponse 'E.NameManagedByScim, ErrorResponse 'E.UserNotFound]

type PutSelfResponses = PutSelfErrorResponses .++ '[RespondEmpty 200 "User updated"]

instance (res ~ PutSelfResponses) => AsUnion res (Maybe UpdateProfileError) where
  toUnion = maybeToUnion (toUnion @PutSelfErrorResponses)
  fromUnion = maybeFromUnion (fromUnion @PutSelfErrorResponses)

-- | The payload for setting or changing a password.
data PasswordChange = PasswordChange
  { cpOldPassword :: Maybe PlainTextPassword6,
    cpNewPassword :: PlainTextPassword8
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform PasswordChange)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema PasswordChange)

instance ToSchema PasswordChange where
  schema =
    over
      doc
      ( description
          ?~ "Data to change a password. The old password is required if \
             \a password already exists."
      )
      . object "PasswordChange"
      $ PasswordChange
        <$> cpOldPassword
          .= maybe_ (optField "old_password" schema)
        <*> cpNewPassword
          .= field "new_password" schema

data ChangePasswordError
  = InvalidCurrentPassword
  | ChangePasswordNoIdentity
  | ChangePasswordMustDiffer
  deriving (Generic)
  deriving (AsUnion ChangePasswordErrorResponses) via GenericAsUnion ChangePasswordErrorResponses ChangePasswordError

instance GSOP.Generic ChangePasswordError

type ChangePasswordErrorResponses =
  [ ErrorResponse 'E.BadCredentials,
    ErrorResponse 'E.NoIdentity,
    ErrorResponse 'E.ChangePasswordMustDiffer
  ]

type ChangePasswordResponses =
  ChangePasswordErrorResponses .++ '[RespondEmpty 200 "Password Changed"]

instance (res ~ ChangePasswordResponses) => AsUnion res (Maybe ChangePasswordError) where
  toUnion = maybeToUnion (toUnion @ChangePasswordErrorResponses)
  fromUnion = maybeFromUnion (fromUnion @ChangePasswordErrorResponses)

newtype LocaleUpdate = LocaleUpdate {luLocale :: Locale}
  deriving stock (Eq, Show, Generic)
  deriving newtype (Arbitrary)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema LocaleUpdate)

instance ToSchema LocaleUpdate where
  schema =
    object "LocaleUpdate" $
      LocaleUpdate
        <$> luLocale
          .= field "locale" schema

newtype EmailUpdate = EmailUpdate {euEmail :: Email}
  deriving stock (Eq, Show, Generic)
  deriving newtype (Arbitrary)
  deriving (S.ToSchema) via (Schema EmailUpdate)

instance ToSchema EmailUpdate where
  schema =
    object "EmailUpdate" $
      EmailUpdate
        <$> euEmail
          .= field "email" schema

instance ToJSON EmailUpdate where
  toJSON e = A.object ["email" A..= euEmail e]

instance FromJSON EmailUpdate where
  parseJSON = A.withObject "email-update" $ \o ->
    EmailUpdate <$> o A..: "email"

newtype PhoneUpdate = PhoneUpdate {puPhone :: Phone}
  deriving stock (Eq, Show, Generic)
  deriving newtype (Arbitrary)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema PhoneUpdate

instance ToSchema PhoneUpdate where
  schema =
    object "PhoneUpdate" $
      PhoneUpdate
        <$> puPhone
          .= field "phone" schema

data ChangePhoneError
  = PhoneExists
  | InvalidNewPhone
  | BlacklistedNewPhone
  deriving (Generic)
  deriving (AsUnion ChangePhoneErrorResponses) via GenericAsUnion ChangePhoneErrorResponses ChangePhoneError

instance GSOP.Generic ChangePhoneError

type ChangePhoneErrorResponses =
  [ ErrorResponse 'UserKeyExists,
    ErrorResponse 'InvalidPhone,
    ErrorResponse 'BlacklistedPhone
  ]

type ChangePhoneResponses =
  ChangePhoneErrorResponses .++ '[RespondEmpty 202 "Phone updated"]

instance (res ~ ChangePhoneResponses) => AsUnion res (Maybe ChangePhoneError) where
  toUnion = maybeToUnion (toUnion @ChangePhoneErrorResponses)
  fromUnion = maybeFromUnion (fromUnion @ChangePhoneErrorResponses)

data RemoveIdentityError
  = LastIdentity
  | NoPassword
  | NoIdentity
  deriving (Generic)
  deriving (AsUnion RemoveIdentityErrorResponses) via GenericAsUnion RemoveIdentityErrorResponses RemoveIdentityError

instance GSOP.Generic RemoveIdentityError

type RemoveIdentityErrorResponses =
  [ ErrorResponse 'E.LastIdentity,
    ErrorResponse 'E.NoPassword,
    ErrorResponse 'E.NoIdentity
  ]

type RemoveIdentityResponses =
  RemoveIdentityErrorResponses .++ '[RespondEmpty 200 "Identity Removed"]

instance (res ~ RemoveIdentityResponses) => AsUnion res (Maybe RemoveIdentityError) where
  toUnion = maybeToUnion (toUnion @RemoveIdentityErrorResponses)
  fromUnion = maybeFromUnion (fromUnion @RemoveIdentityErrorResponses)

newtype HandleUpdate = HandleUpdate {huHandle :: Text}
  deriving stock (Eq, Show, Generic)
  deriving newtype (Arbitrary)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema HandleUpdate)

instance ToSchema HandleUpdate where
  schema =
    object "HandleUpdate" $
      HandleUpdate <$> huHandle .= field "handle" schema

data ChangeHandleError
  = ChangeHandleNoIdentity
  | ChangeHandleExists
  | ChangeHandleInvalid
  | ChangeHandleManagedByScim
  deriving (Show, Generic)
  deriving (AsUnion ChangeHandleErrorResponses) via GenericAsUnion ChangeHandleErrorResponses ChangeHandleError

instance GSOP.Generic ChangeHandleError

type ChangeHandleErrorResponses =
  '[ ErrorResponse 'E.NoIdentity,
     ErrorResponse 'E.HandleExists,
     ErrorResponse 'E.InvalidHandle,
     ErrorResponse 'E.HandleManagedByScim
   ]

type ChangeHandleResponses =
  ChangeHandleErrorResponses .++ '[RespondEmpty 200 "Handle Changed"]

instance (res ~ ChangeHandleResponses) => AsUnion res (Maybe ChangeHandleError) where
  toUnion = maybeToUnion (toUnion @ChangeHandleErrorResponses)
  fromUnion = maybeFromUnion (fromUnion @ChangeHandleErrorResponses)

newtype NameUpdate = NameUpdate {nuHandle :: Text}
  deriving stock (Eq, Show, Generic)
  deriving newtype (Arbitrary)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema NameUpdate)

instance ToSchema NameUpdate where
  schema =
    object "NameUpdate" $
      NameUpdate <$> nuHandle .= field "name" schema

data ChangeEmailResponse
  = ChangeEmailResponseIdempotent
  | ChangeEmailResponseNeedsActivation

instance
  AsUnion
    '[ Respond 202 "Update accepted and pending activation of the new email" (),
       Respond 204 "No update, current and new email address are the same" ()
     ]
    ChangeEmailResponse
  where
  toUnion ChangeEmailResponseNeedsActivation = Z (I ())
  toUnion ChangeEmailResponseIdempotent = S (Z (I ()))
  fromUnion (Z (I ())) = ChangeEmailResponseNeedsActivation
  fromUnion (S (Z (I ()))) = ChangeEmailResponseIdempotent
  fromUnion (S (S x)) = case x of {}

-----------------------------------------------------------------------------
-- Account Deletion

-- | Payload for requesting account deletion.
newtype DeleteUser = DeleteUser
  { deleteUserPassword :: Maybe PlainTextPassword6
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Arbitrary)
  deriving (S.ToSchema) via (Schema DeleteUser)

instance ToSchema DeleteUser where
  schema =
    object "DeleteUser" $
      DeleteUser
        <$> deleteUserPassword
          .= maybe_ (optField "password" schema)

mkDeleteUser :: Maybe PlainTextPassword6 -> DeleteUser
mkDeleteUser = DeleteUser

instance ToJSON DeleteUser where
  toJSON d =
    A.object $
      "password"
        A..= deleteUserPassword d
        # []

instance FromJSON DeleteUser where
  parseJSON = A.withObject "DeleteUser" $ \o ->
    DeleteUser <$> o A..:? "password"

-- | Payload for verifying account deletion via a code.
data VerifyDeleteUser = VerifyDeleteUser
  { verifyDeleteUserKey :: Code.Key,
    verifyDeleteUserCode :: Code.Value
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform VerifyDeleteUser)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema VerifyDeleteUser)

mkVerifyDeleteUser :: Code.Key -> Code.Value -> VerifyDeleteUser
mkVerifyDeleteUser = VerifyDeleteUser

instance ToSchema VerifyDeleteUser where
  schema =
    objectWithDocModifier "VerifyDeleteUser" (description ?~ "Data for verifying an account deletion.") $
      VerifyDeleteUser
        <$> verifyDeleteUserKey
          .= fieldWithDocModifier "key" (description ?~ "The identifying key of the account (i.e. user ID).") schema
        <*> verifyDeleteUserCode
          .= fieldWithDocModifier "code" (description ?~ "The verification code.") schema

-- | A response for a pending deletion code.
newtype DeletionCodeTimeout = DeletionCodeTimeout
  {fromDeletionCodeTimeout :: Code.Timeout}
  deriving stock (Eq, Show, Generic)
  deriving newtype (Arbitrary)
  deriving (S.ToSchema) via (Schema DeletionCodeTimeout)

instance ToSchema DeletionCodeTimeout where
  schema =
    object "DeletionCodeTimeout" $
      DeletionCodeTimeout
        <$> fromDeletionCodeTimeout
          .= field "expires_in" schema

instance ToJSON DeletionCodeTimeout where
  toJSON (DeletionCodeTimeout t) = A.object ["expires_in" A..= t]

instance FromJSON DeletionCodeTimeout where
  parseJSON = A.withObject "DeletionCodeTimeout" $ \o ->
    DeletionCodeTimeout <$> o A..: "expires_in"

-- | Like `DeleteUserResult`, but without the exception case.
data DeleteUserResponse
  = UserResponseAccountAlreadyDeleted
  | UserResponseAccountDeleted

instance
  AsUnion
    '[ Respond 200 "UserResponseAccountAlreadyDeleted" (),
       Respond 202 "UserResponseAccountDeleted" ()
     ]
    DeleteUserResponse
  where
  toUnion UserResponseAccountAlreadyDeleted = Z (I ())
  toUnion UserResponseAccountDeleted = S (Z (I ()))
  fromUnion (Z (I ())) = UserResponseAccountAlreadyDeleted
  fromUnion (S (Z (I ()))) = UserResponseAccountDeleted
  fromUnion (S (S x)) = case x of {}

-- | Result of an internal user/account deletion
data DeleteUserResult
  = -- | User never existed
    NoUser
  | -- | User/account was deleted before
    AccountAlreadyDeleted
  | -- | User/account was deleted in this call
    AccountDeleted
  deriving (Eq, Show)

data ListUsersQuery
  = ListUsersByIds [Qualified UserId]
  | ListUsersByHandles (Range 1 4 [Qualified Handle])
  deriving (Show, Eq)

instance FromJSON ListUsersQuery where
  parseJSON =
    A.withObject "ListUsersQuery" $ \o -> do
      mUids <- ListUsersByIds <$$> o A..:? "qualified_ids"
      mHandles <- ListUsersByHandles <$$> o A..:? "qualified_handles"
      case (mUids, mHandles) of
        (Just uids, Nothing) -> pure uids
        (Nothing, Just handles) -> pure handles
        (_, _) -> fail "exactly one of qualified_ids or qualified_handles must be provided."

instance ToJSON ListUsersQuery where
  toJSON (ListUsersByIds uids) = A.object ["qualified_ids" A..= uids]
  toJSON (ListUsersByHandles handles) = A.object ["qualified_handles" A..= handles]

-- NB: It is not possible to specific mutually exclusive fields in swagger2, so
-- here we write it in description and modify the example to have the correct
-- JSON.
instance S.ToSchema ListUsersQuery where
  declareNamedSchema _ = do
    uids <- S.declareSchemaRef (Proxy @[Qualified UserId])
    handles <- S.declareSchemaRef (Proxy @(Range 1 4 [Qualified Handle]))
    pure $
      S.NamedSchema (Just "ListUsersQuery") $
        mempty
          & S.type_ ?~ S.OpenApiObject
          & S.description ?~ "exactly one of qualified_ids or qualified_handles must be provided."
          & S.properties .~ InsOrdHashMap.fromList [("qualified_ids", uids), ("qualified_handles", handles)]
          & S.example ?~ toJSON (ListUsersByIds [Qualified (Id UUID.nil) (Domain "example.com")])

-------------------------------------------------------------------------------
-- AccountStatus

data AccountStatus
  = Active
  | Suspended
  | Deleted
  | Ephemeral
  | -- | for most intents & purposes, this is another form of inactive.  it is used for
    -- allowing scim to find users that have not accepted their invitation yet after
    -- creating via scim.
    PendingInvitation
  deriving (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via (GenericUniform AccountStatus)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema.Schema AccountStatus

instance Schema.ToSchema AccountStatus where
  schema =
    Schema.enum @Text "AccountStatus" $
      mconcat
        [ Schema.element "active" Active,
          Schema.element "suspended" Suspended,
          Schema.element "deleted" Deleted,
          Schema.element "ephemeral" Ephemeral,
          Schema.element "pending-invitation" PendingInvitation
        ]

instance C.Cql AccountStatus where
  ctype = C.Tagged C.IntColumn

  toCql Active = C.CqlInt 0
  toCql Suspended = C.CqlInt 1
  toCql Deleted = C.CqlInt 2
  toCql Ephemeral = C.CqlInt 3
  toCql PendingInvitation = C.CqlInt 4

  fromCql (C.CqlInt i) = case i of
    0 -> pure Active
    1 -> pure Suspended
    2 -> pure Deleted
    3 -> pure Ephemeral
    4 -> pure PendingInvitation
    n -> Left $ "unexpected account status: " ++ show n
  fromCql _ = Left "account status: int expected"

data AccountStatusResp = AccountStatusResp {fromAccountStatusResp :: AccountStatus}
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform AccountStatusResp)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema.Schema AccountStatusResp

instance Schema.ToSchema AccountStatusResp where
  schema =
    object "AccountStatusResp" $
      AccountStatusResp <$> fromAccountStatusResp .= field "status" schema

newtype AccountStatusUpdate = AccountStatusUpdate {suStatus :: AccountStatus}
  deriving (Generic)
  deriving (Arbitrary) via (GenericUniform AccountStatusUpdate)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema.Schema AccountStatusUpdate

instance Schema.ToSchema AccountStatusUpdate where
  schema =
    object "AccountStatusUpdate" $
      AccountStatusUpdate <$> suStatus .= field "status" schema

-------------------------------------------------------------------------------
-- UserAccount

-- | A UserAccount is targeted to be used by our \"backoffice\" and represents
-- all the data related to a user in our system, regardless of whether they
-- are active or not, their status, etc.
data UserAccount = UserAccount
  { accountUser :: !User,
    accountStatus :: !AccountStatus
  }
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform UserAccount)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema.Schema UserAccount

instance Schema.ToSchema UserAccount where
  schema =
    Schema.object "UserAccount" $
      UserAccount
        <$> accountUser Schema..= userObjectSchema
        <*> accountStatus Schema..= Schema.field "status" Schema.schema

-------------------------------------------------------------------------------
-- NewUserScimInvitation

data NewUserScimInvitation = NewUserScimInvitation
  -- FIXME: the TID should be captured in the route as usual
  { newUserScimInvTeamId :: TeamId,
    newUserScimInvUserId :: UserId,
    newUserScimInvLocale :: Maybe Locale,
    newUserScimInvName :: Name,
    newUserScimInvEmail :: Email,
    newUserScimInvRole :: Role
  }
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform NewUserScimInvitation)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema.Schema NewUserScimInvitation

instance Schema.ToSchema NewUserScimInvitation where
  schema =
    Schema.object "NewUserScimInvitation" $
      NewUserScimInvitation
        <$> newUserScimInvTeamId Schema..= Schema.field "team_id" Schema.schema
        <*> newUserScimInvUserId Schema..= Schema.field "user_id" Schema.schema
        <*> newUserScimInvLocale Schema..= maybe_ (optField "locale" Schema.schema)
        <*> newUserScimInvName Schema..= Schema.field "name" Schema.schema
        <*> newUserScimInvEmail Schema..= Schema.field "email" Schema.schema
        <*> newUserScimInvRole Schema..= Schema.field "role" Schema.schema

-----------------------------------------------------------------------------
-- SndFactorPasswordChallenge

data VerificationAction
  = CreateScimToken
  | Login
  | DeleteTeam
  deriving stock (Eq, Show, Enum, Bounded, Generic)
  deriving (Arbitrary) via (GenericUniform VerificationAction)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema VerificationAction)

instance ToSchema VerificationAction where
  schema =
    enum @Text "VerificationAction" $
      mconcat
        [ element "create_scim_token" CreateScimToken,
          element "login" Login,
          element "delete_team" DeleteTeam
        ]

instance ToByteString VerificationAction where
  builder CreateScimToken = "create_scim_token"
  builder Login = "login"
  builder DeleteTeam = "delete_team"

instance FromByteString VerificationAction where
  parser =
    Parser.takeByteString >>= \b ->
      case T.decodeUtf8' b of
        Right "login" -> pure Login
        Right "create_scim_token" -> pure CreateScimToken
        Right "delete_team" -> pure DeleteTeam
        Right t -> fail $ "Invalid VerificationAction: " <> T.unpack t
        Left e -> fail $ "Invalid VerificationAction: " <> show e

instance S.ToParamSchema VerificationAction where
  toParamSchema _ =
    mempty
      { S._schemaType = Just S.OpenApiString,
        S._schemaEnum = Just (A.String . toQueryParam <$> [(minBound :: VerificationAction) ..])
      }

instance FromHttpApiData VerificationAction where
  parseUrlPiece =
    maybeToEither "Invalid verification action"
      . fromByteString
      . T.encodeUtf8

instance ToHttpApiData VerificationAction where
  toQueryParam a = T.decodeUtf8With lenientDecode (toByteString' a)

data SendVerificationCode = SendVerificationCode
  { svcAction :: VerificationAction,
    svcEmail :: Email
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform SendVerificationCode)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema SendVerificationCode

instance ToSchema SendVerificationCode where
  schema =
    object "SendVerificationCode" $
      SendVerificationCode
        <$> svcAction
          .= field "action" schema
        <*> svcEmail
          .= field "email" schema

--------------------------------------------------------------------------------
-- Protocol preferences

-- | High-level protocols supported by clients.
--
-- Unlike 'ProtocolTag', this does not include any transitional protocols used
-- for migration.
data BaseProtocolTag = BaseProtocolProteusTag | BaseProtocolMLSTag
  deriving stock (Eq, Ord, Enum, Bounded, Show, Generic)
  deriving (Arbitrary) via (GenericUniform BaseProtocolTag)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema BaseProtocolTag)

instance C.Cql (Imports.Set BaseProtocolTag) where
  ctype = C.Tagged C.IntColumn

  toCql = C.CqlInt . fromIntegral . protocolSetBits
  fromCql (C.CqlInt bits) = pure $ protocolSetFromBits (fromIntegral bits)
  fromCql _ = Left "Protocol set: Int expected"

baseProtocolMask :: BaseProtocolTag -> Word32
baseProtocolMask BaseProtocolProteusTag = 1
baseProtocolMask BaseProtocolMLSTag = 2

baseProtocolToProtocol :: BaseProtocolTag -> ProtocolTag
baseProtocolToProtocol BaseProtocolProteusTag = ProtocolProteusTag
baseProtocolToProtocol BaseProtocolMLSTag = ProtocolMLSTag

instance ToSchema BaseProtocolTag where
  schema =
    enum @Text "BaseProtocol" $
      mconcat
        [ element "proteus" BaseProtocolProteusTag,
          element "mls" BaseProtocolMLSTag
        ]

defSupportedProtocols :: Set BaseProtocolTag
defSupportedProtocols = Set.singleton BaseProtocolProteusTag

supportedProtocolsObjectSchema :: ObjectSchema SwaggerDoc (Set BaseProtocolTag)
supportedProtocolsObjectSchema =
  fmap
    (fromMaybe defSupportedProtocols)
    (optField "supported_protocols" (set schema))

protocolSetBits :: Set BaseProtocolTag -> Word32
protocolSetBits = foldr (\p x -> baseProtocolMask p .|. x) 0

protocolSetFromBits :: Word32 -> Set BaseProtocolTag
protocolSetFromBits w =
  foldr
    (\x -> if w .&. baseProtocolMask x /= 0 then Set.insert x else id)
    mempty
    [BaseProtocolProteusTag, BaseProtocolMLSTag]

newtype SupportedProtocolUpdate = SupportedProtocolUpdate
  {unSupportedProtocolUpdate :: Set BaseProtocolTag}
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema SupportedProtocolUpdate)

instance ToSchema SupportedProtocolUpdate where
  schema =
    object "SupportedProtocolUpdate" $
      SupportedProtocolUpdate
        <$> unSupportedProtocolUpdate
          .= field "supported_protocols" (set schema)

------- Partial Successes
data ListUsersById = ListUsersById
  { listUsersByIdFound :: [UserProfile],
    listUsersByIdFailed :: Maybe (NonEmpty (Qualified UserId))
  }
  deriving (Eq, Show)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema ListUsersById

instance ToSchema ListUsersById where
  schema =
    object "ListUsersById" $
      ListUsersById
        <$> listUsersByIdFound .= field "found" (array schema)
        <*> listUsersByIdFailed .= maybe_ (optField "failed" $ nonEmptyArray schema)
