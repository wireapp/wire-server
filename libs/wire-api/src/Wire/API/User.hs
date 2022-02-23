{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
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

module Wire.API.User
  ( UserIdList (..),
    QualifiedUserIdList (..),
    LimitedQualifiedUserIdList (..),
    -- Profiles
    UserProfile (..),
    SelfProfile (..),
    -- User (should not be here)
    User (..),
    userEmail,
    userPhone,
    userSSOId,
    userSCIMExternalId,
    connectedProfile,
    publicProfile,

    -- * NewUser
    NewUserPublic (..),
    NewUser (..),
    emptyNewUser,
    ExpiresIn,
    newUserInvitationCode,
    newUserTeam,
    newUserEmail,
    newUserPhone,
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

    -- * Account Deletion
    DeleteUser (..),
    mkDeleteUser,
    VerifyDeleteUser (..),
    mkVerifyDeleteUser,
    DeletionCodeTimeout (..),

    -- * List Users
    ListUsersQuery (..),

    -- * helpers
    parseIdentity,

    -- * re-exports
    module Wire.API.User.Identity,
    module Wire.API.User.Profile,

    -- * Swagger
    modelDelete,
    modelEmailUpdate,
    modelNewUser,
    modelUser,
    modelUserIdList,
    modelVerifyDelete,

    -- * 2nd factor auth
    SndFactorPasswordChallengeAction (..),
    SendVerificationCode (..),
    TeamFeatureSndFPasswordChallengeNotImplemented (..),
  )
where

import Control.Applicative
import Control.Error.Safe (rightMay)
import Control.Lens (over, view, (.~), (?~))
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as A
import Data.ByteString.Conversion
import qualified Data.CaseInsensitive as CI
import qualified Data.Code as Code
import qualified Data.Currency as Currency
import Data.Domain (Domain (Domain))
import Data.Handle (Handle)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.Id
import Data.Json.Util (UTCTimeMillis, (#))
import Data.LegalHold (UserLegalHoldStatus)
import qualified Data.List as List
import Data.Misc (PlainTextPassword (..))
import Data.Qualified
import Data.Range
import Data.SOP
import Data.Schema
import qualified Data.Swagger as S
import qualified Data.Swagger.Build.Api as Doc
import Data.Text.Ascii
import Data.UUID (UUID, nil)
import qualified Data.UUID as UUID
import Deriving.Swagger
import GHC.TypeLits (KnownNat, Nat)
import qualified Generics.SOP as GSOP
import Imports
import qualified SAML2.WebSSO as SAML
import Servant (type (.++))
import qualified Test.QuickCheck as QC
import Wire.API.Arbitrary (Arbitrary (arbitrary), GenericUniform (..))
import Wire.API.ErrorDescription
import Wire.API.Provider.Service (ServiceRef, modelServiceRef)
import Wire.API.Routes.MultiVerb
import Wire.API.Team (BindingNewTeam (BindingNewTeam), NewTeam (..), modelNewBindingTeam)
import Wire.API.User.Activation (ActivationCode)
import Wire.API.User.Auth (CookieLabel)
import Wire.API.User.Identity
import Wire.API.User.Profile

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
        <$> mUsers .= field "user_ids" (array schema)

modelUserIdList :: Doc.Model
modelUserIdList = Doc.defineModel "UserIdList" $ do
  Doc.description "list of user ids"
  Doc.property "user_ids" (Doc.unique $ Doc.array Doc.bytes') $
    Doc.description "the array of team conversations"

--------------------------------------------------------------------------------
-- QualifiedUserIdList

newtype QualifiedUserIdList = QualifiedUserIdList {qualifiedUserIdList :: [Qualified UserId]}
  deriving stock (Eq, Show, Generic)
  deriving newtype (Arbitrary)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema QualifiedUserIdList

instance ToSchema QualifiedUserIdList where
  schema =
    object "QualifiedUserIdList" $
      QualifiedUserIdList
        <$> qualifiedUserIdList .= field "qualified_user_ids" (array schema)
        <* (fmap qUnqualified . qualifiedUserIdList) .= field "user_ids" (deprecatedSchema "qualified_user_ids" (array schema))

--------------------------------------------------------------------------------
-- LimitedQualifiedUserIdList

-- | We cannot use 'Wrapped' here because all the instances require proof that 1
-- is less than or equal to 'max'.
newtype LimitedQualifiedUserIdList (max :: Nat) = LimitedQualifiedUserIdList
  {qualifiedUsers :: Range 1 max [Qualified UserId]}
  deriving stock (Eq, Show, Generic)
  deriving (S.ToSchema) via CustomSwagger '[FieldLabelModifier CamelToSnake] (LimitedQualifiedUserIdList max)

instance (KnownNat max, LTE 1 max) => Arbitrary (LimitedQualifiedUserIdList max) where
  arbitrary = LimitedQualifiedUserIdList <$> arbitrary

instance LTE 1 max => FromJSON (LimitedQualifiedUserIdList max) where
  parseJSON = A.withObject "LimitedQualifiedUserIdList" $ \o ->
    LimitedQualifiedUserIdList <$> o A..: "qualified_users"

instance LTE 1 max => ToJSON (LimitedQualifiedUserIdList max) where
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
    profileLegalholdStatus :: UserLegalHoldStatus
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform UserProfile)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema UserProfile)

instance ToSchema UserProfile where
  schema =
    object "UserProfile" $
      UserProfile
        <$> profileQualifiedId .= field "qualified_id" schema
        <* (qUnqualified . profileQualifiedId)
          .= optional (field "id" (deprecatedSchema "qualified_id" schema))
        <*> profileName .= field "name" schema
        <*> profilePict .= (field "picture" schema <|> pure noPict)
        <*> profileAssets .= (field "assets" (array schema) <|> pure [])
        <*> profileAccentId .= field "accent_id" schema
        <*> ((\del -> if del then Just True else Nothing) . profileDeleted)
          .= maybe_ (fromMaybe False <$> optField "deleted" schema)
        <*> profileService .= maybe_ (optField "service" schema)
        <*> profileHandle .= maybe_ (optField "handle" schema)
        <*> profileExpire .= maybe_ (optField "expires_at" schema)
        <*> profileTeam .= maybe_ (optField "team" schema)
        <*> profileEmail .= maybe_ (optField "email" schema)
        <*> profileLegalholdStatus .= field "legalhold_status" schema

modelUser :: Doc.Model
modelUser = Doc.defineModel "User" $ do
  Doc.description "User Profile"
  Doc.property "id" Doc.bytes' $
    Doc.description "User ID"
  Doc.property "name" Doc.string' $
    Doc.description "Name"
  Doc.property "email" Doc.string' $ do
    Doc.description "Email"
    Doc.optional
  Doc.property "assets" (Doc.array (Doc.ref modelAsset)) $
    Doc.description "Profile assets"
  Doc.property "accent_id" Doc.int32' $ do
    Doc.description "Accent colour ID"
    Doc.optional
  Doc.property "deleted" Doc.bool' $ do
    Doc.description "Whether the account has been deleted."
    Doc.optional
  Doc.property "service" (Doc.ref modelServiceRef) $ do
    Doc.description "The reference to the owning service, if the user is a 'bot'."
    Doc.optional
  Doc.property "handle" Doc.string' $ do
    Doc.description "Unique user handle."
    Doc.optional
  Doc.property "team" Doc.string' $ do
    Doc.description "Team ID"
    Doc.optional

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
  { userId :: UserId,
    userQualifiedId :: Qualified UserId,
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
    userManagedBy :: ManagedBy
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform User)

-- Cannot use deriving (ToSchema) via (CustomSwagger ...) because we need to
-- mark 'deleted' as optional, but it is not a 'Maybe'
-- and we need to manually add the identity schema fields at the top level
-- instead of nesting them under the 'identity' field.
instance S.ToSchema User where
  declareNamedSchema _ = do
    identityProperties <- view (S.schema . S.properties) <$> S.declareNamedSchema (Proxy @UserIdentity)
    genericSchema <-
      S.genericDeclareNamedSchema
        ( swaggerOptions
            @'[ FieldLabelModifier
                  ( StripPrefix "user",
                    CamelToSnake,
                    LabelMappings
                      '[ "pict" ':-> "picture",
                         "expire" ':-> "expires_at",
                         "display_name" ':-> "name"
                       ]
                  )
              ]
        )
        (Proxy @User)
    pure $
      genericSchema
        & over (S.schema . S.required) (List.delete "deleted")
        -- The UserIdentity fields need to be flat-included, not be in a sub-object
        & over (S.schema . S.properties) (InsOrdHashMap.delete "identity")
        & over (S.schema . S.properties) (InsOrdHashMap.union identityProperties)

-- FUTUREWORK:
-- disentangle json serializations for 'User', 'NewUser', 'UserIdentity', 'NewUserOrigin'.
instance ToJSON User where
  toJSON u =
    A.object $
      "id" A..= userId u
        # "qualified_id" A..= userQualifiedId u
        # "name" A..= userDisplayName u
        # "picture" A..= userPict u
        # "assets" A..= userAssets u
        # "email" A..= userEmail u
        # "phone" A..= userPhone u
        # "accent_id" A..= userAccentId u
        # "deleted" A..= (if userDeleted u then Just True else Nothing)
        # "locale" A..= userLocale u
        # "service" A..= userService u
        # "handle" A..= userHandle u
        # "expires_at" A..= userExpire u
        # "team" A..= userTeam u
        # "sso_id" A..= userSSOId u
        # "managed_by" A..= userManagedBy u
        # []

instance FromJSON User where
  parseJSON = A.withObject "user" $ \o -> do
    ssoid <- o A..:? "sso_id"
    User
      <$> o A..: "id"
      <*> o A..: "qualified_id"
      <*> parseIdentity ssoid o
      <*> o A..: "name"
      <*> o A..:? "picture" A..!= noPict
      <*> o A..:? "assets" A..!= []
      <*> o A..: "accent_id"
      <*> o A..:? "deleted" A..!= False
      <*> o A..: "locale"
      <*> o A..:? "service"
      <*> o A..:? "handle"
      <*> o A..:? "expires_at"
      <*> o A..:? "team"
      <*> o A..:? "managed_by" A..!= ManagedByWire

userEmail :: User -> Maybe Email
userEmail = emailIdentity <=< userIdentity

userPhone :: User -> Maybe Phone
userPhone = phoneIdentity <=< userIdentity

userSSOId :: User -> Maybe UserSSOId
userSSOId = ssoIdentity <=< userIdentity

userSCIMExternalId :: User -> Maybe Text
userSCIMExternalId usr = userSSOId >=> ssoIdExtId $ usr
  where
    ssoIdExtId :: UserSSOId -> Maybe Text
    ssoIdExtId (UserSSOId (SAML.UserRef _ nameIdXML)) = case userManagedBy usr of
      ManagedByWire -> Nothing
      ManagedByScim ->
        -- FUTUREWORK: this is only ignoring case in the email format, and emails should be
        -- handled case-insensitively.  https://wearezeta.atlassian.net/browse/SQSERVICES-909
        Just . CI.original . SAML.unsafeShowNameID $ nameIdXML
    ssoIdExtId (UserScimExternalId extId) = pure extId

connectedProfile :: User -> UserLegalHoldStatus -> UserProfile
connectedProfile u legalHoldStatus =
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
      -- We don't want to show the email by default;
      -- However we do allow adding it back in intentionally later.
      profileEmail = Nothing,
      profileLegalholdStatus = legalHoldStatus
    }

-- FUTUREWORK: should public and conect profile be separate types?
publicProfile :: User -> UserLegalHoldStatus -> UserProfile
publicProfile u legalHoldStatus =
  -- Note that we explicitly unpack and repack the types here rather than using
  -- RecordWildCards or something similar because we want changes to the public profile
  -- to be EXPLICIT and INTENTIONAL so we don't accidentally leak sensitive data.
  let UserProfile
        { profileQualifiedId,
          profileHandle,
          profileName,
          profilePict,
          profileAssets,
          profileAccentId,
          profileService,
          profileDeleted,
          profileExpire,
          profileTeam,
          profileLegalholdStatus
        } = connectedProfile u legalHoldStatus
   in UserProfile
        { profileEmail = Nothing,
          profileQualifiedId,
          profileHandle,
          profileName,
          profilePict,
          profileAssets,
          profileAccentId,
          profileService,
          profileDeleted,
          profileExpire,
          profileTeam,
          profileLegalholdStatus
        }

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
  deriving newtype (ToJSON)

modelNewUser :: Doc.Model
modelNewUser = Doc.defineModel "NewUser" $ do
  Doc.description "New User Data"
  Doc.property "name" Doc.string' $
    Doc.description "Name (1 - 128 characters)"
  Doc.property "email" Doc.string' $ do
    Doc.description "Email address"
    Doc.optional
  Doc.property "password" Doc.string' $ do
    Doc.description "Password (6 - 1024 characters)"
    Doc.optional
  Doc.property "assets" (Doc.array (Doc.ref modelAsset)) $ do
    Doc.description "Profile assets"
    Doc.optional
  Doc.property "phone" Doc.string' $ do
    Doc.description "E.164 phone number"
    Doc.optional
  Doc.property "accent_id" Doc.int32' $ do
    Doc.description "Accent colour ID"
    Doc.optional
  Doc.property "email_code" Doc.bytes' $ do
    Doc.description "Email activation code"
    Doc.optional
  Doc.property "phone_code" Doc.bytes' $ do
    Doc.description "Phone activation code"
    Doc.optional
  Doc.property "invitation_code" Doc.bytes' $ do
    Doc.description "Invitation code. Mutually exclusive with team|team_code"
    Doc.optional
  Doc.property "locale" Doc.string' $ do
    Doc.description "Locale in <ln-cc> format."
    Doc.optional
  Doc.property "label" Doc.string' $ do
    Doc.description
      "An optional label to associate with the access cookie, \
      \if one is granted during account creation."
    Doc.optional
  Doc.property "team_code" Doc.string' $ do
    Doc.description "Team invitation code. Mutually exclusive with team|invitation_code"
    Doc.optional
  Doc.property "team" (Doc.ref modelNewBindingTeam) $ do
    Doc.description "New team information. Mutually exclusive with team_code|invitation_code"
    Doc.optional

instance FromJSON NewUserPublic where
  parseJSON val = do
    nu <- parseJSON val
    either fail pure $ validateNewUserPublic nu

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

data NewUser = NewUser
  { newUserDisplayName :: Name,
    -- | use this as 'UserId' (if 'Nothing', call 'Data.UUID.nextRandom').
    newUserUUID :: Maybe UUID,
    newUserIdentity :: Maybe UserIdentity,
    -- | DEPRECATED
    newUserPict :: Maybe Pict,
    newUserAssets :: [Asset],
    newUserAccentId :: Maybe ColourId,
    newUserEmailCode :: Maybe ActivationCode,
    newUserPhoneCode :: Maybe ActivationCode,
    newUserOrigin :: Maybe NewUserOrigin,
    newUserLabel :: Maybe CookieLabel,
    newUserLocale :: Maybe Locale,
    newUserPassword :: Maybe PlainTextPassword,
    newUserExpiresIn :: Maybe ExpiresIn,
    newUserManagedBy :: Maybe ManagedBy
  }
  deriving stock (Eq, Show, Generic)

emptyNewUser :: Name -> NewUser
emptyNewUser name =
  NewUser
    { newUserDisplayName = name,
      newUserUUID = Nothing,
      newUserIdentity = Nothing,
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
      newUserManagedBy = Nothing
    }

-- | 1 second - 1 week
type ExpiresIn = Range 1 604800 Integer

instance ToJSON NewUser where
  toJSON u =
    A.object $
      "name" A..= newUserDisplayName u
        # "uuid" A..= newUserUUID u
        # "email" A..= newUserEmail u
        # "email_code" A..= newUserEmailCode u
        # "picture" A..= newUserPict u
        # "assets" A..= newUserAssets u
        # "phone" A..= newUserPhone u
        # "phone_code" A..= newUserPhoneCode u
        # "accent_id" A..= newUserAccentId u
        # "label" A..= newUserLabel u
        # "locale" A..= newUserLocale u
        # "password" A..= newUserPassword u
        # "expires_in" A..= newUserExpiresIn u
        # "sso_id" A..= newUserSSOId u
        # "managed_by" A..= newUserManagedBy u
        # maybe [] jsonNewUserOrigin (newUserOrigin u)

instance FromJSON NewUser where
  parseJSON = A.withObject "new-user" $ \o -> do
    ssoid <- o A..:? "sso_id"
    newUserDisplayName <- o A..: "name"
    newUserUUID <- o A..:? "uuid"
    newUserIdentity <- parseIdentity ssoid o
    newUserPict <- o A..:? "picture"
    newUserAssets <- o A..:? "assets" A..!= []
    newUserAccentId <- o A..:? "accent_id"
    newUserEmailCode <- o A..:? "email_code"
    newUserPhoneCode <- o A..:? "phone_code"
    newUserLabel <- o A..:? "label"
    newUserLocale <- o A..:? "locale"
    newUserPassword <- o A..:? "password"
    newUserOrigin <- parseNewUserOrigin newUserPassword newUserIdentity ssoid o
    newUserExpires <- o A..:? "expires_in"
    newUserExpiresIn <- case (newUserExpires, newUserIdentity) of
      (Just _, Just _) -> fail "Only users without an identity can expire"
      _ -> return newUserExpires
    newUserManagedBy <- o A..:? "managed_by"
    return NewUser {..}

-- FUTUREWORK: align more with FromJSON instance?
instance Arbitrary NewUser where
  arbitrary = do
    newUserIdentity <- arbitrary
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

newUserPhone :: NewUser -> Maybe Phone
newUserPhone = phoneIdentity <=< newUserIdentity

newUserSSOId :: NewUser -> Maybe UserSSOId
newUserSSOId = ssoIdentity <=< newUserIdentity

--------------------------------------------------------------------------------
-- NewUserOrigin

data NewUserOrigin
  = NewUserOriginInvitationCode InvitationCode
  | NewUserOriginTeamUser NewTeamUser
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform NewUserOrigin)

jsonNewUserOrigin :: NewUserOrigin -> [A.Pair]
jsonNewUserOrigin = \case
  NewUserOriginInvitationCode inv -> ["invitation_code" A..= inv]
  NewUserOriginTeamUser (NewTeamMember tc) -> ["team_code" A..= tc]
  NewUserOriginTeamUser (NewTeamCreator team) -> ["team" A..= team]
  NewUserOriginTeamUser (NewTeamMemberSSO ti) -> ["team_id" A..= ti]

parseNewUserOrigin ::
  Maybe PlainTextPassword ->
  Maybe UserIdentity ->
  Maybe UserSSOId ->
  A.Object ->
  A.Parser (Maybe NewUserOrigin)
parseNewUserOrigin pass uid ssoid o = do
  invcode <- o A..:? "invitation_code"
  teamcode <- o A..:? "team_code"
  team <- o A..:? "team"
  teamid <- o A..:? "team_id"
  result <- case (invcode, teamcode, team, ssoid, teamid) of
    (Just a, Nothing, Nothing, Nothing, Nothing) -> return . Just . NewUserOriginInvitationCode $ a
    (Nothing, Just a, Nothing, Nothing, Nothing) -> return . Just . NewUserOriginTeamUser $ NewTeamMember a
    (Nothing, Nothing, Just a, Nothing, Nothing) -> return . Just . NewUserOriginTeamUser $ NewTeamCreator a
    (Nothing, Nothing, Nothing, Just _, Just t) -> return . Just . NewUserOriginTeamUser $ NewTeamMemberSSO t
    (Nothing, Nothing, Nothing, Nothing, Nothing) -> return Nothing
    (_, _, _, Just _, Nothing) -> fail "sso_id, team_id must be either both present or both absent."
    (_, _, _, Nothing, Just _) -> fail "sso_id, team_id must be either both present or both absent."
    _ -> fail "team_code, team, invitation_code, sso_id, and the pair (sso_id, team_id) are mutually exclusive"
  case (result, pass, uid) of
    (_, _, Just SSOIdentity {}) -> pure result
    (Just (NewUserOriginTeamUser _), Nothing, _) -> fail "all team users must set a password on creation"
    _ -> pure result

-- | A random invitation code for use during registration
newtype InvitationCode = InvitationCode
  {fromInvitationCode :: AsciiBase64Url}
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON, ToByteString, FromByteString, Arbitrary)

--------------------------------------------------------------------------------
-- helpers

-- | Fails if email or phone or ssoid are present but invalid.
-- If neither are present, it will not fail, but return Nothing.
--
-- FUTUREWORK: Why is the SSO ID passed separately?
parseIdentity :: Maybe UserSSOId -> A.Object -> A.Parser (Maybe UserIdentity)
parseIdentity ssoid o =
  if isJust (KeyMap.lookup "email" o <|> KeyMap.lookup "phone" o) || isJust ssoid
    then Just <$> parseJSON (A.Object o)
    else pure Nothing

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

data BindingNewTeamUser = BindingNewTeamUser
  { bnuTeam :: BindingNewTeam,
    bnuCurrency :: Maybe Currency.Alpha
    -- FUTUREWORK:
    -- Remove Currency selection once billing supports currency changes after team creation
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform BindingNewTeamUser)

instance ToJSON BindingNewTeamUser where
  toJSON (BindingNewTeamUser (BindingNewTeam t) c) =
    A.object $
      "currency" A..= c
        # newTeamJson t
    where
      -- FUTUREWORK(leif): this was originally defined in libs/wire-api/src/Wire/API/Team.hs and I moved it here
      -- during the process of servantifying, it should go away when servantification is complete
      newTeamJson :: NewTeam a -> [A.Pair]
      newTeamJson (NewTeam n i ik _) =
        "name" A..= fromRange n
          # "icon" A..= fromRange i
          # "icon_key" A..= (fromRange <$> ik)
          # []

instance FromJSON BindingNewTeamUser where
  parseJSON j@(A.Object o) = do
    c <- o A..:? "currency"
    t <- parseJSON j
    return $ BindingNewTeamUser t c
  parseJSON _ = fail "parseJSON BindingNewTeamUser: must be an object"

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
        <$> uupName .= maybe_ (optField "name" schema)
        <*> uupPict .= maybe_ (optField "picture" schema)
        <*> uupAssets .= maybe_ (optField "assets" (array schema))
        <*> uupAccentId .= maybe_ (optField "accent_id" schema)

data UpdateProfileError
  = DisplayNameManagedByScim
  | ProfileNotFound
  deriving (Generic)
  deriving (AsUnion PutSelfErrorResponses) via GenericAsUnion PutSelfErrorResponses UpdateProfileError

instance GSOP.Generic UpdateProfileError

type PutSelfErrorResponses = '[NameManagedByScim, UserNotFound]

type PutSelfResponses = PutSelfErrorResponses .++ '[RespondEmpty 200 "User updated"]

instance (res ~ PutSelfResponses) => AsUnion res (Maybe UpdateProfileError) where
  toUnion = maybeToUnion (toUnion @PutSelfErrorResponses)
  fromUnion = maybeFromUnion (fromUnion @PutSelfErrorResponses)

-- | The payload for setting or changing a password.
data PasswordChange = PasswordChange
  { cpOldPassword :: Maybe PlainTextPassword,
    cpNewPassword :: PlainTextPassword
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
        <$> cpOldPassword .= maybe_ (optField "old_password" schema)
        <*> cpNewPassword .= field "new_password" schema

data ChangePasswordError
  = InvalidCurrentPassword
  | ChangePasswordNoIdentity
  | ChangePasswordMustDiffer
  deriving (Generic)
  deriving (AsUnion ChangePasswordErrorResponses) via GenericAsUnion ChangePasswordErrorResponses ChangePasswordError

instance GSOP.Generic ChangePasswordError

type ChangePasswordErrorResponses = [BadCredentials, NoIdentity, ChangePasswordMustDiffer]

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
        <$> luLocale .= field "locale" schema

newtype EmailUpdate = EmailUpdate {euEmail :: Email}
  deriving stock (Eq, Show, Generic)
  deriving newtype (Arbitrary)
  deriving (S.ToSchema) via (Schema EmailUpdate)

instance ToSchema EmailUpdate where
  schema =
    object "EmailUpdate" $
      EmailUpdate
        <$> euEmail .= field "email" schema

modelEmailUpdate :: Doc.Model
modelEmailUpdate = Doc.defineModel "EmailUpdate" $ do
  Doc.description "Email Update Data"
  Doc.property "email" Doc.string' $
    Doc.description "Email"

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
        <$> puPhone .= field "phone" schema

data ChangePhoneError
  = PhoneExists
  | InvalidNewPhone
  | BlacklistedNewPhone
  deriving (Generic)
  deriving (AsUnion ChangePhoneErrorResponses) via GenericAsUnion ChangePhoneErrorResponses ChangePhoneError

instance GSOP.Generic ChangePhoneError

type ChangePhoneErrorResponses = [UserKeyExists, InvalidPhone, BlacklistedPhone]

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

type RemoveIdentityErrorResponses = [LastIdentity, NoPassword, NoIdentity]

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
  deriving (Generic)
  deriving (AsUnion ChangeHandleErrorResponses) via GenericAsUnion ChangeHandleErrorResponses ChangeHandleError

instance GSOP.Generic ChangeHandleError

type ChangeHandleErrorResponses = [NoIdentity, HandleExists, InvalidHandle, HandleManagedByScim]

type ChangeHandleResponses =
  ChangeHandleErrorResponses .++ '[RespondEmpty 200 "Handle Changed"]

instance (res ~ ChangeHandleResponses) => AsUnion res (Maybe ChangeHandleError) where
  toUnion = maybeToUnion (toUnion @ChangeHandleErrorResponses)
  fromUnion = maybeFromUnion (fromUnion @ChangeHandleErrorResponses)

newtype NameUpdate = NameUpdate {nuHandle :: Text}
  deriving stock (Eq, Show, Generic)
  deriving newtype (Arbitrary)

instance ToJSON NameUpdate where
  toJSON h = A.object ["name" A..= nuHandle h]

instance FromJSON NameUpdate where
  parseJSON = A.withObject "name-update" $ \o ->
    NameUpdate <$> o A..: "name"

-----------------------------------------------------------------------------
-- Account Deletion

-- | Payload for requesting account deletion.
newtype DeleteUser = DeleteUser
  { deleteUserPassword :: Maybe PlainTextPassword
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Arbitrary)
  deriving (S.ToSchema) via (Schema DeleteUser)

instance ToSchema DeleteUser where
  schema =
    object "DeleteUser" $
      DeleteUser
        <$> deleteUserPassword .= maybe_ (optField "password" schema)

mkDeleteUser :: Maybe PlainTextPassword -> DeleteUser
mkDeleteUser = DeleteUser

modelDelete :: Doc.Model
modelDelete = Doc.defineModel "Delete" $ do
  Doc.description "Data for an account deletion request."
  Doc.property "password" Doc.string' $ do
    Doc.description "The account password to authorise the deletion."
    Doc.optional

instance ToJSON DeleteUser where
  toJSON d =
    A.object $
      "password" A..= deleteUserPassword d
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

modelVerifyDelete :: Doc.Model
modelVerifyDelete = Doc.defineModel "VerifyDelete" $ do
  Doc.description "Data for verifying an account deletion."
  Doc.property "key" Doc.string' $
    Doc.description "The identifying key of the account (i.e. user ID)."
  Doc.property "code" Doc.string' $
    Doc.description "The verification code."

mkVerifyDeleteUser :: Code.Key -> Code.Value -> VerifyDeleteUser
mkVerifyDeleteUser = VerifyDeleteUser

instance ToJSON VerifyDeleteUser where
  toJSON d =
    A.object
      [ "key" A..= verifyDeleteUserKey d,
        "code" A..= verifyDeleteUserCode d
      ]

instance FromJSON VerifyDeleteUser where
  parseJSON = A.withObject "VerifyDeleteUser" $ \o ->
    VerifyDeleteUser
      <$> o A..: "key"
      <*> o A..: "code"

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
        <$> fromDeletionCodeTimeout .= field "expires_in" schema

instance ToJSON DeletionCodeTimeout where
  toJSON (DeletionCodeTimeout t) = A.object ["expires_in" A..= t]

instance FromJSON DeletionCodeTimeout where
  parseJSON = A.withObject "DeletionCodeTimeout" $ \o ->
    DeletionCodeTimeout <$> o A..: "expires_in"

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
    return $
      S.NamedSchema (Just "ListUsersQuery") $
        mempty
          & S.type_ ?~ S.SwaggerObject
          & S.description ?~ "exactly one of qualified_ids or qualified_handles must be provided."
          & S.properties .~ InsOrdHashMap.fromList [("qualified_ids", uids), ("qualified_handles", handles)]
          & S.example ?~ toJSON (ListUsersByIds [Qualified (Id UUID.nil) (Domain "example.com")])

-----------------------------------------------------------------------------
-- SndFactorPasswordChallenge

-- | remove this type once we have an implementation in order to find all the places where we need to touch code.
data TeamFeatureSndFPasswordChallengeNotImplemented
  = TeamFeatureSndFPasswordChallengeNotImplemented

data SndFactorPasswordChallengeAction = GenerateScimToken | Login
  deriving stock (Eq, Show, Enum, Bounded, Generic)
  deriving (Arbitrary) via (GenericUniform SndFactorPasswordChallengeAction)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema SndFactorPasswordChallengeAction)

instance ToSchema SndFactorPasswordChallengeAction where
  schema =
    enum @Text "SndFactorPasswordChallengeAction" $
      mconcat
        [ element "generate_scim_token" GenerateScimToken,
          element "login" Login
        ]

data SendVerificationCode = SendVerificationCode
  { svcAction :: SndFactorPasswordChallengeAction,
    svcEmail :: Email
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform SendVerificationCode)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema SendVerificationCode

instance ToSchema SendVerificationCode where
  schema =
    object "SendVerificationCode" $
      SendVerificationCode
        <$> svcAction .= field "action" schema
        <*> svcEmail .= field "email" schema
