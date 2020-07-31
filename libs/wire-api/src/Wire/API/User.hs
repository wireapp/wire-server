{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
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

module Wire.API.User
  ( UserIdList (..),
    -- Profiles
    UserProfile (..),
    SelfProfile (..),
    -- User (should not be here)
    User (..),
    userEmail,
    userPhone,
    userSSOId,
    connectedProfile,
    publicProfile,

    -- * NewUser
    NewUserPublic (..),
    NewUser (..),
    ExpiresIn,
    newUserInvitationCode,
    newUserTeam,
    newUserEmail,
    newUserPhone,
    newUserSSOId,

    -- * NewUserOrigin
    NewUserOrigin (..),
    InvitationCode (..),
    NewTeamUser (..),
    BindingNewTeamUser (..),

    -- * Profile Updates
    UserUpdate (..),
    PasswordChange (..),
    LocaleUpdate (..),
    EmailUpdate (..),
    PhoneUpdate (..),
    HandleUpdate (..),

    -- * Account Deletion
    DeleteUser (..),
    mkDeleteUser,
    VerifyDeleteUser (..),
    mkVerifyDeleteUser,
    DeletionCodeTimeout (..),

    -- * helpers
    parseIdentity,

    -- * re-exports
    module Wire.API.User.Identity,
    module Wire.API.User.Profile,

    -- * Swagger
    modelUserIdList,
    modelSelf,
    modelUser,
    modelNewUser,
    modelUserUpdate,
    modelChangePassword,
    modelChangeLocale,
    modelEmailUpdate,
    modelPhoneUpdate,
    modelChangeHandle,
    modelDelete,
    modelVerifyDelete,
  )
where

import Control.Error.Safe (rightMay)
import Data.Aeson
import qualified Data.Aeson.Types as Aeson
import Data.ByteString.Conversion
import qualified Data.Code as Code
import qualified Data.Currency as Currency
import Data.Handle (Handle)
import qualified Data.HashMap.Strict as HashMap
import Data.Id
import Data.Json.Util (UTCTimeMillis, (#))
import Data.Misc (PlainTextPassword (..))
import Data.Range
import qualified Data.Swagger.Build.Api as Doc
import Data.Text.Ascii
import Data.UUID (UUID, nil)
import Imports
import qualified Test.QuickCheck as QC
import Wire.API.Arbitrary (Arbitrary (arbitrary), GenericUniform (..))
import Wire.API.Provider.Service (ServiceRef, modelServiceRef)
import Wire.API.Team (BindingNewTeam (BindingNewTeam), modelNewBindingTeam, newTeamJson)
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
newtype UserIdList = UserIdList
  {mUsers :: [UserId]}
  deriving stock (Eq, Show, Generic)
  deriving newtype (Arbitrary)

modelUserIdList :: Doc.Model
modelUserIdList = Doc.defineModel "UserIdList" $ do
  Doc.description "list of user ids"
  Doc.property "user_ids" (Doc.unique $ Doc.array Doc.bytes') $
    Doc.description "the array of team conversations"

instance FromJSON UserIdList where
  parseJSON = withObject "user-ids-payload" $ \o ->
    UserIdList <$> o .: "user_ids"

instance ToJSON UserIdList where
  toJSON e = object ["user_ids" .= mUsers e]

--------------------------------------------------------------------------------
-- UserProfile

-- | A subset of the data of an existing 'User' that is returned on the API and is visible to
-- other users. Each user also has access to their own profile in a richer format --
-- 'SelfProfile'.
data UserProfile = UserProfile
  { profileId :: UserId,
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
    profileLocale :: Maybe Locale,
    profileExpire :: Maybe UTCTimeMillis,
    profileTeam :: Maybe TeamId,
    profileEmail :: Maybe Email
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform UserProfile)

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

instance ToJSON UserProfile where
  toJSON u =
    object $
      "id" .= profileId u
        # "name" .= profileName u
        # "picture" .= profilePict u
        # "assets" .= profileAssets u
        # "accent_id" .= profileAccentId u
        # "deleted" .= (if profileDeleted u then Just True else Nothing)
        # "service" .= profileService u
        # "handle" .= profileHandle u
        # "locale" .= profileLocale u
        # "expires_at" .= profileExpire u
        # "team" .= profileTeam u
        # "email" .= profileEmail u
        # []

instance FromJSON UserProfile where
  parseJSON = withObject "UserProfile" $ \o ->
    UserProfile
      <$> o .: "id"
      <*> o .: "name"
      <*> o .:? "picture" .!= noPict
      <*> o .:? "assets" .!= []
      <*> o .: "accent_id"
      <*> o .:? "deleted" .!= False
      <*> o .:? "service"
      <*> o .:? "handle"
      <*> o .:? "locale"
      <*> o .:? "expires_at"
      <*> o .:? "team"
      <*> o .:? "email"

--------------------------------------------------------------------------------
-- SelfProfile

-- | A self profile.
data SelfProfile = SelfProfile
  { selfUser :: User
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform SelfProfile)

modelSelf :: Doc.Model
modelSelf = Doc.defineModel "Self" $ do
  Doc.description "Self Profile"
  Doc.property "id" Doc.bytes' $
    Doc.description "User ID"
  Doc.property "name" Doc.string' $
    Doc.description "Name"
  Doc.property "assets" (Doc.array (Doc.ref modelAsset)) $
    Doc.description "Profile assets"
  Doc.property "email" Doc.string' $ do
    Doc.description "Email address"
    Doc.optional
  Doc.property "phone" Doc.string' $ do
    Doc.description "E.164 Phone number"
    Doc.optional
  Doc.property "accent_id" Doc.int32' $ do
    Doc.description "Accent colour ID"
    Doc.optional
  Doc.property "locale" Doc.string' $
    Doc.description "Locale in <ln-cc> format."
  Doc.property "handle" Doc.string' $ do
    Doc.description "Unique handle."
    Doc.optional
  Doc.property "deleted" Doc.bool' $ do
    Doc.description "Whether the account has been deleted."
    Doc.optional
  Doc.property "managed_by" typeManagedBy $ do
    Doc.description
      "What is the source of truth for this user; if it's SCIM \
      \then the profile can't be edited via normal means"
    Doc.optional

instance ToJSON SelfProfile where
  toJSON (SelfProfile u) = toJSON u

instance FromJSON SelfProfile where
  parseJSON = withObject "SelfProfile" $ \o ->
    SelfProfile <$> parseJSON (Object o)

--------------------------------------------------------------------------------
-- User
--
-- FUTUREWORK: Move this type somewhere else, it's not part of the client API.

-- | The data of an existing user.
data User = User
  { userId :: UserId,
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

-- FUTUREWORK:
-- disentangle json serializations for 'User', 'NewUser', 'UserIdentity', 'NewUserOrigin'.
instance ToJSON User where
  toJSON u =
    object $
      "id" .= userId u
        # "name" .= userDisplayName u
        # "picture" .= userPict u
        # "assets" .= userAssets u
        # "email" .= userEmail u
        # "phone" .= userPhone u
        # "accent_id" .= userAccentId u
        # "deleted" .= (if userDeleted u then Just True else Nothing)
        # "locale" .= userLocale u
        # "service" .= userService u
        # "handle" .= userHandle u
        # "expires_at" .= userExpire u
        # "team" .= userTeam u
        # "sso_id" .= userSSOId u
        # "managed_by" .= userManagedBy u
        # []

instance FromJSON User where
  parseJSON = withObject "user" $ \o -> do
    ssoid <- o .:? "sso_id"
    User
      <$> o .: "id"
      <*> parseIdentity ssoid o
      <*> o .: "name"
      <*> o .:? "picture" .!= noPict
      <*> o .:? "assets" .!= []
      <*> o .: "accent_id"
      <*> o .:? "deleted" .!= False
      <*> o .: "locale"
      <*> o .:? "service"
      <*> o .:? "handle"
      <*> o .:? "expires_at"
      <*> o .:? "team"
      <*> o .:? "managed_by" .!= ManagedByWire

userEmail :: User -> Maybe Email
userEmail = emailIdentity <=< userIdentity

userPhone :: User -> Maybe Phone
userPhone = phoneIdentity <=< userIdentity

userSSOId :: User -> Maybe UserSSOId
userSSOId = ssoIdentity <=< userIdentity

connectedProfile :: User -> UserProfile
connectedProfile u =
  UserProfile
    { profileId = userId u,
      profileHandle = userHandle u,
      profileName = userDisplayName u,
      profilePict = userPict u,
      profileAssets = userAssets u,
      profileAccentId = userAccentId u,
      profileService = userService u,
      profileLocale = Just (userLocale u),
      profileDeleted = userDeleted u,
      profileExpire = userExpire u,
      profileTeam = userTeam u,
      -- We don't want to show the email by default;
      -- However we do allow adding it back in intentionally later.
      profileEmail = Nothing
    }

-- FUTUREWORK: should public and conect profile be separate types?
publicProfile :: User -> UserProfile
publicProfile u =
  -- Note that we explicitly unpack and repack the types here rather than using
  -- RecordWildCards or something similar because we want changes to the public profile
  -- to be EXPLICIT and INTENTIONAL so we don't accidentally leak sensitive data.
  let UserProfile
        { profileId,
          profileHandle,
          profileName,
          profilePict,
          profileAssets,
          profileAccentId,
          profileService,
          profileDeleted,
          profileExpire,
          profileTeam
        } = connectedProfile u
   in UserProfile
        { profileLocale = Nothing,
          profileEmail = Nothing,
          profileId,
          profileHandle,
          profileName,
          profilePict,
          profileAssets,
          profileAccentId,
          profileService,
          profileDeleted,
          profileExpire,
          profileTeam
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

-- | 1 second - 1 week
type ExpiresIn = Range 1 604800 Integer

instance ToJSON NewUser where
  toJSON u =
    object $
      "name" .= newUserDisplayName u
        # "uuid" .= newUserUUID u
        # "email" .= newUserEmail u
        # "email_code" .= newUserEmailCode u
        # "picture" .= newUserPict u
        # "assets" .= newUserAssets u
        # "phone" .= newUserPhone u
        # "phone_code" .= newUserPhoneCode u
        # "accent_id" .= newUserAccentId u
        # "label" .= newUserLabel u
        # "locale" .= newUserLocale u
        # "password" .= newUserPassword u
        # "expires_in" .= newUserExpiresIn u
        # "sso_id" .= newUserSSOId u
        # "managed_by" .= newUserManagedBy u
        # maybe [] jsonNewUserOrigin (newUserOrigin u)

instance FromJSON NewUser where
  parseJSON = withObject "new-user" $ \o -> do
    ssoid <- o .:? "sso_id"
    newUserDisplayName <- o .: "name"
    newUserUUID <- o .:? "uuid"
    newUserIdentity <- parseIdentity ssoid o
    newUserPict <- o .:? "picture"
    newUserAssets <- o .:? "assets" .!= []
    newUserAccentId <- o .:? "accent_id"
    newUserEmailCode <- o .:? "email_code"
    newUserPhoneCode <- o .:? "phone_code"
    newUserLabel <- o .:? "label"
    newUserLocale <- o .:? "locale"
    newUserPassword <- o .:? "password"
    newUserOrigin <- parseNewUserOrigin newUserPassword newUserIdentity ssoid o
    newUserExpires <- o .:? "expires_in"
    newUserExpiresIn <- case (newUserExpires, newUserIdentity) of
      (Just _, Just _) -> fail "Only users without an identity can expire"
      _ -> return newUserExpires
    newUserManagedBy <- o .:? "managed_by"
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

jsonNewUserOrigin :: NewUserOrigin -> [Aeson.Pair]
jsonNewUserOrigin = \case
  NewUserOriginInvitationCode inv -> ["invitation_code" .= inv]
  NewUserOriginTeamUser (NewTeamMember tc) -> ["team_code" .= tc]
  NewUserOriginTeamUser (NewTeamCreator team) -> ["team" .= team]
  NewUserOriginTeamUser (NewTeamMemberSSO ti) -> ["team_id" .= ti]

parseNewUserOrigin ::
  Maybe PlainTextPassword ->
  Maybe UserIdentity ->
  Maybe UserSSOId ->
  Object ->
  Aeson.Parser (Maybe NewUserOrigin)
parseNewUserOrigin pass uid ssoid o = do
  invcode <- o .:? "invitation_code"
  teamcode <- o .:? "team_code"
  team <- o .:? "team"
  teamid <- o .:? "team_id"
  result <- case (invcode, teamcode, team, ssoid, teamid) of
    (Just a, Nothing, Nothing, Nothing, Nothing) -> return . Just . NewUserOriginInvitationCode $ a
    (Nothing, Just a, Nothing, Nothing, Nothing) -> return . Just . NewUserOriginTeamUser $ NewTeamMember a
    (Nothing, Nothing, Just a, Nothing, Nothing) -> return . Just . NewUserOriginTeamUser $ NewTeamCreator a
    (Nothing, Nothing, Nothing, Just _, Just t) -> return . Just . NewUserOriginTeamUser $ NewTeamMemberSSO t
    (Nothing, Nothing, Nothing, Nothing, Nothing) -> return Nothing
    (_, _, _, _, _) ->
      fail $
        "team_code, team, invitation_code, sso_id are mutually exclusive\
        \ and sso_id, team_id must be either both present or both absent."
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
parseIdentity :: Maybe UserSSOId -> Object -> Aeson.Parser (Maybe UserIdentity)
parseIdentity ssoid o =
  if isJust (HashMap.lookup "email" o <|> HashMap.lookup "phone" o) || isJust ssoid
    then Just <$> parseJSON (Object o)
    else pure Nothing

--------------------------------------------------------------------------------
-- NewTeamUser

data NewTeamUser
  = -- | requires email address
    NewTeamMember InvitationCode
  | NewTeamCreator BindingNewTeamUser
  | NewTeamMemberSSO TeamId
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
    object $
      "currency" .= c
        # newTeamJson t

instance FromJSON BindingNewTeamUser where
  parseJSON j@(Object o) = do
    c <- o .:? "currency"
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
  deriving (Arbitrary) via (GenericUniform UserUpdate)

modelUserUpdate :: Doc.Model
modelUserUpdate = Doc.defineModel "UserUpdate" $ do
  Doc.description "User Update Data"
  Doc.property "name" Doc.string' $
    Doc.description "Name (1 - 128 characters)"
  Doc.property "assets" (Doc.array (Doc.ref modelAsset)) $ do
    Doc.description "Profile assets"
    Doc.optional
  Doc.property "accent_id" Doc.int32' $ do
    Doc.description "Accent colour ID"
    Doc.optional

instance ToJSON UserUpdate where
  toJSON u =
    object $
      "name" .= uupName u
        # "picture" .= uupPict u
        # "assets" .= uupAssets u
        # "accent_id" .= uupAccentId u
        # []

instance FromJSON UserUpdate where
  parseJSON = withObject "UserUpdate" $ \o ->
    UserUpdate
      <$> o .:? "name"
      <*> o .:? "picture"
      <*> o .:? "assets"
      <*> o .:? "accent_id"

-- | The payload for setting or changing a password.
data PasswordChange = PasswordChange
  { cpOldPassword :: Maybe PlainTextPassword,
    cpNewPassword :: PlainTextPassword
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform PasswordChange)

modelChangePassword :: Doc.Model
modelChangePassword = Doc.defineModel "ChangePassword" $ do
  Doc.description
    "Data to change a password. The old password is required if \
    \a password already exists."
  Doc.property "old_password" Doc.string' $ do
    Doc.description "Old password"
    Doc.optional
  Doc.property "new_password" Doc.string' $
    Doc.description "New password (6 - 1024 characters)"

instance ToJSON PasswordChange where
  toJSON (PasswordChange old new) =
    object
      [ "old_password" .= old,
        "new_password" .= new
      ]

instance FromJSON PasswordChange where
  parseJSON = withObject "PasswordChange" $ \o ->
    PasswordChange
      <$> o .:? "old_password"
      <*> o .: "new_password"

newtype LocaleUpdate = LocaleUpdate {luLocale :: Locale}
  deriving stock (Eq, Show, Generic)
  deriving newtype (Arbitrary)

modelChangeLocale :: Doc.Model
modelChangeLocale = Doc.defineModel "ChangeLocale" $ do
  Doc.description "Data to change a locale."
  Doc.property "locale" Doc.string' $
    Doc.description "Locale to be set"

instance ToJSON LocaleUpdate where
  toJSON l = object ["locale" .= luLocale l]

instance FromJSON LocaleUpdate where
  parseJSON = withObject "locale-update" $ \o ->
    LocaleUpdate <$> o .: "locale"

newtype EmailUpdate = EmailUpdate {euEmail :: Email}
  deriving stock (Eq, Show, Generic)
  deriving newtype (Arbitrary)

modelEmailUpdate :: Doc.Model
modelEmailUpdate = Doc.defineModel "EmailUpdate" $ do
  Doc.description "Email Update Data"
  Doc.property "email" Doc.string' $
    Doc.description "Email"

instance ToJSON EmailUpdate where
  toJSON e = object ["email" .= euEmail e]

instance FromJSON EmailUpdate where
  parseJSON = withObject "email-update" $ \o ->
    EmailUpdate <$> o .: "email"

newtype PhoneUpdate = PhoneUpdate {puPhone :: Phone}
  deriving stock (Eq, Show, Generic)
  deriving newtype (Arbitrary)

modelPhoneUpdate :: Doc.Model
modelPhoneUpdate = Doc.defineModel "PhoneUpdate" $ do
  Doc.description "Phone Update Data"
  Doc.property "phone" Doc.string' $
    Doc.description "E.164 phone number"

instance ToJSON PhoneUpdate where
  toJSON p = object ["phone" .= puPhone p]

instance FromJSON PhoneUpdate where
  parseJSON = withObject "phone-update" $ \o ->
    PhoneUpdate <$> o .: "phone"

newtype HandleUpdate = HandleUpdate {huHandle :: Text}
  deriving stock (Eq, Show, Generic)
  deriving newtype (Arbitrary)

modelChangeHandle :: Doc.Model
modelChangeHandle = Doc.defineModel "ChangeHandle" $ do
  Doc.description "Change the handle."
  Doc.property "handle" Doc.string' $
    Doc.description "Handle to set"

instance ToJSON HandleUpdate where
  toJSON h = object ["handle" .= huHandle h]

instance FromJSON HandleUpdate where
  parseJSON = withObject "handle-update" $ \o ->
    HandleUpdate <$> o .: "handle"

-----------------------------------------------------------------------------
-- Account Deletion

-- | Payload for requesting account deletion.
newtype DeleteUser = DeleteUser
  { deleteUserPassword :: Maybe PlainTextPassword
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Arbitrary)

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
    object $
      "password" .= deleteUserPassword d
        # []

instance FromJSON DeleteUser where
  parseJSON = withObject "DeleteUser" $ \o ->
    DeleteUser <$> o .:? "password"

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
    object
      [ "key" .= verifyDeleteUserKey d,
        "code" .= verifyDeleteUserCode d
      ]

instance FromJSON VerifyDeleteUser where
  parseJSON = withObject "VerifyDeleteUser" $ \o ->
    VerifyDeleteUser
      <$> o .: "key"
      <*> o .: "code"

-- | A response for a pending deletion code.
newtype DeletionCodeTimeout = DeletionCodeTimeout
  {fromDeletionCodeTimeout :: Code.Timeout}
  deriving stock (Eq, Show, Generic)
  deriving newtype (Arbitrary)

instance ToJSON DeletionCodeTimeout where
  toJSON (DeletionCodeTimeout t) = object ["expires_in" .= t]

instance FromJSON DeletionCodeTimeout where
  parseJSON = withObject "DeletionCodeTimeout" $ \o ->
    DeletionCodeTimeout <$> o .: "expires_in"
