{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ViewPatterns               #-}

module Brig.Types.User
    ( module Brig.Types.User
    , module C
    ) where

import Imports
import Brig.Types.Activation (ActivationCode)
import Brig.Types.Common as C
import Brig.Types.User.Auth (CookieLabel)
import Data.Aeson
import Data.Aeson.Types (Parser, Pair)
import Data.ByteString.Conversion
import Data.Id
import Data.Json.Util ((#), UTCTimeMillis)
import Data.Misc (PlainTextPassword (..))
import Data.Range
import Data.Text.Ascii
import Data.UUID (UUID)
import Galley.Types.Bot (ServiceRef)
import Galley.Types.Teams hiding (userId)

import qualified Brig.Types.Code     as Code
import qualified Data.Currency       as Currency
import qualified Data.HashMap.Strict as HashMap

-----------------------------------------------------------------------------
-- User Attributes

-- DEPRECATED
newtype Pict = Pict { fromPict :: [Object] }
    deriving (Eq, Show, ToJSON)

instance FromJSON Pict where
    parseJSON x = Pict . fromRange <$> (parseJSON x :: Parser (Range 0 10 [Object]))

noPict :: Pict
noPict = Pict []

--------------------------------------------------------------------------------
-- UserHandleInfo

newtype UserHandleInfo = UserHandleInfo { userHandleId :: UserId }
    deriving (Eq, Show)

instance ToJSON UserHandleInfo where
    toJSON (UserHandleInfo u) = object
        [ "user" .= u ]

instance FromJSON UserHandleInfo where
    parseJSON = withObject "UserHandleInfo" $ \o ->
        UserHandleInfo <$> o .: "user"

--------------------------------------------------------------------------------
-- CheckHandles

-- | Check the availability of user handles.
data CheckHandles = CheckHandles
    { checkHandlesList :: Range 1 50 [Text]
        -- ^ Handles to check for availability, in ascending order of preference.
    , checkHandlesNum  :: Range 1 10 Word
        -- ^ Number of free handles to return. Default 1.
    } deriving (Eq, Show)

instance ToJSON CheckHandles where
    toJSON (CheckHandles l n) = object
        [ "handles" .= l
        , "return"  .= n
        ]

instance FromJSON CheckHandles where
    parseJSON = withObject "CheckHandles" $ \o ->
        CheckHandles <$> o .:  "handles"
                     <*> o .:? "return" .!= unsafeRange 1

-----------------------------------------------------------------------------
-- User Profiles

-- | A self profile.
data SelfProfile = SelfProfile
    { selfUser       :: !User }
    deriving (Eq, Show)

connectedProfile :: User -> UserProfile
connectedProfile u = UserProfile
    { profileId       = userId u
    , profileHandle   = userHandle u
    , profileName     = userName u
    , profilePict     = userPict u
    , profileAssets   = userAssets u
    , profileAccentId = userAccentId u
    , profileService  = userService u
    , profileLocale   = Just (userLocale u)
    , profileDeleted  = userDeleted u
    , profileExpire   = userExpire u
    , profileTeam     = userTeam u
    }

publicProfile :: User -> UserProfile
publicProfile u = (connectedProfile u)
    { profileLocale = Nothing
    }

-- | The data of an existing user.
data User = User
    { userId       :: !UserId
    , userIdentity :: !(Maybe UserIdentity)
    , userName     :: !Name  -- ^ required; non-unique
    , userPict     :: !Pict -- ^ DEPRECATED
    , userAssets   :: [Asset]
    , userAccentId :: !ColourId
    , userDeleted  :: !Bool
    , userLocale   :: !Locale
    , userService  :: !(Maybe ServiceRef)
        -- ^ Set if the user represents an external service,
        -- i.e. it is a "bot".
    , userHandle   :: !(Maybe Handle)  -- ^ not required; must be unique if present
    , userExpire   :: !(Maybe UTCTimeMillis)
        -- ^ Set if the user is ephemeral
    , userTeam     :: !(Maybe TeamId)
        -- ^ Set if the user is part of a binding team
    , userManagedBy :: !ManagedBy
        -- ^ How is the user profile managed (e.g. if it's via SCIM then the user profile
        -- can't be edited via normal means)
    }
    deriving (Eq, Show)

userEmail :: User -> Maybe Email
userEmail = emailIdentity <=< userIdentity

userPhone :: User -> Maybe Phone
userPhone = phoneIdentity <=< userIdentity

userSSOId :: User -> Maybe UserSSOId
userSSOId = ssoIdentity <=< userIdentity

-- | A subset of the data of an existing 'User' that is returned on the API and is visible to
-- other users. Each user also has access to their own profile in a richer format --
-- 'SelfProfile'.
data UserProfile = UserProfile
    { profileId       :: !UserId
    , profileName     :: !Name
    , profilePict     :: !Pict -- ^ DEPRECATED
    , profileAssets   :: [Asset]
    , profileAccentId :: !ColourId
    , profileDeleted  :: !Bool
    , profileService  :: !(Maybe ServiceRef)
        -- ^ Set if the user represents an external service,
        -- i.e. it is a "bot".
    , profileHandle   :: !(Maybe Handle)
    , profileLocale   :: !(Maybe Locale)
    , profileExpire   :: !(Maybe UTCTimeMillis)
    , profileTeam     :: !(Maybe TeamId)
    }
    deriving (Eq, Show)

-- TODO: disentangle json serializations for 'User', 'NewUser', 'UserIdentity', 'NewUserOrigin'.
instance ToJSON User where
    toJSON u = object
        $ "id"         .= userId u
        # "name"       .= userName u
        # "picture"    .= userPict u
        # "assets"     .= userAssets u
        # "email"      .= userEmail u
        # "phone"      .= userPhone u
        # "accent_id"  .= userAccentId u
        # "deleted"    .= (if userDeleted u then Just True else Nothing)
        # "locale"     .= userLocale u
        # "service"    .= userService u
        # "handle"     .= userHandle u
        # "expires_at" .= userExpire u
        # "team"       .= userTeam u
        # "sso_id"     .= userSSOId u
        # "managed_by" .= userManagedBy u
        # []

instance FromJSON User where
    parseJSON = withObject "user" $ \o -> do
        ssoid <- o .:? "sso_id"
        User <$> o .:  "id"
             <*> parseIdentity ssoid o
             <*> o .:  "name"
             <*> o .:? "picture" .!= noPict
             <*> o .:? "assets"  .!= []
             <*> o .:  "accent_id"
             <*> o .:? "deleted" .!= False
             <*> o .:  "locale"
             <*> o .:? "service"
             <*> o .:? "handle"
             <*> o .:? "expires_at"
             <*> o .:? "team"
             <*> o .:? "managed_by" .!= ManagedByWire

instance FromJSON UserProfile where
    parseJSON = withObject "UserProfile" $ \o ->
        UserProfile <$> o .:  "id"
                    <*> o .:  "name"
                    <*> o .:? "picture" .!= noPict
                    <*> o .:? "assets"  .!= []
                    <*> o .:  "accent_id"
                    <*> o .:? "deleted" .!= False
                    <*> o .:? "service"
                    <*> o .:? "handle"
                    <*> o .:? "locale"
                    <*> o .:? "expires_at"
                    <*> o .:? "team"

instance ToJSON UserProfile where
    toJSON u = object
        $ "id"         .= profileId u
        # "name"       .= profileName u
        # "picture"    .= profilePict u
        # "assets"     .= profileAssets u
        # "accent_id"  .= profileAccentId u
        # "deleted"    .= (if profileDeleted u then Just True else Nothing)
        # "service"    .= profileService u
        # "handle"     .= profileHandle u
        # "locale"     .= profileLocale u
        # "expires_at" .= profileExpire u
        # "team"       .= profileTeam u
        # []

instance FromJSON SelfProfile where
    parseJSON = withObject "SelfProfile" $ \o ->
        SelfProfile <$> parseJSON (Object o)

instance ToJSON SelfProfile where
    toJSON (SelfProfile u) = toJSON u

-----------------------------------------------------------------------------
-- New Users

data NewUser = NewUser
    { newUserName           :: !Name
    , newUserUUID           :: !(Maybe UUID)  -- ^ use this as 'UserId' (if 'Nothing', call 'Data.UUID.nextRandom').
    , newUserIdentity       :: !(Maybe UserIdentity)
    , newUserPict           :: !(Maybe Pict) -- ^ DEPRECATED
    , newUserAssets         :: [Asset]
    , newUserAccentId       :: !(Maybe ColourId)
    , newUserEmailCode      :: !(Maybe ActivationCode)
    , newUserPhoneCode      :: !(Maybe ActivationCode)
    , newUserOrigin         :: !(Maybe NewUserOrigin)
    , newUserLabel          :: !(Maybe CookieLabel)
    , newUserLocale         :: !(Maybe Locale)
    , newUserPassword       :: !(Maybe PlainTextPassword)
    , newUserExpiresIn      :: !(Maybe ExpiresIn)
    }
    deriving (Eq, Show)

-- | 1 second - 1 week
type ExpiresIn = Range 1 604800 Integer

data NewUserOrigin =
    NewUserOriginInvitationCode !InvitationCode
  | NewUserOriginTeamUser !NewTeamUser
  deriving (Eq, Show)

parseNewUserOrigin :: Maybe PlainTextPassword -> Maybe UserIdentity -> Maybe UserSSOId
                   -> Object -> Parser (Maybe NewUserOrigin)
parseNewUserOrigin pass uid ssoid o = do
    invcode  <- o .:? "invitation_code"
    teamcode <- o .:? "team_code"
    team     <- o .:? "team"
    teamid   <- o .:? "team_id"
    result <- case (invcode, teamcode, team, ssoid, teamid) of
        (Just a,  Nothing, Nothing, Nothing, Nothing) -> return . Just . NewUserOriginInvitationCode $ a
        (Nothing, Just a,  Nothing, Nothing, Nothing) -> return . Just . NewUserOriginTeamUser $ NewTeamMember a
        (Nothing, Nothing, Just a,  Nothing, Nothing) -> return . Just . NewUserOriginTeamUser $ NewTeamCreator a
        (Nothing, Nothing, Nothing, Just _,  Just t)  -> return . Just . NewUserOriginTeamUser $ NewTeamMemberSSO t
        (Nothing, Nothing, Nothing, Nothing, Nothing) -> return Nothing
        (_, _, _, _, _) -> fail $ "team_code, team, invitation_code, sso_id are mutually exclusive\
                                  \ and sso_id, team_id must be either both present or both absent."
    case (result, pass, uid) of
        (_, _, Just SSOIdentity {}) -> pure result
        (Just (NewUserOriginTeamUser _), Nothing, _) -> fail "all team users must set a password on creation"
        _ -> pure result

jsonNewUserOrigin :: NewUserOrigin -> [Pair]
jsonNewUserOrigin = \case
    NewUserOriginInvitationCode inv             -> ["invitation_code" .= inv]
    NewUserOriginTeamUser (NewTeamMember tc)    -> ["team_code" .= tc]
    NewUserOriginTeamUser (NewTeamCreator team) -> ["team" .= team]
    NewUserOriginTeamUser (NewTeamMemberSSO ti) -> ["team_id" .= ti]

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

instance FromJSON NewUser where
      parseJSON = withObject "new-user" $ \o -> do
          ssoid                 <- o .:? "sso_id"
          newUserName           <- o .: "name"
          newUserUUID           <- o .:? "uuid"
          newUserIdentity       <- parseIdentity ssoid o
          newUserPict           <- o .:? "picture"
          newUserAssets         <- o .:? "assets" .!= []
          newUserAccentId       <- o .:? "accent_id"
          newUserEmailCode      <- o .:? "email_code"
          newUserPhoneCode      <- o .:? "phone_code"
          newUserLabel          <- o .:? "label"
          newUserLocale         <- o .:? "locale"
          newUserPassword       <- o .:? "password"
          newUserOrigin         <- parseNewUserOrigin newUserPassword newUserIdentity ssoid o
          newUserExpires   <- o .:? "expires_in"
          newUserExpiresIn <- case (newUserExpires, newUserIdentity) of
                (Just _, Just _) -> fail "Only users without an identity can expire"
                _                -> return newUserExpires
          return NewUser{..}

instance ToJSON NewUser where
    toJSON u = object
        $ "name"            .= newUserName u
        # "uuid"            .= newUserUUID u
        # "email"           .= newUserEmail u
        # "email_code"      .= newUserEmailCode u
        # "password"        .= newUserPassword u
        # "picture"         .= newUserPict u
        # "assets"          .= newUserAssets u
        # "phone"           .= newUserPhone u
        # "phone_code"      .= newUserPhoneCode u
        # "accent_id"       .= newUserAccentId u
        # "label"           .= newUserLabel u
        # "locale"          .= newUserLocale u
        # "password"        .= newUserPassword u
        # "expires_in"      .= newUserExpiresIn u
        # "sso_id"          .= newUserSSOId u
        # maybe [] jsonNewUserOrigin (newUserOrigin u)

-- | Fails if email or phone or ssoid are present but invalid
parseIdentity :: Maybe UserSSOId -> Object -> Parser (Maybe UserIdentity)
parseIdentity ssoid o = if isJust (HashMap.lookup "email" o <|> HashMap.lookup "phone" o) || isJust ssoid
    then Just <$> parseJSON (Object o)
    else pure Nothing

-- | A random invitation code for use during registration
newtype InvitationCode = InvitationCode
    { fromInvitationCode :: AsciiBase64Url }
    deriving (Eq, Show, FromJSON, ToJSON, ToByteString, FromByteString)

data BindingNewTeamUser = BindingNewTeamUser
    { bnuTeam     :: !BindingNewTeam
    , bnuCurrency :: !(Maybe Currency.Alpha)
    -- TODO: Remove Currency selection once billing supports currency changes after team creation
    }
    deriving (Eq, Show)

instance FromJSON BindingNewTeamUser where
    parseJSON j@(Object o) = do
        c <- o .:? "currency"
        t <- parseJSON j
        return $ BindingNewTeamUser t c
    parseJSON _ = fail "parseJSON BindingNewTeamUser: must be an object"

instance ToJSON BindingNewTeamUser where
    toJSON (BindingNewTeamUser t c) =
        let (Object t') = toJSON t
         in object $ "currency" .= c
                   # HashMap.toList t'

data NewTeamUser = NewTeamMember    !InvitationCode      -- ^ requires email address
                 | NewTeamCreator   !BindingNewTeamUser
                 | NewTeamMemberSSO !TeamId
    deriving (Eq, Show)

-- | newtype for using in external end-points where setting 'SSOIdentity', 'UUID' is not allowed.
-- ('UUID' is only needed by spar for creating users that it can find again later, after a crash.
-- if there another use case arises, this newtype and the 'FromJSON' instance would have to be
-- refactored.)
newtype NewUserNoSSO = NewUserNoSSO NewUser
    deriving (Eq, Show)

instance FromJSON NewUserNoSSO where
    parseJSON val = do
        nu <- parseJSON val
        when (isJust $ newUserSSOId nu) $ fail "SSO-managed users are not allowed here."
        when (isJust $ newUserUUID nu)  $ fail "it is not allowed to provide a UUID for the users here."
        pure $ NewUserNoSSO nu


-----------------------------------------------------------------------------
-- Profile Updates

data UserUpdate = UserUpdate
    { uupName     :: !(Maybe Name)
    , uupPict     :: !(Maybe Pict) -- DEPRECATED
    , uupAssets   :: !(Maybe [Asset])
    , uupAccentId :: !(Maybe ColourId)
    } deriving (Eq, Show)

newtype LocaleUpdate = LocaleUpdate { luLocale :: Locale } deriving (Eq, Show)

newtype EmailUpdate  = EmailUpdate  { euEmail  :: Email  } deriving (Eq, Show)
newtype PhoneUpdate  = PhoneUpdate  { puPhone  :: Phone  } deriving (Eq, Show)
newtype HandleUpdate = HandleUpdate { huHandle :: Text   } deriving (Eq, Show)
newtype EmailRemove  = EmailRemove  { erEmail  :: Email  } deriving (Eq, Show)
newtype PhoneRemove  = PhoneRemove  { prPhone  :: Phone  } deriving (Eq, Show)

instance FromJSON UserUpdate where
    parseJSON = withObject "UserUpdate" $ \o ->
        UserUpdate <$> o .:? "name"
                   <*> o .:? "picture"
                   <*> o .:? "assets"
                   <*> o .:? "accent_id"

instance ToJSON UserUpdate where
    toJSON u = object
        $ "name"       .= uupName u
        # "picture"    .= uupPict u
        # "assets"     .= uupAssets u
        # "accent_id"  .= uupAccentId u
        # []

instance FromJSON LocaleUpdate where
    parseJSON = withObject "locale-update" $ \o ->
        LocaleUpdate <$> o .: "locale"

instance ToJSON LocaleUpdate where
    toJSON l = object ["locale" .= luLocale l]

instance FromJSON EmailUpdate where
    parseJSON = withObject "email-update" $ \o ->
        EmailUpdate <$> o .: "email"

instance ToJSON EmailUpdate where
    toJSON e = object ["email" .= euEmail e]

instance FromJSON PhoneUpdate where
    parseJSON = withObject "phone-update" $ \o ->
        PhoneUpdate <$> o .: "phone"

instance ToJSON PhoneUpdate where
    toJSON p = object ["phone" .= puPhone p]

instance FromJSON HandleUpdate where
    parseJSON = withObject "handle-update" $ \o ->
        HandleUpdate <$> o .: "handle"

instance ToJSON HandleUpdate where
    toJSON h = object ["handle" .= huHandle h]

instance FromJSON EmailRemove where
    parseJSON = withObject "email-remove" $ \o ->
        EmailRemove <$> o .: "email"

instance ToJSON EmailRemove where
    toJSON e = object ["email" .= erEmail e]

instance FromJSON PhoneRemove where
    parseJSON = withObject "phone-remove" $ \o ->
        PhoneRemove <$> o .: "phone"

instance ToJSON PhoneRemove where
    toJSON p = object ["phone" .= prPhone p]

-----------------------------------------------------------------------------
-- Account Deletion

-- | Payload for requesting account deletion.
newtype DeleteUser = DeleteUser
    { deleteUserPassword :: Maybe PlainTextPassword
    }
    deriving (Eq, Show)

mkDeleteUser :: Maybe PlainTextPassword -> DeleteUser
mkDeleteUser = DeleteUser

-- | Payload for verifying account deletion via a code.
data VerifyDeleteUser = VerifyDeleteUser
    { verifyDeleteUserKey  :: !Code.Key
    , verifyDeleteUserCode :: !Code.Value
    } deriving (Eq, Show)

mkVerifyDeleteUser :: Code.Key -> Code.Value -> VerifyDeleteUser
mkVerifyDeleteUser = VerifyDeleteUser

-- | A response for a pending deletion code.
newtype DeletionCodeTimeout = DeletionCodeTimeout
    { fromDeletionCodeTimeout :: Code.Timeout }
    deriving (Eq, Show)

instance ToJSON DeleteUser where
    toJSON d = object
        $ "password" .= deleteUserPassword d
        # []

instance FromJSON DeleteUser where
    parseJSON = withObject "DeleteUser" $ \o ->
        DeleteUser <$> o .:? "password"

instance ToJSON VerifyDeleteUser where
    toJSON d = object
        [ "key"  .= verifyDeleteUserKey d
        , "code" .= verifyDeleteUserCode d
        ]

instance FromJSON VerifyDeleteUser where
    parseJSON = withObject "VerifyDeleteUser" $ \o ->
        VerifyDeleteUser <$> o .: "key"
                         <*> o .: "code"

instance FromJSON DeletionCodeTimeout where
    parseJSON = withObject "DeletionCodeTimeout" $ \o ->
        DeletionCodeTimeout <$> o .: "expires_in"

instance ToJSON DeletionCodeTimeout where
    toJSON (DeletionCodeTimeout t) = object [ "expires_in" .= t ]

-----------------------------------------------------------------------------
-- Password Change / Reset

-- | The payload for initiating a password reset.
newtype NewPasswordReset = NewPasswordReset (Either Email Phone)
    deriving (Eq, Show)

-- | Opaque identifier per user (SHA256 of the user ID).
newtype PasswordResetKey = PasswordResetKey
    { fromPasswordResetKey :: AsciiBase64Url }
    deriving (Eq, Show, FromByteString, ToByteString, FromJSON, ToJSON)

-- | Random code, acting as a very short-lived, single-use password.
newtype PasswordResetCode = PasswordResetCode
    { fromPasswordResetCode :: AsciiBase64Url }
    deriving (Eq, Show, FromByteString, ToByteString, FromJSON, ToJSON)

type PasswordResetPair = (PasswordResetKey, PasswordResetCode)

-- | The target identity of a password reset.
data PasswordResetIdentity
    = PasswordResetIdentityKey !PasswordResetKey
        -- ^ An opaque identity key for a pending password reset.
    | PasswordResetEmailIdentity !Email
        -- ^ A known email address with a pending password reset.
    | PasswordResetPhoneIdentity !Phone
        -- ^ A known phone number with a pending password reset.
    deriving (Eq, Show)

-- | The payload for completing a password reset.
data CompletePasswordReset = CompletePasswordReset
    { cpwrIdent    :: !PasswordResetIdentity
    , cpwrCode     :: !PasswordResetCode
    , cpwrPassword :: !PlainTextPassword
    }
    deriving (Eq, Show)

-- | The payload for setting or changing a password.
data PasswordChange = PasswordChange
    { cpOldPassword :: !(Maybe PlainTextPassword)
    , cpNewPassword :: !PlainTextPassword
    }
    deriving (Eq, Show)

instance FromJSON NewPasswordReset where
    parseJSON = withObject "NewPasswordReset" $ \o ->
        NewPasswordReset <$> (  (Left  <$> o .: "email")
                            <|> (Right <$> o .: "phone")
                             )

instance ToJSON NewPasswordReset where
    toJSON (NewPasswordReset ident) = object
        [ either ("email" .=) ("phone" .=) ident ]

instance FromJSON CompletePasswordReset where
    parseJSON = withObject "CompletePasswordReset" $ \o ->
        CompletePasswordReset <$> ident o <*> o .: "code" <*> o .: "password"
      where
        ident o =  (PasswordResetIdentityKey   <$> o .: "key")
               <|> (PasswordResetEmailIdentity <$> o .: "email")
               <|> (PasswordResetPhoneIdentity <$> o .: "phone")

instance ToJSON CompletePasswordReset where
    toJSON (CompletePasswordReset i c pw) = object
        [ ident i, "code" .= c, "password" .= pw ]
      where
        ident (PasswordResetIdentityKey   k) = "key"   .= k
        ident (PasswordResetEmailIdentity e) = "email" .= e
        ident (PasswordResetPhoneIdentity p) = "phone" .= p

instance ToJSON PasswordChange where
    toJSON (PasswordChange old new) = object
        [ "old_password" .= old
        , "new_password" .= new
        ]

instance FromJSON PasswordChange where
    parseJSON = withObject "PasswordChange" $ \o ->
        PasswordChange <$> o .:? "old_password"
                       <*> o .:  "new_password"

-- DEPRECATED

data PasswordReset = PasswordReset
    { pwrCode     :: !PasswordResetCode
    , pwrPassword :: !PlainTextPassword
    }

instance FromJSON PasswordReset where
    parseJSON = withObject "PasswordReset" $ \o ->
        PasswordReset <$> o .: "code"
                      <*> o .: "password"
