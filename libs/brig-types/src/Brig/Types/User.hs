{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Brig.Types.User
    ( module Brig.Types.User
    , module C
    ) where

import Brig.Types.Activation (ActivationCode)
import Brig.Types.User.Auth (CookieLabel)
import Brig.Types.Common as C
import Control.Applicative
import Control.Monad ((<=<))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString.Conversion
import Data.Id
import Data.Json.Util ((#))
import Data.Misc (PlainTextPassword (..))
import Data.Maybe (isJust)
import Data.Range
import Data.Text (Text)
import Data.Text.Ascii
import Galley.Types.Bot (ServiceRef)

import qualified Brig.Types.Code     as Code
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
    }

publicProfile :: User -> UserProfile
publicProfile u = (connectedProfile u)
    { profileLocale = Nothing
    }

-- | The data of an existing user.
data User = User
    { userId       :: !UserId
    , userIdentity :: !(Maybe UserIdentity)
    , userName     :: !Name
    , userPict     :: !Pict -- ^ DEPRECATED
    , userAssets   :: [Asset]
    , userAccentId :: !ColourId
    , userDeleted  :: !Bool
    , userLocale   :: !Locale
    , userService  :: !(Maybe ServiceRef)
        -- ^ Set if the user represents an external service,
        -- i.e. it is a "bot".
    , userHandle   :: !(Maybe Handle)
    }

userEmail :: User -> Maybe Email
userEmail = emailIdentity <=< userIdentity

userPhone :: User -> Maybe Phone
userPhone = phoneIdentity <=< userIdentity

-- | A subset of the data of an existing 'User'
-- that is returned on the API.
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
    }

instance ToJSON User where
    toJSON u = object
        $ "id"        .= userId u
        # "name"      .= userName u
        # "picture"   .= userPict u
        # "assets"    .= userAssets u
        # "email"     .= userEmail u
        # "phone"     .= userPhone u
        # "accent_id" .= userAccentId u
        # "deleted"   .= (if userDeleted u then Just True else Nothing)
        # "locale"    .= userLocale u
        # "service"   .= userService u
        # "handle"    .= userHandle u
        # []

instance FromJSON User where
    parseJSON = withObject "user" $ \o ->
        User <$> o .:  "id"
             <*> parseIdentity o
             <*> o .:  "name"
             <*> o .:? "picture" .!= noPict
             <*> o .:? "assets"  .!= []
             <*> o .:  "accent_id"
             <*> o .:? "deleted" .!= False
             <*> o .:  "locale"
             <*> o .:? "service"
             <*> o .:? "handle"

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

instance ToJSON UserProfile where
    toJSON u = object
        $ "id"        .= profileId u
        # "name"      .= profileName u
        # "picture"   .= profilePict u
        # "assets"    .= profileAssets u
        # "accent_id" .= profileAccentId u
        # "deleted"   .= (if profileDeleted u then Just True else Nothing)
        # "service"   .= profileService u
        # "handle"    .= profileHandle u
        # "locale"    .= profileLocale u
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
    , newUserIdentity       :: !(Maybe UserIdentity)
    , newUserPict           :: !(Maybe Pict) -- ^ DEPRECATED
    , newUserAssets         :: [Asset]
    , newUserAccentId       :: !(Maybe ColourId)
    , newUserPhoneCode      :: !(Maybe ActivationCode)
    , newUserInvitationCode :: !(Maybe InvitationCode)
    , newUserLabel          :: !(Maybe CookieLabel)
    , newUserLocale         :: !(Maybe Locale)
    , newUserPassword       :: !(Maybe PlainTextPassword)
    }

newUserEmail :: NewUser -> Maybe Email
newUserEmail = emailIdentity <=< newUserIdentity

newUserPhone :: NewUser -> Maybe Phone
newUserPhone = phoneIdentity <=< newUserIdentity

instance FromJSON NewUser where
    parseJSON = withObject "new-user" $ \o ->
        NewUser <$> o .:  "name"
                <*> parseIdentity o
                <*> o .:? "picture"
                <*> o .:? "assets" .!= []
                <*> o .:? "accent_id"
                <*> o .:? "phone_code"
                <*> o .:? "invitation_code"
                <*> o .:? "label"
                <*> o .:? "locale"
                <*> o .:? "password"

instance ToJSON NewUser where
    toJSON u = object
        $ "name"            .= newUserName u
        # "email"           .= newUserEmail u
        # "password"        .= newUserPassword u
        # "picture"         .= newUserPict u
        # "assets"          .= newUserAssets u
        # "phone"           .= newUserPhone u
        # "phone_code"      .= newUserPhoneCode u
        # "accent_id"       .= newUserAccentId u
        # "invitation_code" .= newUserInvitationCode u
        # "label"           .= newUserLabel u
        # "locale"          .= newUserLocale u
        # "password"        .= newUserPassword u
        # []

parseIdentity :: FromJSON a => Object -> Parser (Maybe a)
parseIdentity o = if isJust (HashMap.lookup "email" o <|> HashMap.lookup "phone" o)
    then Just <$> parseJSON (Object o)
    else pure Nothing

-- | A random invitation code for use during registration
newtype InvitationCode = InvitationCode
    { fromInvitationCode :: AsciiBase64Url }
    deriving (Eq, Show, FromJSON, ToJSON, ToByteString, FromByteString)

-----------------------------------------------------------------------------
-- Profile Updates

data UserUpdate = UserUpdate
    { uupName     :: !(Maybe Name)
    , uupPict     :: !(Maybe Pict) -- DEPRECATED
    , uupAssets   :: !(Maybe [Asset])
    , uupAccentId :: !(Maybe ColourId)
    } deriving Eq

newtype LocaleUpdate = LocaleUpdate { luLocale :: Locale } deriving Eq

newtype EmailUpdate  = EmailUpdate  { euEmail  :: Email  }
newtype PhoneUpdate  = PhoneUpdate  { puPhone  :: Phone  }
newtype HandleUpdate = HandleUpdate { huHandle :: Text   }
newtype EmailRemove  = EmailRemove  { erEmail  :: Email  }
newtype PhoneRemove  = PhoneRemove  { prPhone  :: Phone  }

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

mkDeleteUser :: Maybe PlainTextPassword -> DeleteUser
mkDeleteUser = DeleteUser

-- | Payload for verifying account deletion via a code.
data VerifyDeleteUser = VerifyDeleteUser
    { verifyDeleteUserKey  :: !Code.Key
    , verifyDeleteUserCode :: !Code.Value
    } deriving Eq

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

-- | Opaque identifier per user (SHA256 of the user ID).
newtype PasswordResetKey = PasswordResetKey
    { fromPasswordResetKey :: AsciiBase64Url }
    deriving (Eq, Show, FromByteString, ToByteString, FromJSON, ToJSON)

-- | Random code, acting as a very short-lived, single-use password.
newtype PasswordResetCode = PasswordResetCode
    { fromPasswordResetCode :: AsciiBase64Url }
    deriving (Eq, FromByteString, ToByteString, FromJSON, ToJSON)

type PasswordResetPair = (PasswordResetKey, PasswordResetCode)

-- | The target identity of a password reset.
data PasswordResetIdentity
    = PasswordResetIdentityKey !PasswordResetKey
        -- ^ An opaque identity key for a pending password reset.
    | PasswordResetEmailIdentity !Email
        -- ^ A known email address with a pending password reset.
    | PasswordResetPhoneIdentity !Phone
        -- ^ A known phone number with a pending password reset.

-- | The payload for completing a password reset.
data CompletePasswordReset = CompletePasswordReset
    { cpwrIdent    :: !PasswordResetIdentity
    , cpwrCode     :: !PasswordResetCode
    , cpwrPassword :: !PlainTextPassword
    }

-- | The payload for setting or changing a password.
data PasswordChange = PasswordChange
    { cpOldPassword :: !(Maybe PlainTextPassword)
    , cpNewPassword :: !PlainTextPassword
    }

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
