{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

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

module Brig.Types.User
  ( module Brig.Types.User,
    module C,
  )
where

import Brig.Types.Activation (ActivationCode)
import qualified Brig.Types.Code as Code
import Brig.Types.Common as C
import Brig.Types.User.Auth (CookieLabel)
import Control.Monad.Fail (MonadFail)
import Data.Aeson
import qualified Data.Aeson.Types as Aeson
import Data.ByteString.Conversion
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import qualified Data.Currency as Currency
import Data.Handle (Handle)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.Id
import Data.Json.Util ((#), UTCTimeMillis)
import qualified Data.Map as Map
import Data.Misc (PlainTextPassword (..))
import Data.Range
import qualified Data.Text as Text
import Data.Text.Ascii
import Data.UUID (UUID)
import Galley.Types.Bot (ServiceRef)
import Galley.Types.Teams hiding (userId)
import Imports

-----------------------------------------------------------------------------
-- User Attributes

-- DEPRECATED
newtype Pict = Pict {fromPict :: [Object]}
  deriving (Eq, Show, ToJSON, Generic)

instance FromJSON Pict where
  parseJSON x = Pict . fromRange @0 @10 <$> parseJSON x

noPict :: Pict
noPict = Pict []

--------------------------------------------------------------------------------
-- UserHandleInfo

newtype UserHandleInfo = UserHandleInfo {userHandleId :: UserId}
  deriving (Eq, Show, Generic)

instance ToJSON UserHandleInfo where
  toJSON (UserHandleInfo u) =
    object
      ["user" .= u]

instance FromJSON UserHandleInfo where
  parseJSON = withObject "UserHandleInfo" $ \o ->
    UserHandleInfo <$> o .: "user"

--------------------------------------------------------------------------------
-- CheckHandles

-- | Check the availability of user handles.
data CheckHandles = CheckHandles
  { -- | Handles to check for availability, in ascending order of preference.
    checkHandlesList :: Range 1 50 [Text],
    -- | Number of free handles to return. Default 1.
    checkHandlesNum :: Range 1 10 Word
  }
  deriving (Eq, Show, Generic)

instance ToJSON CheckHandles where
  toJSON (CheckHandles l n) =
    object
      [ "handles" .= l,
        "return" .= n
      ]

instance FromJSON CheckHandles where
  parseJSON = withObject "CheckHandles" $ \o ->
    CheckHandles <$> o .: "handles"
      <*> o .:? "return" .!= unsafeRange 1

-----------------------------------------------------------------------------
-- User Profiles

-- | A self profile.
data SelfProfile = SelfProfile
  {selfUser :: !User}
  deriving (Eq, Show, Generic)

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

-- | The data of an existing user.
data User = User
  { userId :: !UserId,
    -- | User identity. For endpoints like @/self@, it will be present in the response iff
    -- the user is activated, and the email/phone contained in it will be guaranteedly
    -- verified. {#RefActivation}
    userIdentity :: !(Maybe UserIdentity),
    -- | required; non-unique
    userDisplayName :: !Name,
    -- | DEPRECATED
    userPict :: !Pict,
    userAssets :: [Asset],
    userAccentId :: !ColourId,
    userDeleted :: !Bool,
    userLocale :: !Locale,
    -- | Set if the user represents an external service,
    -- i.e. it is a "bot".
    userService :: !(Maybe ServiceRef),
    -- | not required; must be unique if present
    userHandle :: !(Maybe Handle),
    -- | Set if the user is ephemeral
    userExpire :: !(Maybe UTCTimeMillis),
    -- | Set if the user is part of a binding team
    userTeam :: !(Maybe TeamId),
    -- | How is the user profile managed (e.g. if it's via SCIM then the user profile
    -- can't be edited via normal means)
    userManagedBy :: !ManagedBy
  }
  deriving (Eq, Show, Generic)

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
  { profileId :: !UserId,
    profileName :: !Name,
    -- | DEPRECATED
    profilePict :: !Pict,
    profileAssets :: [Asset],
    profileAccentId :: !ColourId,
    profileDeleted :: !Bool,
    -- | Set if the user represents an external service,
    -- i.e. it is a "bot".
    profileService :: !(Maybe ServiceRef),
    profileHandle :: !(Maybe Handle),
    profileLocale :: !(Maybe Locale),
    profileExpire :: !(Maybe UTCTimeMillis),
    profileTeam :: !(Maybe TeamId),
    profileEmail :: !(Maybe Email)
  }
  deriving (Eq, Show, Generic)

-- TODO: disentangle json serializations for 'User', 'NewUser', 'UserIdentity', 'NewUserOrigin'.
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
    User <$> o .: "id"
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

instance FromJSON UserProfile where
  parseJSON = withObject "UserProfile" $ \o ->
    UserProfile <$> o .: "id"
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

instance FromJSON SelfProfile where
  parseJSON = withObject "SelfProfile" $ \o ->
    SelfProfile <$> parseJSON (Object o)

instance ToJSON SelfProfile where
  toJSON (SelfProfile u) = toJSON u

----------------------------------------------------------------------------
-- Rich info
data RichInfo = RichInfo
  { richInfoMap :: Map (CI Text) Text,
    richInfoAssocList :: [RichField]
  }
  deriving (Eq, Show, Generic)

newtype RichInfoAssocList = RichInfoAssocList [RichField]
  deriving (Eq, Show, Generic)

toRichInfoAssocList :: RichInfo -> RichInfoAssocList
toRichInfoAssocList (RichInfo mp al) = RichInfoAssocList . nubrf $ toal mp <> al
  where
    nubrf :: [RichField] -> [RichField]
    nubrf = nubBy ((==) `on` \(RichField k _) -> CI.mk k)
    toal :: Map (CI Text) Text -> [RichField]
    toal = map (\(k, v) -> RichField k v) . Map.toAscList

richInfoMapURN, richInfoAssocListURN :: Text
richInfoMapURN = "urn:ietf:params:scim:schemas:extension:wire:1.0:User"
richInfoAssocListURN = "urn:wire:scim:schemas:profile:1.0"

instance FromJSON RichInfo where
  parseJSON =
    withObject "RichInfo" $
      \o ->
        let objWithCIKeys = hmMapKeys CI.mk o
         in normalizeRichInfo
              <$> ( RichInfo
                      <$> extractMap objWithCIKeys
                      <*> extractAssocList objWithCIKeys
                  )
    where
      extractMap :: HashMap (CI Text) Value -> Aeson.Parser (Map (CI Text) Text)
      extractMap o =
        case HM.lookup (CI.mk richInfoMapURN) o of
          Nothing -> pure mempty
          Just innerObj -> do
            Map.mapKeys CI.mk <$> parseJSON innerObj
      extractAssocList :: HashMap (CI Text) Value -> Aeson.Parser [RichField]
      extractAssocList o =
        case HM.lookup (CI.mk richInfoAssocListURN) o of
          Nothing -> pure []
          Just (Object innerObj) -> do
            richInfo <- lookupOrFail "richinfo" $ hmMapKeys CI.mk innerObj
            case richInfo of
              Object richinfoObj -> do
                fields <- richInfoAssocListFromObject richinfoObj
                pure fields
              Array fields -> parseJSON (Array fields)
              v -> Aeson.typeMismatch "Object" v
          Just v -> Aeson.typeMismatch "Object" v
      hmMapKeys :: (Eq k2, Hashable k2) => (k1 -> k2) -> HashMap k1 v -> HashMap k2 v
      hmMapKeys f = HashMap.fromList . (map (\(k, v) -> (f k, v))) . HashMap.toList
      lookupOrFail :: (MonadFail m, Show k, Eq k, Hashable k) => k -> HashMap k v -> m v
      lookupOrFail key theMap = case HM.lookup key theMap of
        Nothing -> fail $ "key '" ++ show key ++ "' not found"
        Just v -> return v

richInfoAssocListFromObject :: Object -> Aeson.Parser [RichField]
richInfoAssocListFromObject richinfoObj = do
  version :: Int <- richinfoObj .: "version"
  when (version /= 0) $ fail $ "unknown version: " <> show version
  fields <- richinfoObj .: "fields"
  checkDuplicates (map richFieldType fields)
  pure fields
  where
    checkDuplicates :: [CI Text] -> Aeson.Parser ()
    checkDuplicates xs =
      case filter ((> 1) . length) . group . sort $ xs of
        [] -> pure ()
        ds -> fail ("duplicate fields: " <> show (map head ds))

instance ToJSON RichInfo where
  toJSON u =
    object
      [ richInfoAssocListURN
          .= object
            [ "richInfo"
                .= object
                  [ "fields" .= richInfoAssocList u,
                    "version" .= (0 :: Int)
                  ]
            ],
        richInfoMapURN .= (Map.mapKeys CI.original $ richInfoMap u)
      ]

instance FromJSON RichInfoAssocList where
  parseJSON v =
    RichInfoAssocList <$> withObject "RichInfoAssocList" richInfoAssocListFromObject v

instance ToJSON RichInfoAssocList where
  toJSON (RichInfoAssocList l) =
    object
      [ "fields" .= l,
        "version" .= (0 :: Int)
      ]

-- TODO: Make richFieldType @CI Text@
data RichField = RichField
  { richFieldType :: !(CI Text),
    richFieldValue :: !Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON RichField where
  -- NB: "name" would be a better name for 'richFieldType', but "type" is used because we
  -- also have "type" in SCIM; and the reason we use "type" for SCIM is that @{"type": ...,
  -- "value": ...}@ is how all other SCIM payloads are formatted, so it's quite possible
  -- that some provisioning agent would support "type" but not "name".
  toJSON u =
    object
      [ "type" .= CI.original (richFieldType u),
        "value" .= richFieldValue u
      ]

instance FromJSON RichField where
  parseJSON = withObject "RichField" $ \o -> do
    RichField
      <$> (CI.mk <$> o .: "type")
      <*> o .: "value"

-- | Calculate the length of user-supplied data in 'RichInfo'. Used for enforcing
-- 'setRichInfoLimit'
--
-- This works on @[RichField]@ so it can be reused by @RichInfoAssocList@
--
-- NB: we could just calculate the length of JSON-encoded payload, but it is fragile because
-- if our JSON encoding changes, existing payloads might become unacceptable.
richInfoAssocListSize :: [RichField] -> Int
richInfoAssocListSize fields = sum [Text.length (CI.original t) + Text.length v | RichField t v <- fields]

-- | Calculate the length of user-supplied data in 'RichInfo'. Used for enforcing
-- 'setRichInfoLimit'
richInfoMapSize :: RichInfo -> Int
richInfoMapSize rif = sum [Text.length (CI.original k) + Text.length v | (k, v) <- Map.toList $ richInfoMap rif]

-- | Remove fields with @""@ values.
normalizeRichInfo :: RichInfo -> RichInfo
normalizeRichInfo (RichInfo rifMap assocList) =
  RichInfo
    { richInfoAssocList = filter (not . Text.null . richFieldValue) assocList,
      richInfoMap = rifMap
    }

-- | Remove fields with @""@ values.
normalizeRichInfoAssocList :: RichInfoAssocList -> RichInfoAssocList
normalizeRichInfoAssocList (RichInfoAssocList l) =
  RichInfoAssocList $ filter (not . Text.null . richFieldValue) l

emptyRichInfoAssocList :: RichInfoAssocList
emptyRichInfoAssocList = RichInfoAssocList []

-----------------------------------------------------------------------------
-- New Users

data NewUser = NewUser
  { newUserDisplayName :: !Name,
    -- | use this as 'UserId' (if 'Nothing', call 'Data.UUID.nextRandom').
    newUserUUID :: !(Maybe UUID),
    newUserIdentity :: !(Maybe UserIdentity),
    -- | DEPRECATED
    newUserPict :: !(Maybe Pict),
    newUserAssets :: [Asset],
    newUserAccentId :: !(Maybe ColourId),
    newUserEmailCode :: !(Maybe ActivationCode),
    newUserPhoneCode :: !(Maybe ActivationCode),
    newUserOrigin :: !(Maybe NewUserOrigin),
    newUserLabel :: !(Maybe CookieLabel),
    newUserLocale :: !(Maybe Locale),
    newUserPassword :: !(Maybe PlainTextPassword),
    newUserExpiresIn :: !(Maybe ExpiresIn),
    newUserManagedBy :: !(Maybe ManagedBy)
  }
  deriving (Eq, Show, Generic)

-- | 1 second - 1 week
type ExpiresIn = Range 1 604800 Integer

data NewUserOrigin
  = NewUserOriginInvitationCode !InvitationCode
  | NewUserOriginTeamUser !NewTeamUser
  deriving (Eq, Show, Generic)

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

jsonNewUserOrigin :: NewUserOrigin -> [Aeson.Pair]
jsonNewUserOrigin = \case
  NewUserOriginInvitationCode inv -> ["invitation_code" .= inv]
  NewUserOriginTeamUser (NewTeamMember tc) -> ["team_code" .= tc]
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

-- | Fails if email or phone or ssoid are present but invalid
parseIdentity :: Maybe UserSSOId -> Object -> Aeson.Parser (Maybe UserIdentity)
parseIdentity ssoid o =
  if isJust (HashMap.lookup "email" o <|> HashMap.lookup "phone" o) || isJust ssoid
    then Just <$> parseJSON (Object o)
    else pure Nothing

-- | A random invitation code for use during registration
newtype InvitationCode = InvitationCode
  {fromInvitationCode :: AsciiBase64Url}
  deriving (Eq, Show, FromJSON, ToJSON, ToByteString, FromByteString, Generic)

data BindingNewTeamUser = BindingNewTeamUser
  { bnuTeam :: !BindingNewTeam,
    bnuCurrency :: !(Maybe Currency.Alpha)
    -- TODO: Remove Currency selection once billing supports currency changes after team creation
  }
  deriving (Eq, Show, Generic)

instance FromJSON BindingNewTeamUser where
  parseJSON j@(Object o) = do
    c <- o .:? "currency"
    t <- parseJSON j
    return $ BindingNewTeamUser t c
  parseJSON _ = fail "parseJSON BindingNewTeamUser: must be an object"

instance ToJSON BindingNewTeamUser where
  toJSON (BindingNewTeamUser t c) =
    let (Object t') = toJSON t
     in object $
          "currency" .= c
            # HashMap.toList t'

data NewTeamUser
  = -- | requires email address
    NewTeamMember !InvitationCode
  | NewTeamCreator !BindingNewTeamUser
  | NewTeamMemberSSO !TeamId
  deriving (Eq, Show, Generic)

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
  deriving (Eq, Show, Generic)

instance FromJSON NewUserPublic where
  parseJSON val = do
    nu <- parseJSON val
    when (isJust $ newUserSSOId nu) $
      fail "SSO-managed users are not allowed here."
    when (isJust $ newUserUUID nu) $
      fail "it is not allowed to provide a UUID for the users here."
    when (newUserManagedBy nu `notElem` [Nothing, Just ManagedByWire]) $
      fail "only managed-by-Wire users can be created here."
    pure $ NewUserPublic nu

-----------------------------------------------------------------------------
-- Profile Updates

data UserUpdate = UserUpdate
  { uupName :: !(Maybe Name),
    uupPict :: !(Maybe Pict), -- DEPRECATED
    uupAssets :: !(Maybe [Asset]),
    uupAccentId :: !(Maybe ColourId)
  }
  deriving (Eq, Show, Generic)

newtype LocaleUpdate = LocaleUpdate {luLocale :: Locale} deriving (Eq, Show, Generic)

newtype EmailUpdate = EmailUpdate {euEmail :: Email} deriving (Eq, Show, Generic)

newtype PhoneUpdate = PhoneUpdate {puPhone :: Phone} deriving (Eq, Show, Generic)

newtype HandleUpdate = HandleUpdate {huHandle :: Text} deriving (Eq, Show, Generic)

newtype ManagedByUpdate = ManagedByUpdate {mbuManagedBy :: ManagedBy} deriving (Eq, Show, Generic)

newtype RichInfoUpdate = RichInfoUpdate {riuRichInfo :: RichInfoAssocList} deriving (Eq, Show, Generic)

newtype EmailRemove = EmailRemove {erEmail :: Email} deriving (Eq, Show, Generic)

newtype PhoneRemove = PhoneRemove {prPhone :: Phone} deriving (Eq, Show, Generic)

-- NB: when adding new types, please also add roundtrip tests to
-- 'Test.Brig.Types.User.roundtripTests'

instance FromJSON UserUpdate where
  parseJSON = withObject "UserUpdate" $ \o ->
    UserUpdate <$> o .:? "name"
      <*> o .:? "picture"
      <*> o .:? "assets"
      <*> o .:? "accent_id"

instance ToJSON UserUpdate where
  toJSON u =
    object $
      "name" .= uupName u
        # "picture" .= uupPict u
        # "assets" .= uupAssets u
        # "accent_id" .= uupAccentId u
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

instance FromJSON ManagedByUpdate where
  parseJSON = withObject "managed-by-update" $ \o ->
    ManagedByUpdate <$> o .: "managed_by"

instance ToJSON ManagedByUpdate where
  toJSON m = object ["managed_by" .= mbuManagedBy m]

instance FromJSON RichInfoUpdate where
  parseJSON = withObject "rich-info-update" $ \o ->
    RichInfoUpdate <$> o .: "rich_info"

instance ToJSON RichInfoUpdate where
  toJSON (RichInfoUpdate rif) = object ["rich_info" .= rif]

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
  deriving (Eq, Show, Generic)

mkDeleteUser :: Maybe PlainTextPassword -> DeleteUser
mkDeleteUser = DeleteUser

-- | Payload for verifying account deletion via a code.
data VerifyDeleteUser = VerifyDeleteUser
  { verifyDeleteUserKey :: !Code.Key,
    verifyDeleteUserCode :: !Code.Value
  }
  deriving (Eq, Show, Generic)

mkVerifyDeleteUser :: Code.Key -> Code.Value -> VerifyDeleteUser
mkVerifyDeleteUser = VerifyDeleteUser

-- | A response for a pending deletion code.
newtype DeletionCodeTimeout = DeletionCodeTimeout
  {fromDeletionCodeTimeout :: Code.Timeout}
  deriving (Eq, Show, Generic)

instance ToJSON DeleteUser where
  toJSON d =
    object $
      "password" .= deleteUserPassword d
        # []

instance FromJSON DeleteUser where
  parseJSON = withObject "DeleteUser" $ \o ->
    DeleteUser <$> o .:? "password"

instance ToJSON VerifyDeleteUser where
  toJSON d =
    object
      [ "key" .= verifyDeleteUserKey d,
        "code" .= verifyDeleteUserCode d
      ]

instance FromJSON VerifyDeleteUser where
  parseJSON = withObject "VerifyDeleteUser" $ \o ->
    VerifyDeleteUser <$> o .: "key"
      <*> o .: "code"

instance FromJSON DeletionCodeTimeout where
  parseJSON = withObject "DeletionCodeTimeout" $ \o ->
    DeletionCodeTimeout <$> o .: "expires_in"

instance ToJSON DeletionCodeTimeout where
  toJSON (DeletionCodeTimeout t) = object ["expires_in" .= t]

-----------------------------------------------------------------------------
-- Password Change / Reset

-- | The payload for initiating a password reset.
newtype NewPasswordReset = NewPasswordReset (Either Email Phone)
  deriving (Eq, Show, Generic)

-- | Opaque identifier per user (SHA256 of the user ID).
newtype PasswordResetKey = PasswordResetKey
  {fromPasswordResetKey :: AsciiBase64Url}
  deriving (Eq, Show, FromByteString, ToByteString, FromJSON, ToJSON, Generic)

-- | Random code, acting as a very short-lived, single-use password.
newtype PasswordResetCode = PasswordResetCode
  {fromPasswordResetCode :: AsciiBase64Url}
  deriving (Eq, Show, FromByteString, ToByteString, FromJSON, ToJSON, Generic)

type PasswordResetPair = (PasswordResetKey, PasswordResetCode)

-- | The target identity of a password reset.
data PasswordResetIdentity
  = -- | An opaque identity key for a pending password reset.
    PasswordResetIdentityKey !PasswordResetKey
  | -- | A known email address with a pending password reset.
    PasswordResetEmailIdentity !Email
  | -- | A known phone number with a pending password reset.
    PasswordResetPhoneIdentity !Phone
  deriving (Eq, Show, Generic)

-- | The payload for completing a password reset.
data CompletePasswordReset = CompletePasswordReset
  { cpwrIdent :: !PasswordResetIdentity,
    cpwrCode :: !PasswordResetCode,
    cpwrPassword :: !PlainTextPassword
  }
  deriving (Eq, Show, Generic)

-- | The payload for setting or changing a password.
--
-- FUTUREWORK: is this actually used?
data PasswordChange = PasswordChange
  { cpOldPassword :: !(Maybe PlainTextPassword),
    cpNewPassword :: !PlainTextPassword
  }
  deriving (Eq, Show, Generic)

instance FromJSON NewPasswordReset where
  parseJSON = withObject "NewPasswordReset" $ \o ->
    NewPasswordReset
      <$> ( (Left <$> o .: "email")
              <|> (Right <$> o .: "phone")
          )

instance ToJSON NewPasswordReset where
  toJSON (NewPasswordReset ident) =
    object
      [either ("email" .=) ("phone" .=) ident]

instance FromJSON CompletePasswordReset where
  parseJSON = withObject "CompletePasswordReset" $ \o ->
    CompletePasswordReset <$> ident o <*> o .: "code" <*> o .: "password"
    where
      ident o =
        (PasswordResetIdentityKey <$> o .: "key")
          <|> (PasswordResetEmailIdentity <$> o .: "email")
          <|> (PasswordResetPhoneIdentity <$> o .: "phone")

instance ToJSON CompletePasswordReset where
  toJSON (CompletePasswordReset i c pw) =
    object
      [ident i, "code" .= c, "password" .= pw]
    where
      ident (PasswordResetIdentityKey k) = "key" .= k
      ident (PasswordResetEmailIdentity e) = "email" .= e
      ident (PasswordResetPhoneIdentity p) = "phone" .= p

instance ToJSON PasswordChange where
  toJSON (PasswordChange old new) =
    object
      [ "old_password" .= old,
        "new_password" .= new
      ]

instance FromJSON PasswordChange where
  parseJSON = withObject "PasswordChange" $ \o ->
    PasswordChange <$> o .:? "old_password"
      <*> o .: "new_password"

-- DEPRECATED

data PasswordReset = PasswordReset
  { pwrCode :: !PasswordResetCode,
    pwrPassword :: !PlainTextPassword
  }

instance FromJSON PasswordReset where
  parseJSON = withObject "PasswordReset" $ \o ->
    PasswordReset <$> o .: "code"
      <*> o .: "password"
