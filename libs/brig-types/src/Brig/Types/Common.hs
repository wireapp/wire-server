{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ViewPatterns               #-}

-- Brig.Types.Account?
module Brig.Types.Common where

import Imports
import Control.Applicative (optional)
import Control.Error (hush)
import Data.Aeson
import Data.Attoparsec.Text
import Data.ByteString.Conversion
import Data.Hashable (Hashable)
import Data.Json.Util ((#))
import Data.ISO3166_CountryCodes
import Data.LanguageCodes
import Data.Range
import Data.Time.Clock

import qualified Data.Aeson.Types as Json
import qualified Data.Text        as Text

--------------------------------------------------------------------------------
-- Handle

newtype Handle = Handle
    { fromHandle :: Text }
    deriving (Eq, Show, ToJSON, ToByteString, Hashable, Generic)

instance FromByteString Handle where
    parser = parser >>= maybe (fail "Invalid handle") return . parseHandle

instance FromJSON Handle where
    parseJSON = withText "Handle" $
        maybe (fail "Invalid handle") pure . parseHandle

parseHandle :: Text -> Maybe Handle
parseHandle t
    | isValidHandle t = Just (Handle t)
    | otherwise       = Nothing

isValidHandle :: Text -> Bool
isValidHandle t = either (const False) (const True)
                $ parseOnly handle t
  where
    handle = count 2 (satisfy chars)
          *> count 254 (optional (satisfy chars))
          *> endOfInput
    -- NOTE: Ensure that characters such as `@` and `+` should _NOT_
    -- be used so that "phone numbers", "emails", and "handles" remain
    -- disjoint sets.
    -- The rationale behind max size here relates to the max length of
    -- an email address as defined here:
    -- http://www.rfc-editor.org/errata_search.php?rfc=3696&eid=1690
    -- with the intent that in the enterprise world handle =~ email address
    chars = inClass "a-z0-9_.-"

--------------------------------------------------------------------------------
-- Name

newtype Name = Name
    { fromName :: Text }
    deriving (Eq, Ord, Show, ToJSON, FromByteString, ToByteString, Generic)

instance FromJSON Name where
    parseJSON x = Name . fromRange
               <$> (parseJSON x :: Json.Parser (Range 1 128 Text))

--------------------------------------------------------------------------------
-- Colour

newtype ColourId = ColourId { fromColourId :: Int32 }
    deriving (Eq, Num, Ord, Show, FromJSON, ToJSON, Generic)

defaultAccentId :: ColourId
defaultAccentId = ColourId 0

-----------------------------------------------------------------------------
-- Email

data Email = Email
    { emailLocal  :: !Text
    , emailDomain :: !Text
    } deriving (Eq, Ord, Generic)

instance Show Email where
    show = Text.unpack . fromEmail

instance FromByteString Email where
    parser = parser >>= maybe (fail "Invalid email") return . parseEmail

instance ToByteString Email where
    builder = builder . fromEmail

instance FromJSON Email where
    parseJSON = withText "email" $
          maybe (fail "Invalid email. Expected '<local>@<domain>'.") return
        . parseEmail

instance ToJSON Email where
    toJSON = String . fromEmail

fromEmail :: Email -> Text
fromEmail (Email loc dom) = loc <> "@" <> dom

-- | Parses an email address of the form <local-part>@<domain>.
parseEmail :: Text -> Maybe Email
parseEmail t = case Text.split (=='@') t of
    [localPart, domain] -> Just $! Email localPart domain
    _                   -> Nothing

-----------------------------------------------------------------------------
-- Phone

newtype Phone = Phone { fromPhone :: Text } deriving (Eq, Show, ToJSON, Generic)

-- | Parses a phone number in E.164 format with a mandatory leading '+'.
parsePhone :: Text -> Maybe Phone
parsePhone p
    | isValidPhone p = Just $! Phone p
    | otherwise      = Nothing

-- | Checks whether a phone number is valid, i.e. it is in E.164 format
-- with a mandatory leading '+' followed by 10-15 digits.
isValidPhone :: Text -> Bool
isValidPhone = either (const False) (const True) . parseOnly e164
  where
    e164 = char '+' *> count 8 digit *> count 7 (optional digit) *> endOfInput

instance FromJSON Phone where
    parseJSON (String s) = case parsePhone s of
        Just p  -> return p
        Nothing -> fail "Invalid phone number. Expected E.164 format."
    parseJSON _          = mempty

instance FromByteString Phone where
    parser = parser >>= maybe (fail "Invalid phone") return . parsePhone

instance ToByteString Phone where
    builder = builder . fromPhone

-- | If the budget for SMS and voice calls for a phone number
-- has been exhausted within a certain time frame, this timeout
-- indicates in seconds when another attempt may be made.
newtype PhoneBudgetTimeout = PhoneBudgetTimeout
    { phoneBudgetTimeout :: NominalDiffTime }
    deriving (Eq, Show, Generic)

instance FromJSON PhoneBudgetTimeout where
    parseJSON = withObject "PhoneBudgetTimeout" $ \o ->
        PhoneBudgetTimeout <$> o .: "expires_in"

instance ToJSON PhoneBudgetTimeout where
    toJSON (PhoneBudgetTimeout t) = object [ "expires_in" .= t ]

-----------------------------------------------------------------------------
-- PhonePrefix (for excluding from SMS/calling)

newtype PhonePrefix = PhonePrefix { fromPhonePrefix :: Text } deriving (Eq, Show, ToJSON, Generic)

-- | Parses a phone number prefix with a mandatory leading '+'.
parsePhonePrefix :: Text -> Maybe PhonePrefix
parsePhonePrefix p
    | isValidPhonePrefix p  = Just $ PhonePrefix p
    | otherwise             = Nothing

-- | Checks whether a phone number prefix is valid,
-- i.e. it is like a E.164 format phone number, but shorter
-- (with a mandatory leading '+', followed by 1-15 digits.)
isValidPhonePrefix :: Text -> Bool
isValidPhonePrefix = isRight . parseOnly e164Prefix
  where
    e164Prefix = char '+' *> count 1 digit *> count 14 (optional digit) *> endOfInput

-- | get all valid prefixes of a phone number or phone number prefix
-- e.g. from +123456789 get prefixes ["+1", "+12", "+123", ..., "+123456789" ]
allPrefixes :: Text -> [PhonePrefix]
allPrefixes t = catMaybes $ parsePhonePrefix <$> Text.inits t

instance FromJSON PhonePrefix where
    parseJSON = withText "PhonePrefix" $ \s ->
        case parsePhonePrefix s of
            Just p  -> return p
            Nothing -> fail $ "Invalid phone number prefix: [" ++ show s
                            ++ "]. Expected format similar to E.164 (with 1-15 digits after the +)."

instance FromByteString PhonePrefix where
    parser = parser >>= maybe (fail "Invalid phone") return . parsePhonePrefix

instance ToByteString PhonePrefix where
    builder = builder . fromPhonePrefix

data ExcludedPrefix = ExcludedPrefix { phonePrefix :: PhonePrefix
                                     , comment :: Text
                                     } deriving (Eq, Show, Generic)

instance FromJSON ExcludedPrefix where
    parseJSON = withObject "ExcludedPrefix" $ \o -> ExcludedPrefix
        <$> o .: "phone_prefix"
        <*> o .: "comment"

instance ToJSON ExcludedPrefix where
    toJSON (ExcludedPrefix p c) = object ["phone_prefix" .= p, "comment" .= c]

-----------------------------------------------------------------------------
-- UserIdentity

-- | The private unique user identity that is used for login and
-- account recovery.
data UserIdentity
    = FullIdentity  !Email !Phone
    | EmailIdentity !Email
    | PhoneIdentity        !Phone
    | SSOIdentity !UserSSOId !(Maybe Email) !(Maybe Phone)
    deriving (Eq, Show, Generic)

instance FromJSON UserIdentity where
    parseJSON = withObject "UserIdentity" $ \o -> do
        email <- o .:? "email"
        phone <- o .:? "phone"
        ssoid <- o .:? "sso_id"
        maybe (fail "Missing 'email' or 'phone' or 'sso_id'.")
              return
              (newIdentity email phone ssoid)

instance ToJSON UserIdentity where
    toJSON = \case
        FullIdentity  em ph  -> go (Just em) (Just ph) Nothing
        EmailIdentity em     -> go (Just em) Nothing   Nothing
        PhoneIdentity    ph  -> go Nothing   (Just ph) Nothing
        SSOIdentity si em ph -> go em        ph        (Just si)
      where
        go :: Maybe Email -> Maybe Phone -> Maybe UserSSOId -> Value
        go em ph si = object ["email" .= em, "phone" .= ph, "sso_id" .= si]

newIdentity :: Maybe Email -> Maybe Phone -> Maybe UserSSOId -> Maybe UserIdentity
newIdentity email    phone    (Just sso) = Just $! SSOIdentity sso email phone
newIdentity Nothing  Nothing  Nothing    = Nothing
newIdentity (Just e) Nothing  Nothing    = Just $! EmailIdentity e
newIdentity Nothing  (Just p) Nothing    = Just $! PhoneIdentity p
newIdentity (Just e) (Just p) Nothing    = Just $! FullIdentity e p

emailIdentity :: UserIdentity -> Maybe Email
emailIdentity (FullIdentity  email _)        = Just email
emailIdentity (EmailIdentity email  )        = Just email
emailIdentity (PhoneIdentity       _)        = Nothing
emailIdentity (SSOIdentity _ (Just email) _) = Just email
emailIdentity (SSOIdentity _ Nothing _)      = Nothing

phoneIdentity :: UserIdentity -> Maybe Phone
phoneIdentity (FullIdentity  _ phone) = Just phone
phoneIdentity (PhoneIdentity   phone) = Just phone
phoneIdentity (EmailIdentity _      ) = Nothing
phoneIdentity (SSOIdentity _ _ (Just phone)) = Just phone
phoneIdentity (SSOIdentity _ _ Nothing) = Nothing

ssoIdentity :: UserIdentity -> Maybe UserSSOId
ssoIdentity (SSOIdentity ssoid _ _) = Just ssoid
ssoIdentity _ = Nothing

-- | User's external identity.
--
-- Morally this is the same thing as 'SAML.UserRef', but we forget the
-- structure -- i.e. we just store XML-encoded SAML blobs. If the structure
-- of those blobs changes, Brig won't have to deal with it, only Spar will.
--
-- TODO: once we have @/libs/spar-types@ for the wire-sso-sp-server called spar, this type should
-- move there.
data UserSSOId = UserSSOId
    {
    -- | An XML blob pointing to the identity provider that can confirm
    -- user's identity.
      userSSOIdTenant :: Text
    -- | An XML blob specifying the user's ID on the identity provider's side.
    , userSSOIdSubject :: Text
    } deriving (Eq, Show, Generic)

instance FromJSON UserSSOId where
    parseJSON = withObject "UserSSOId" $ \obj -> UserSSOId
        <$> obj .: "tenant"
        <*> obj .: "subject"

instance ToJSON UserSSOId where
    toJSON (UserSSOId tenant subject) = object ["tenant" .= tenant, "subject" .= subject]

-----------------------------------------------------------------------------
-- Asset

data AssetSize = AssetComplete | AssetPreview
    deriving (Eq, Show, Enum, Bounded, Generic)

-- Note: Intended to be turned into a sum type to add further asset types.
data Asset = ImageAsset
    { assetKey  :: !Text
    , assetSize :: !(Maybe AssetSize)
    } deriving (Eq, Show, Generic)

instance FromJSON AssetSize where
    parseJSON = withText "AssetSize" $ \s ->
        case s of
            "preview"  -> pure AssetPreview
            "complete" -> pure AssetComplete
            _          -> fail $ "Invalid asset size: " ++ show s

instance ToJSON AssetSize where
    toJSON AssetPreview  = String "preview"
    toJSON AssetComplete = String "complete"

instance FromJSON Asset where
    parseJSON = withObject "Asset" $ \o -> do
        typ <- o .:  "type"
        key <- o .:  "key"
        siz <- o .:? "size"
        case (typ :: Text) of
            "image" -> pure (ImageAsset key siz)
            _       -> fail $ "Invalid asset type: " ++ show typ

instance ToJSON Asset where
    toJSON (ImageAsset k s) = object
        $ "type" .= ("image" :: Text)
        # "key"  .= k
        # "size" .= s
        # []

-----------------------------------------------------------------------------
-- Language

newtype Language = Language { fromLanguage :: ISO639_1 } deriving (Ord, Eq, Show, Generic)

languageParser :: Parser Language
languageParser = codeParser "language" $ fmap Language . checkAndConvert isLower

lan2Text :: Language -> Text
lan2Text = Text.toLower . Text.pack . show . fromLanguage

parseLanguage :: Text -> Maybe Language
parseLanguage = hush . parseOnly languageParser

-----------------------------------------------------------------------------
-- Country

newtype Country = Country { fromCountry :: CountryCode } deriving (Ord, Eq, Show, Generic)

countryParser :: Parser Country
countryParser = codeParser "country" $ fmap Country . checkAndConvert isUpper

con2Text :: Country -> Text
con2Text = Text.pack . show . fromCountry

parseCountry :: Text -> Maybe Country
parseCountry = hush . parseOnly countryParser

-----------------------------------------------------------------------------
-- Locale

data Locale = Locale
    { lLanguage :: !Language
    , lCountry  :: !(Maybe Country)
    } deriving (Eq, Ord, Generic)

locToText :: Locale -> Text
locToText (Locale l c) = lan2Text l <> maybe mempty (("-"<>) . con2Text) c

instance FromJSON Locale where
    parseJSON = withText "locale"
              $ maybe (fail "Invalid locale. Expected <ISO 639-1>(-<ISO 3166-1-alpha2>)? format") return
              . parseLocale

instance ToJSON Locale where
    toJSON = String . locToText

instance Show Locale where
    show = Text.unpack . locToText

parseLocale :: Text -> Maybe Locale
parseLocale = hush . parseOnly localeParser
  where
    localeParser :: Parser Locale
    localeParser = Locale <$> (languageParser                       <?> "Language code")
                          <*> (optional (char '-' *> countryParser) <?> "Country code" )

-- Common language / country functions
checkAndConvert :: (Read a) => (Char -> Bool) -> String -> Maybe a
checkAndConvert f t = if all f t
    then readMaybe (map toUpper t)
    else fail "Format not supported."

codeParser :: String -> (String -> Maybe a) -> Parser a
codeParser err conv = do
    code <- count 2 anyChar
    maybe (fail err) return (conv code)

-----------------------------------------------------------------------------
-- ManagedBy

-- | Who controls changes to the user profile (where the profile is defined as "all
-- user-editable, user-visible attributes").  See {#DevScimOneWaySync}.
data ManagedBy
      -- | The profile can be changed in-app; user doesn't show up via SCIM at all.
    = ManagedByWire
      -- | The profile can only be changed via SCIM, with several exceptions:
      --
      --   1. User properties can still be set (because they are used internally by clients
      --      and none of them can be modified via SCIM now or in the future).
      --
      --   2. Password can be changed by the user (SCIM doesn't support setting passwords yet,
      --      but currently SCIM only works with SSO-users who don't even have passwords).
      --
      --   3. The user can still be deleted normally (SCIM doesn't support deleting users yet;
      --      but it's questionable whether this should even count as a /change/ of a user
      --      profile).
      --
      -- There are some other things that SCIM can't do yet, like setting accent IDs, but they
      -- are not essential, unlike e.g. passwords.
    | ManagedByScim
    deriving (Eq, Show, Bounded, Enum, Generic)

instance FromJSON ManagedBy where
    parseJSON = withText "ManagedBy" $ \case
        "wire" -> pure ManagedByWire
        "scim" -> pure ManagedByScim
        other  -> fail $ "Invalid ManagedBy: " ++ show other

instance ToJSON ManagedBy where
    toJSON = String . \case
        ManagedByWire -> "wire"
        ManagedByScim -> "scim"

defaultManagedBy :: ManagedBy
defaultManagedBy = ManagedByWire

-- NB: when adding new types, please add a roundtrip test to "Test.Brig.Types.Common"
