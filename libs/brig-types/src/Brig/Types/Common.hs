{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ViewPatterns               #-}

-- Brig.Types.Account?
module Brig.Types.Common where

import Control.Applicative
import Control.Error (hush, readMay)
import Data.Aeson
import Data.Attoparsec.Text
import Data.ByteString.Conversion
import Data.Char (isLower, toUpper, isUpper)
import Data.Hashable (Hashable)
import Data.Int (Int32)
import Data.Json.Util ((#))
import Data.ISO3166_CountryCodes
import Data.LanguageCodes
import Data.Monoid ((<>))
import Data.Range
import Data.Text (Text, toLower)
import Data.Time.Clock

import qualified Data.Aeson.Types as Json
import qualified Data.Text        as Text

--------------------------------------------------------------------------------
-- Handle

newtype Handle = Handle
    { fromHandle :: Text }
    deriving (Eq, Show, ToJSON, ToByteString, Hashable)

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
          *> count 19 (optional (satisfy chars))
          *> endOfInput
    chars  = inClass "a-z0-9_"

--------------------------------------------------------------------------------
-- Name

newtype Name = Name
    { fromName :: Text }
    deriving (Eq, Ord, Show, ToJSON, FromByteString, ToByteString)

instance FromJSON Name where
    parseJSON x = Name . fromRange
               <$> (parseJSON x :: Json.Parser (Range 1 128 Text))

--------------------------------------------------------------------------------
-- Colour

newtype ColourId = ColourId { fromColourId :: Int32 }
    deriving (Eq, Num, Ord, Show, FromJSON, ToJSON)

defaultAccentId :: ColourId
defaultAccentId = ColourId 0

-----------------------------------------------------------------------------
-- Email

data Email = Email
    { emailLocal  :: !Text
    , emailDomain :: !Text
    } deriving (Eq, Ord)

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
    [local, domain] -> Just $! Email local domain
    _               -> Nothing

-----------------------------------------------------------------------------
-- Phone

newtype Phone = Phone { fromPhone :: Text } deriving (Eq, Show, ToJSON)

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
    deriving (Eq, Show)

instance FromJSON PhoneBudgetTimeout where
    parseJSON = withObject "PhoneBudgetTimeout" $ \o ->
        PhoneBudgetTimeout <$> o .: "expires_in"

instance ToJSON PhoneBudgetTimeout where
    toJSON (PhoneBudgetTimeout t) = object [ "expires_in" .= t ]

-----------------------------------------------------------------------------
-- UserIdentity

-- | The private unique user identity that is used for login and
-- account recovery.
data UserIdentity
    = FullIdentity  !Email !Phone
    | EmailIdentity !Email
    | PhoneIdentity        !Phone
    | SSOIdentity !UserSSOId !(Maybe Email) !(Maybe Phone)
    deriving (Eq, Show)

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

-- | TODO: once we have @/libs/spar-types@ for the wire-sso-sp-server called spar, this type should
-- move there.
data UserSSOId = UserSSOId { userSSOIdTenant :: Text, userSSOIdSubject :: Text }
    deriving (Eq, Show)

instance FromJSON UserSSOId where
    parseJSON = withObject "UserSSOId" $ \obj -> UserSSOId
        <$> obj .: "tenant"
        <*> obj .: "subject"

instance ToJSON UserSSOId where
    toJSON (UserSSOId tenant subject) = object ["tenant" .= tenant, "subject" .= subject]

-----------------------------------------------------------------------------
-- Asset

data AssetSize = AssetComplete | AssetPreview
    deriving (Eq, Show, Enum, Bounded)

-- Note: Intended to be turned into a sum type to add further asset types.
data Asset = ImageAsset
    { assetKey  :: !Text
    , assetSize :: !(Maybe AssetSize)
    } deriving (Eq, Show)

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

newtype Language = Language { fromLanguage :: ISO639_1 } deriving (Ord, Eq)

languageParser :: Parser Language
languageParser = codeParser "language" $ fmap Language . checkAndConvert isLower

lan2Text :: Language -> Text
lan2Text = toLower . Text.pack . show . fromLanguage

parseLanguage :: Text -> Maybe Language
parseLanguage = hush . parseOnly languageParser

-----------------------------------------------------------------------------
-- Country

newtype Country = Country { fromCountry :: CountryCode } deriving (Ord, Eq)

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
    } deriving (Eq, Ord)

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
    then readMay (map toUpper t)
    else fail "Format not supported."

codeParser :: String -> (String -> Maybe a) -> Parser a
codeParser err conv = do
    code <- count 2 anyChar
    maybe (fail err) return (conv code)
