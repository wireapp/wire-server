{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Types for the (internal) provider API.
--
-- TODO: Lenses & proper export list
module Brig.Types.Provider
    ( module Brig.Types.Provider

      -- * Re-exports
    , module Common
    , HttpsUrl (..)
    , ServiceToken (..)
    , ServiceTag (..)
    ) where

import Brig.Types.Client.Prekey
import Brig.Types.Code as Code
import Brig.Types.Common as Common
import Brig.Types.Provider.Tag (ServiceTag (..))
import Data.Aeson
import Data.Aeson.TH
import Data.ByteString.Conversion
import Data.Id
import Data.Int
import Data.Json.Util
import Data.List1 (List1)
import Data.Misc (HttpsUrl (..), PlainTextPassword (..))
import Data.PEM
import Data.Range
import Data.Set (Set)
import Data.Singletons.TypeLits
import Data.Text (Text)
import Galley.Types (Event)
import Galley.Types.Bot (ServiceToken (..))

import qualified Brig.Types.Provider.Tag as Tag
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8   as C8
import qualified Data.Range              as Range
import qualified Data.Set                as Set
import qualified Data.Text.Encoding      as Text

--------------------------------------------------------------------------------
-- NewProvider

-- | Input data for registering a new provider.
data NewProvider = NewProvider
    { newProviderName     :: !Name
    , newProviderEmail    :: !Email
    , newProviderUrl      :: !HttpsUrl
    , newProviderDescr    :: !(Range 1 1024 Text)
    , newProviderPassword :: !(Maybe PlainTextPassword)
        -- ^ If none provided, a password is generated.
    }

instance FromJSON NewProvider where
    parseJSON = withObject "NewProvider" $ \o ->
        NewProvider <$> o .:  "name"
                    <*> o .:  "email"
                    <*> o .:  "url"
                    <*> o .:  "description"
                    <*> o .:? "password"

instance ToJSON NewProvider where
    toJSON p = object
        $ "name"        .= newProviderName p
        # "email"       .= newProviderEmail p
        # "url"         .= newProviderUrl p
        # "description" .= newProviderDescr p
        # "password"    .= newProviderPassword p
        # []

-- | Response data upon registering a new provider.
data NewProviderResponse = NewProviderResponse
    { rsNewProviderId       :: !ProviderId
    , rsNewProviderPassword :: !(Maybe PlainTextPassword)
        -- ^ The generated password, if none was provided
        -- in the 'NewProvider' request.
    }

instance FromJSON NewProviderResponse where
    parseJSON = withObject "NewProviderResponse" $ \o ->
        NewProviderResponse <$> o .:  "id"
                            <*> o .:? "password"

instance ToJSON NewProviderResponse where
    toJSON r = object
        $ "id"       .= rsNewProviderId r
        # "password" .= rsNewProviderPassword r
        # []

--------------------------------------------------------------------------------
-- Provider

-- | Full provider definition as seen by a verified provider itself.
data Provider = Provider
    { providerId    :: !ProviderId
    , providerName  :: !Name
    , providerEmail :: !Email
    , providerUrl   :: !HttpsUrl
    , providerDescr :: !Text
    } deriving (Eq, Show)

instance FromJSON Provider where
    parseJSON = withObject "Provider" $ \o ->
        Provider <$> o .: "id"
                 <*> o .: "name"
                 <*> o .: "email"
                 <*> o .: "url"
                 <*> o .: "description"

instance ToJSON Provider where
    toJSON p = object
        $ "id"          .= providerId p
        # "name"        .= providerName p
        # "email"       .= providerEmail p
        # "url"         .= providerUrl p
        # "description" .= providerDescr p
        # []

--------------------------------------------------------------------------------
-- ProviderProfile

-- | A provider profile as seen by regular users.
-- Note: This is a placeholder that may evolve to contain only a subset of
-- the full provider information.
newtype ProviderProfile = ProviderProfile Provider
    deriving (Eq, Show, FromJSON, ToJSON)

--------------------------------------------------------------------------------
-- UpdateProvider

-- | Input data for updating general provider information.
data UpdateProvider = UpdateProvider
    { updateProviderName  :: !(Maybe Name)
    , updateProviderUrl   :: !(Maybe HttpsUrl)
    , updateProviderDescr :: !(Maybe Text)
    } deriving (Eq, Show)

instance FromJSON UpdateProvider where
    parseJSON = withObject "UpdateProvider" $ \o ->
        UpdateProvider <$> o .:? "name"
                       <*> o .:? "url"
                       <*> o .:? "description"

instance ToJSON UpdateProvider where
    toJSON p = object
        $ "name"        .= updateProviderName p
        # "url"         .= updateProviderUrl p
        # "description" .= updateProviderDescr p
        # []

--------------------------------------------------------------------------------
-- ProviderActivationResponse

-- | Successful response upon activating an email address (or possibly phone
-- number in the future) of a provider.
newtype ProviderActivationResponse = ProviderActivationResponse
    { activatedProviderIdentity :: Email }
    deriving (Eq, Show)

instance FromJSON ProviderActivationResponse where
    parseJSON = withObject "ProviderActivationResponse" $ \o ->
        ProviderActivationResponse <$> o .: "email"

instance ToJSON ProviderActivationResponse where
    toJSON (ProviderActivationResponse e) =
        object [ "email" .= e ]

--------------------------------------------------------------------------------
-- ProviderLogin

-- | Input data for a provider login request.
data ProviderLogin = ProviderLogin
    { providerLoginEmail    :: !Email
    , providerLoginPassword :: !PlainTextPassword
    }

instance FromJSON ProviderLogin where
    parseJSON = withObject "ProviderLogin" $ \o ->
        ProviderLogin <$> o .: "email"
                      <*> o .: "password"

instance ToJSON ProviderLogin where
    toJSON l = object
        [ "email"    .= providerLoginEmail l
        , "password" .= providerLoginPassword l
        ]

--------------------------------------------------------------------------------
-- DeleteProvider

-- | Input data for a provider deletion request.
newtype DeleteProvider = DeleteProvider
    { deleteProviderPassword :: PlainTextPassword }

instance FromJSON DeleteProvider where
    parseJSON = withObject "DeleteProvider" $ \o ->
        DeleteProvider <$> o .: "password"

instance ToJSON DeleteProvider where
    toJSON d = object
        [ "password" .= deleteProviderPassword d
        ]

--------------------------------------------------------------------------------
-- Password Change / Reset

-- | The payload for initiating a password reset.
newtype PasswordReset = PasswordReset { nprEmail :: Email }

deriveJSON toJSONFieldName ''PasswordReset

-- | The payload for completing a password reset.
data CompletePasswordReset = CompletePasswordReset
    { cpwrKey      :: !Code.Key
    , cpwrCode     :: !Code.Value
    , cpwrPassword :: !PlainTextPassword
    }

deriveJSON toJSONFieldName ''CompletePasswordReset

-- | The payload for changing a password.
data PasswordChange = PasswordChange
    { cpOldPassword :: !PlainTextPassword
    , cpNewPassword :: !PlainTextPassword
    }

deriveJSON toJSONFieldName ''PasswordChange

-- | The payload for updating an email address
newtype EmailUpdate = EmailUpdate { euEmail :: Email }

deriveJSON toJSONFieldName ''EmailUpdate

--------------------------------------------------------------------------------
-- Bounded ServiceTag Queries

queryAnyTags :: LTE m n => Tag.MatchAny -> Maybe (QueryAnyTags m n)
queryAnyTags t = do
    x <- mapM queryAllTags (Set.toList (Tag.matchAnySet t))
    QueryAnyTags <$> Range.checked (Set.fromList x)

queryAllTags :: LTE m n => Tag.MatchAll -> Maybe (QueryAllTags m n)
queryAllTags = fmap QueryAllTags . Range.checked . Tag.matchAllSet

-- | Bounded logical disjunction of 'm' to 'n' 'QueryAllTags'.
newtype QueryAnyTags (m :: Nat) (n :: Nat) = QueryAnyTags
    { queryAnyTagsRange :: Range m n (Set (QueryAllTags m n)) }
    deriving (Eq, Show, Ord)

-- | QueryAny ::= QueryAll { "," QueryAll }
instance LTE m n => FromByteString (QueryAnyTags m n) where
    parser = do
        bs <- C8.split ',' <$> parser
        ts <- mapM (either fail pure . runParser parser) bs
        rs <- either fail pure (Range.checkedEither (Set.fromList ts))
        return $! QueryAnyTags rs

-- | QueryAny ::= QueryAll { "," QueryAll }
instance ToByteString (QueryAnyTags m n) where
    builder = BB.byteString
            . C8.intercalate ","
            . map toByteString'
            . Set.toList
            . fromRange
            . queryAnyTagsRange

-- | Bounded logical conjunction of 'm' to 'n' 'ServiceTag's to match.
newtype QueryAllTags (m :: Nat) (n :: Nat) = QueryAllTags
    { queryAllTagsRange :: Range m n (Set ServiceTag) }
    deriving (Eq, Show, Ord)

-- | QueryAll ::= tag { "." tag }
instance LTE m n => FromByteString (QueryAllTags m n) where
    parser = do
        bs <- C8.split '.' <$> parser
        ts <- mapM (either fail pure . runParser parser) bs
        rs <- either fail pure (Range.checkedEither (Set.fromList ts))
        return $! QueryAllTags rs

-- | QueryAll ::= tag { "." tag }
instance ToByteString (QueryAllTags m n) where
    builder = BB.byteString
            . C8.intercalate "."
            . map toByteString'
            . Set.toList
            . fromRange
            . queryAllTagsRange

--------------------------------------------------------------------------------
-- ServiceTagList

newtype ServiceTagList = ServiceTagList [ServiceTag]
    deriving (Eq, Show, Ord, FromJSON, ToJSON)

--------------------------------------------------------------------------------
-- ServiceKeyPEM

newtype ServiceKeyPEM = ServiceKeyPEM { unServiceKeyPEM :: PEM }
    deriving (Eq, Show)

instance FromByteString ServiceKeyPEM where
    parser = do
        bs <- parser
        case pemParseBS bs of
            Left   e  -> fail e
            Right [k] -> pure (ServiceKeyPEM k)
            Right  _  -> fail "Too many sections in PEM format. Expected 1."

instance ToByteString ServiceKeyPEM where
    builder = BB.lazyByteString . pemWriteLBS . unServiceKeyPEM

instance FromJSON ServiceKeyPEM where
    parseJSON = withText "ServiceKeyPEM" $
        either fail pure . runParser parser . Text.encodeUtf8

instance ToJSON ServiceKeyPEM where
    toJSON = String . Text.decodeUtf8 . toByteString'

--------------------------------------------------------------------------------
-- ServiceKey

data ServiceKeyType
    = RsaServiceKey
    -- Other types may be supported in the future.
    deriving (Eq, Show)

instance FromJSON ServiceKeyType where
    parseJSON (String "rsa") = pure RsaServiceKey
    parseJSON _              = fail "Invalid service key type. Expected string 'rsa'."

instance ToJSON ServiceKeyType where
    toJSON RsaServiceKey = String "rsa"

-- | A PEM-encoded public key of a service used to verify the
-- identity of the remote peer in every established TLS connection
-- towards the service (i.e. public key pinning to prevent MITM attacks
-- with forged certificates).
data ServiceKey = ServiceKey
    { serviceKeyType :: !ServiceKeyType
    , serviceKeySize :: !Int32
    , serviceKeyPEM  :: !ServiceKeyPEM
    } deriving (Eq, Show)

instance FromJSON ServiceKey where
    parseJSON = withObject "ServiceKey" $ \o ->
        ServiceKey <$> o .: "type"
                   <*> o .: "size"
                   <*> o .: "pem"

instance ToJSON ServiceKey where
    toJSON k = object
        [ "type" .= serviceKeyType k
        , "size" .= serviceKeySize k
        , "pem"  .= serviceKeyPEM k
        ]

--------------------------------------------------------------------------------
-- NewService

-- | Input data for registering a new service.
data NewService = NewService
    { newServiceName    :: !Name
    , newServiceSummary :: !(Range 1 128 Text)
    , newServiceDescr   :: !(Range 1 1024 Text)
    , newServiceUrl     :: !HttpsUrl
    , newServiceKey     :: !ServiceKeyPEM
    , newServiceToken   :: !(Maybe ServiceToken)
    , newServiceAssets  :: [Asset]
    , newServiceTags    :: Range 1 3 (Set ServiceTag)
    }

instance FromJSON NewService where
    parseJSON = withObject "NewService" $ \o ->
        NewService <$> o .:  "name"
                   <*> o .:  "summary"
                   <*> o .:  "description"
                   <*> o .:  "base_url"
                   <*> o .:  "public_key"
                   <*> o .:? "auth_token"
                   <*> o .:? "assets" .!= []
                   <*> o .:  "tags"

instance ToJSON NewService where
    toJSON s = object
        $ "name"        .= newServiceName s
        # "summary"     .= newServiceSummary s
        # "description" .= newServiceDescr s
        # "base_url"    .= newServiceUrl s
        # "public_key"  .= newServiceKey s
        # "auth_token"  .= newServiceToken s
        # "assets"      .= newServiceAssets s
        # "tags"        .= newServiceTags s
        # []

-- | Response data upon adding a new service.
data NewServiceResponse = NewServiceResponse
    { rsNewServiceId    :: !ServiceId
    , rsNewServiceToken :: !(Maybe ServiceToken)
        -- ^ The generated bearer token that we will use for
        -- authenticating requests towards the service, if none was
        -- provided in the 'NewService' request.
    }

instance FromJSON NewServiceResponse where
    parseJSON = withObject "NewServiceResponse" $ \o ->
        NewServiceResponse <$> o .:  "id"
                           <*> o .:? "auth_token"

instance ToJSON NewServiceResponse where
    toJSON r = object
        $ "id"         .= rsNewServiceId r
        # "auth_token" .= rsNewServiceToken r
        # []

--------------------------------------------------------------------------------
-- Service

-- | Full service definition as seen by the provider.
data Service = Service
    { serviceId      :: !ServiceId
    , serviceName    :: !Name
    , serviceSummary :: !Text
    , serviceDescr   :: !Text
    , serviceUrl     :: !HttpsUrl
    , serviceTokens  :: !(List1 ServiceToken)
    , serviceKeys    :: !(List1 ServiceKey)
    , serviceAssets  :: ![Asset]
    , serviceTags    :: !(Set ServiceTag)
    , serviceEnabled :: !Bool
    }

instance FromJSON Service where
    parseJSON = withObject "Service" $ \o ->
        Service <$> o .: "id"
                <*> o .: "name"
                <*> o .: "summary"
                <*> o .: "description"
                <*> o .: "base_url"
                <*> o .: "auth_tokens"
                <*> o .: "public_keys"
                <*> o .: "assets"
                <*> o .: "tags"
                <*> o .: "enabled"

instance ToJSON Service where
    toJSON s = object
        $ "id"           .= serviceId s
        # "name"         .= serviceName s
        # "summary"      .= serviceSummary s
        # "description"  .= serviceDescr s
        # "base_url"     .= serviceUrl s
        # "auth_tokens"  .= serviceTokens s
        # "public_keys"  .= serviceKeys s
        # "assets"       .= serviceAssets s
        # "tags"         .= serviceTags s
        # "enabled"      .= serviceEnabled s
        # []

--------------------------------------------------------------------------------
-- ServiceProfile

-- | Public profile of a service as seen by users.
data ServiceProfile = ServiceProfile
    { serviceProfileId       :: !ServiceId
    , serviceProfileProvider :: !ProviderId
    , serviceProfileName     :: !Name
    , serviceProfileSummary  :: !Text
    , serviceProfileDescr    :: !Text
    , serviceProfileAssets   :: ![Asset]
    , serviceProfileTags     :: !(Set ServiceTag)
    , serviceProfileEnabled  :: !Bool
    }

instance FromJSON ServiceProfile where
    parseJSON = withObject "ServiceProfile" $ \o ->
        ServiceProfile <$> o .: "id"
                       <*> o .: "provider"
                       <*> o .: "name"
                       <*> o .: "summary"
                       <*> o .: "description"
                       <*> o .: "assets"
                       <*> o .: "tags"
                       <*> o .: "enabled"

instance ToJSON ServiceProfile where
    toJSON s = object
        $ "id"          .= serviceProfileId s
        # "provider"    .= serviceProfileProvider s
        # "name"        .= serviceProfileName s
        # "summary"     .= serviceProfileSummary s
        # "description" .= serviceProfileDescr s
        # "assets"      .= serviceProfileAssets s
        # "tags"        .= serviceProfileTags s
        # "enabled"     .= serviceProfileEnabled s
        # []

--------------------------------------------------------------------------------
-- ServiceProfilePage

data ServiceProfilePage = ServiceProfilePage
    { serviceProfilePageHasMore :: !Bool
    , serviceProfilePageResults :: ![ServiceProfile]
    }

instance FromJSON ServiceProfilePage where
    parseJSON = withObject "ServiceProfilePage" $ \o ->
        ServiceProfilePage <$> o .: "has_more"
                           <*> o .: "services"

instance ToJSON ServiceProfilePage where
    toJSON p = object
        [ "has_more" .= serviceProfilePageHasMore p
        , "services" .= serviceProfilePageResults p
        ]

--------------------------------------------------------------------------------
-- UpdateService

-- | Update service profile information.
data UpdateService = UpdateService
    { updateServiceName     :: !(Maybe Name)
    , updateServiceSummary  :: !(Maybe (Range 1 128 Text))
    , updateServiceDescr    :: !(Maybe (Range 1 1024 Text))
    , updateServiceAssets   :: !(Maybe [Asset])
    , updateServiceTags     :: !(Maybe (Range 1 3 (Set ServiceTag)))
    }

instance FromJSON UpdateService where
    parseJSON = withObject "UpdateService" $ \o ->
        UpdateService <$> o .:? "name"
                      <*> o .:? "summary"
                      <*> o .:? "description"
                      <*> o .:? "assets"
                      <*> o .:? "tags"

instance ToJSON UpdateService where
    toJSON u = object
        $ "name"        .= updateServiceName u
        # "summary"     .= updateServiceSummary u
        # "description" .= updateServiceDescr u
        # "assets"      .= updateServiceAssets u
        # "tags"        .= updateServiceTags u
        # []

--------------------------------------------------------------------------------
-- UpdateServiceConn

-- | Update service connection information.
-- This operation requires re-authentication via password.
data UpdateServiceConn = UpdateServiceConn
    { updateServiceConnPassword :: !PlainTextPassword
    , updateServiceConnUrl      :: !(Maybe HttpsUrl)
    , updateServiceConnKeys     :: !(Maybe (Range 1 2 [ServiceKeyPEM]))
    , updateServiceConnTokens   :: !(Maybe (Range 1 2 [ServiceToken]))
    , updateServiceConnEnabled  :: !(Maybe Bool)
    }

mkUpdateServiceConn :: PlainTextPassword -> UpdateServiceConn
mkUpdateServiceConn pw = UpdateServiceConn pw Nothing Nothing Nothing Nothing

instance FromJSON UpdateServiceConn where
    parseJSON = withObject "UpdateServiceConn" $ \o ->
        UpdateServiceConn <$> o .:  "password"
                          <*> o .:? "base_url"
                          <*> o .:? "public_keys"
                          <*> o .:? "auth_tokens"
                          <*> o .:? "enabled"

instance ToJSON UpdateServiceConn where
    toJSON u = object
        $ "password"       .= updateServiceConnPassword u
        # "base_url"       .= updateServiceConnUrl u
        # "public_keys"    .= updateServiceConnKeys u
        # "auth_tokens"    .= updateServiceConnTokens u
        # "enabled"        .= updateServiceConnEnabled u
        # []

--------------------------------------------------------------------------------
-- DeleteService

-- | Input data for a service deletion request.
newtype DeleteService = DeleteService
    { deleteServicePassword :: PlainTextPassword }

instance FromJSON DeleteService where
    parseJSON = withObject "DeleteService" $ \o ->
        DeleteService <$> o .: "password"

instance ToJSON DeleteService where
    toJSON d = object
        [ "password" .= deleteServicePassword d
        ]

--------------------------------------------------------------------------------
-- AddBot

-- | Input data for adding a bot to a conversation.
data AddBot = AddBot
    { addBotProvider :: !ProviderId
    , addBotService  :: !ServiceId
    , addBotLocale   :: !(Maybe Locale)
    }

instance FromJSON AddBot where
    parseJSON = withObject "NewBot" $ \o ->
        AddBot <$> o .:  "provider"
               <*> o .:  "service"
               <*> o .:? "locale"

instance ToJSON AddBot where
    toJSON n = object
        $ "provider" .= addBotProvider n
        # "service"  .= addBotService n
        # "locale"   .= addBotLocale n
        # []

data AddBotResponse = AddBotResponse
    { rsAddBotId     :: !BotId
    , rsAddBotClient :: !ClientId
    , rsAddBotName   :: !Name
    , rsAddBotColour :: !ColourId
    , rsAddBotAssets :: ![Asset]
    , rsAddBotEvent  :: !Event
    }

instance FromJSON AddBotResponse where
    parseJSON = withObject "AddBotResponse" $ \o ->
        AddBotResponse <$> o .: "id"
                       <*> o .: "client"
                       <*> o .: "name"
                       <*> o .: "accent_id"
                       <*> o .: "assets"
                       <*> o .: "event"

instance ToJSON AddBotResponse where
    toJSON r = object
        [ "id"        .= rsAddBotId r
        , "client"    .= rsAddBotClient r
        , "name"      .= rsAddBotName r
        , "accent_id" .= rsAddBotColour r
        , "assets"    .= rsAddBotAssets r
        , "event"     .= rsAddBotEvent r
        ]

--------------------------------------------------------------------------------
-- RemoveBot

-- (There is no request payload for bot removal)

newtype RemoveBotResponse = RemoveBotResponse
    { rsRemoveBotEvent :: Event
    }

instance FromJSON RemoveBotResponse where
    parseJSON = withObject "RemoveBotResponse" $ \o ->
        RemoveBotResponse <$> o .: "event"

instance ToJSON RemoveBotResponse where
    toJSON r = object
        [ "event" .= rsRemoveBotEvent r
        ]

--------------------------------------------------------------------------------
-- UpdateBotPrekeys

newtype UpdateBotPrekeys = UpdateBotPrekeys
    { updateBotPrekeyList :: [Prekey]
    }

instance FromJSON UpdateBotPrekeys where
    parseJSON = withObject "UpdateBotPrekeys" $ \o ->
        UpdateBotPrekeys <$> o .: "prekeys"

instance ToJSON UpdateBotPrekeys where
    toJSON u = object
        [ "prekeys" .= updateBotPrekeyList u
        ]

