{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Gundeck.Types.Push.V2
    ( Push
    , newPush
    , pushRecipients
    , pushOrigin
    , pushConnections
    , pushOriginConnection
    , pushTransient
    , pushNativeIncludeOrigin
    , pushNativeEncrypt
    , pushNativeAps
    , pushNativePriority
    , pushPayload
    , singletonRecipient
    , singletonPayload

    , Recipient
    , recipient
    , recipientId
    , recipientRoute
    , recipientClients
    , recipientFallback

    , Priority  (..)
    , Route     (..)
    , Transport (..)

    , ApsData
    , ApsPreference (..)
    , ApsLocKey     (..)
    , ApsSound      (..)
    , apsData
    , apsLocKey
    , apsLocArgs
    , apsSound
    , apsPreference
    , apsBadge

    , PushToken
    , Token   (..)
    , AppName (..)
    , pushToken
    , tokenTransport
    , tokenApp
    , tokenClient
    , tokenFallback
    , token

    , EncKey        (..)
    , MacKey        (..)
    , SignalingKeys (..)
    ) where

import Control.Lens (makeLenses)
import Control.Monad
import Data.Aeson
import Data.Attoparsec.ByteString (takeByteString)
import Data.ByteString (ByteString)
import Data.ByteString.Conversion
import Data.Id
import Data.Json.Util
import Data.Monoid
import Data.List1
import Data.Range
import Data.Set (Set)
import Data.String
import Data.Text (Text)
import Data.Text.Encoding

import qualified Data.ByteString.Base64 as B64
import qualified Data.List1             as List1
import qualified Data.Range             as Range
import qualified Data.Set               as Set

-----------------------------------------------------------------------------
-- Route

data Route
    = RouteAny
    | RouteDirect
    | RouteNative
    deriving (Eq, Ord, Show)

instance FromJSON Route where
    parseJSON (String "any")    = return RouteAny
    parseJSON (String "direct") = return RouteDirect
    parseJSON (String "native") = return RouteNative
    parseJSON x = fail $ "Invalid routing: " ++ show (encode x)

instance ToJSON Route where
    toJSON RouteAny    = String "any"
    toJSON RouteDirect = String "direct"
    toJSON RouteNative = String "native"

-----------------------------------------------------------------------------
-- Recipient

data Recipient = Recipient
    { _recipientId        :: !UserId
    , _recipientRoute     :: !Route
    , _recipientClients   :: ![ClientId]
    , _recipientFallback  :: !Bool
    } deriving (Show)

instance Eq Recipient where
    (Recipient uid1 _ _ _) == (Recipient uid2 _ _ _) = uid1 == uid2

instance Ord Recipient where
    compare r r' = compare (_recipientId r) (_recipientId r')

makeLenses ''Recipient

recipient :: UserId -> Route -> Recipient
recipient u r = Recipient u r [] True

instance FromJSON Recipient where
    parseJSON = withObject "Recipient" $ \p ->
      Recipient <$> p .:  "user_id"
                <*> p .:  "route"
                <*> p .:? "clients" .!= []
                <*> p .:? "fallback" .!= True

instance ToJSON Recipient where
    toJSON (Recipient u r c f) = object
        $ "user_id"   .= u
        # "route"     .= r
        # "clients"   .= (if null c then Nothing else Just c)
        # "fallback"  .= (if not f then Just False else Nothing)
        # []

-----------------------------------------------------------------------------
-- ApsData

newtype ApsSound = ApsSound { fromSound :: Text }
    deriving (Eq, Show, ToJSON, FromJSON)

newtype ApsLocKey = ApsLocKey { fromLocKey :: Text }
    deriving (Eq, Show, ToJSON, FromJSON)

data ApsPreference
    = ApsStdPreference
    | ApsVoIPPreference
    deriving (Eq, Show)

instance ToJSON ApsPreference where
    toJSON ApsVoIPPreference = "voip"
    toJSON ApsStdPreference  = "std"

instance FromJSON ApsPreference where
    parseJSON = withText "ApsPreference" $ \case
        "voip" -> pure ApsVoIPPreference
        "std"  -> pure ApsStdPreference
        x      -> fail $ "Invalid preference: " ++ show x

data ApsData = ApsData
    { _apsLocKey     :: !ApsLocKey
    , _apsLocArgs    :: [Text]
    , _apsSound      :: !(Maybe ApsSound)
    , _apsPreference :: !(Maybe ApsPreference)
    , _apsBadge      :: !Bool
    } deriving (Eq, Show)

makeLenses ''ApsData

apsData :: ApsLocKey -> [Text] -> ApsData
apsData lk la = ApsData lk la Nothing Nothing True

instance ToJSON ApsData where
    toJSON (ApsData k a s p b) = object
        $ "loc_key"    .= k
        # "loc_args"   .= a
        # "sound"      .= s
        # "preference" .= p
        # "badge"      .= b
        # []

instance FromJSON ApsData where
    parseJSON = withObject "ApsData" $ \o ->
        ApsData <$> o .:  "loc_key"
                <*> o .:? "loc_args" .!= []
                <*> o .:? "sound"
                <*> o .:? "preference"
                <*> o .:? "badge"    .!= True

-----------------------------------------------------------------------------
-- Priority

data Priority = LowPriority | HighPriority
    deriving (Eq, Show, Ord, Enum)

instance ToJSON Priority where
    toJSON LowPriority  = String "low"
    toJSON HighPriority = String "high"

instance FromJSON Priority where
    parseJSON = withText "Priority" $ \case
        "low"  -> pure LowPriority
        "high" -> pure HighPriority
        x      -> fail $ "Invalid push priority: " ++ show x

-----------------------------------------------------------------------------
-- Push

data Push = Push
    { _pushRecipients :: Range 1 1024 (Set Recipient)
      -- ^ Recipients
    , _pushOrigin :: !UserId
      -- ^ Originating user
    , _pushConnections :: !(Set ConnId)
      -- ^ Destination connections, if a directed push is desired.
    , _pushOriginConnection :: !(Maybe ConnId)
      -- ^ Originating connection, if any.
    , _pushTransient :: !Bool
      -- ^ Transient payloads are not forwarded to the notification stream.
    , _pushNativeIncludeOrigin :: !Bool
      -- ^ Whether to send native notifications to other clients
      -- of the originating user, if he is among the recipients.
    , _pushNativeEncrypt :: !Bool
      -- ^ Should native push payloads be encrypted?
    , _pushNativeAps :: !(Maybe ApsData)
      -- ^ APNs-specific metadata.
    , _pushNativePriority :: !Priority
      -- ^ Native push priority.
    , _pushPayload :: !(List1 Object)
      -- ^ Opaque payload
    } deriving (Eq, Show)

makeLenses ''Push

newPush :: UserId -> Range 1 1024 (Set Recipient) -> List1 Object -> Push
newPush from to pload = Push
    { _pushRecipients          = to
    , _pushOrigin              = from
    , _pushConnections         = Set.empty
    , _pushOriginConnection    = Nothing
    , _pushTransient           = False
    , _pushNativeIncludeOrigin = True
    , _pushNativeEncrypt       = True
    , _pushNativeAps           = Nothing
    , _pushNativePriority      = HighPriority
    , _pushPayload             = pload
    }

singletonRecipient :: Recipient -> Range 1 1024 (Set Recipient)
singletonRecipient = Range.unsafeRange . Set.singleton

singletonPayload :: ToJSONObject a => a -> List1 Object
singletonPayload = List1.singleton . toJSONObject

instance FromJSON Push where
    parseJSON = withObject "Push" $ \p ->
        Push <$> p .:  "recipients"
             <*> p .:  "origin"
             <*> p .:? "connections"           .!= Set.empty
             <*> p .:? "origin_connection"
             <*> p .:? "transient"             .!= False
             <*> p .:? "native_include_origin" .!= True
             <*> p .:? "native_encrypt"        .!= True
             <*> p .:? "native_aps"
             <*> p .:? "native_priority"       .!= HighPriority
             <*> p .:  "payload"

instance ToJSON Push where
    toJSON p = object
        $ "recipients"            .= _pushRecipients p
        # "origin"                .= _pushOrigin p
        # "connections"           .= ifNot Set.null (_pushConnections p)
        # "origin_connection"     .= _pushOriginConnection p
        # "transient"             .= ifNot (== False) (_pushTransient p)
        # "native_include_origin" .= ifNot (== True) (_pushNativeIncludeOrigin p)
        # "native_encrypt"        .= ifNot (== True) (_pushNativeEncrypt p)
        # "native_aps"            .= _pushNativeAps p
        # "native_priority"       .= ifNot (== HighPriority) (_pushNativePriority p)
        # "payload"               .= _pushPayload p
        # []
      where
        ifNot f a = if f a then Nothing else Just a

-----------------------------------------------------------------------------
-- Transport

data Transport
    = GCM
    | APNS
    | APNSSandbox
    | APNSVoIP
    | APNSVoIPSandbox
    deriving (Eq, Ord, Show, Bounded, Enum)

instance ToJSON Transport where
    toJSON GCM             = "GCM"
    toJSON APNS            = "APNS"
    toJSON APNSSandbox     = "APNS_SANDBOX"
    toJSON APNSVoIP        = "APNS_VOIP"
    toJSON APNSVoIPSandbox = "APNS_VOIP_SANDBOX"

instance FromJSON Transport where
    parseJSON = withText "transport" $ \case
        "GCM"               -> return GCM
        "APNS"              -> return APNS
        "APNS_SANDBOX"      -> return APNSSandbox
        "APNS_VOIP"         -> return APNSVoIP
        "APNS_VOIP_SANDBOX" -> return APNSVoIPSandbox
        x                   -> fail $ "Invalid push transport: " ++ show x

instance FromByteString Transport where
    parser = takeByteString >>= \case
        "GCM"               -> return GCM
        "APNS"              -> return APNS
        "APNS_SANDBOX"      -> return APNSSandbox
        "APNS_VOIP"         -> return APNSVoIP
        "APNS_VOIP_SANDBOX" -> return APNSVoIPSandbox
        x                   -> fail $ "Invalid push transport: " <> show x

-----------------------------------------------------------------------------
-- PushToken

newtype Token = Token
    { tokenText :: Text
    } deriving (Eq, Ord, Show, FromJSON, ToJSON, FromByteString, ToByteString)

newtype AppName = AppName
    { appNameText :: Text
    } deriving (Eq, Ord, Show, FromJSON, ToJSON, IsString)

data PushToken = PushToken
    { _tokenTransport :: !Transport
    , _tokenApp       :: !AppName
    , _token          :: !Token
    , _tokenClient    :: !ClientId
    , _tokenFallback  :: !(Maybe Transport)
    } deriving (Eq, Ord, Show)

makeLenses ''PushToken

pushToken :: Transport -> AppName -> Token -> ClientId -> PushToken
pushToken tp an tk cl = PushToken tp an tk cl Nothing

instance ToJSON PushToken where
    toJSON p = object
        $ "transport" .= _tokenTransport p
        # "app"       .= _tokenApp p
        # "token"     .= _token p
        # "client"    .= _tokenClient p
        # "fallback"  .= _tokenFallback p
        # []

instance FromJSON PushToken where
    parseJSON = withObject "PushToken" $ \p ->
        PushToken <$> p .:  "transport"
                  <*> p .:  "app"
                  <*> p .:  "token"
                  <*> p .:  "client"
                  <*> p .:? "fallback"

-----------------------------------------------------------------------------
-- Native Push Encryption

checkKeyLen :: ByteString -> Either String (Range 32 32 ByteString)
checkKeyLen = Range.checkedEither

-- | Symmetric encryption key of a specific client for native push notifications.
newtype EncKey = EncKey
    { encKeyBytes :: ByteString
    } deriving Eq

-- | MAC key of a specific client for native push notifications.
newtype MacKey = MacKey
    { macKeyBytes :: ByteString
    } deriving Eq

data SignalingKeys = SignalingKeys
    { sigEncKey :: !EncKey
    , sigMacKey :: !MacKey
    }

instance FromJSON EncKey where
    parseJSON = withText "EncKey" $
          either fail (return . EncKey . fromRange)
        . (checkKeyLen <=< B64.decode)
        . encodeUtf8

instance ToJSON EncKey where
    toJSON = String . decodeLatin1 . B64.encode . encKeyBytes

instance FromJSON MacKey where
    parseJSON = withText "MacKey" $
          either fail (return . MacKey . fromRange)
        . (checkKeyLen <=< B64.decode)
        . encodeUtf8

instance ToJSON MacKey where
    toJSON = String . decodeLatin1 . B64.encode . macKeyBytes

instance FromJSON SignalingKeys where
    parseJSON = withObject "signaling-keys" $ \o ->
        SignalingKeys <$> o .: "enckey" <*> o .: "mackey"

instance ToJSON SignalingKeys where
    toJSON (SignalingKeys ek mk) = object
        [ "enckey" .= ek, "mackey" .= mk ]
