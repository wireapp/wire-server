{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Brig.Types.TURN
    ( RTCConfiguration
    , rtcConfiguration
    , rtcConfIceServers
    , rtcConfTTL

    , RTCIceServer
    , rtcIceServer
    , iceURLs
    , iceUsername
    , iceCredential

    , TurnURI
    , turnURI
    , turiScheme
    , Scheme (..)
    , turiHost
    , turiPort
    , turiTransport
    , Transport (..)

    , TurnHost (..)
    , isHostName
    , TurnUsername
    , turnUsername
    , tuExpiresAt
    , tuVersion
    , tuKeyindex
    , tuT
    , tuRandom
    )
where

import           Control.Lens               hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Encoding        (text)
import           Data.Attoparsec.Text       hiding (parse)
import           Data.ByteString            (ByteString)
import           Data.ByteString.Builder
import qualified Data.ByteString.Conversion as BC
import           Data.List1
import           Data.Misc                  (IpAddr (..), Port (..))
import           Data.Monoid
import           Data.Text                  (Text)
import           Data.Text.Ascii
import qualified Data.Text.Encoding         as TE
import           Data.Text.Strict.Lens      (utf8)
import           Data.Time.Clock.POSIX
import           Data.Word
import           GHC.Base                   (Alternative)
import           GHC.Generics               (Generic)
import           Text.Hostname

-- | A configuration object resembling \"RTCConfiguration\"
--
-- The \"ttl\" field is a proprietary extension
--
-- cf. https://developer.mozilla.org/en-US/docs/Web/API/RTCPeerConnection/RTCPeerConnection#RTCConfiguration_dictionary
--
data RTCConfiguration = RTCConfiguration
    { _rtcConfIceServers :: List1 RTCIceServer
    , _rtcConfTTL        :: Word32
    } deriving (Show, Generic)

-- | A configuration object resembling \"RTCIceServer\"
--
-- cf. https://developer.mozilla.org/en-US/docs/Web/API/RTCIceServer
--
data RTCIceServer = RTCIceServer
    { _iceURLs       :: List1 TurnURI
    , _iceUsername   :: TurnUsername
    , _iceCredential :: AsciiBase64
    } deriving (Show, Generic)

-- | TURN server URI as described in https://tools.ietf.org/html/rfc7065, minus ext
-- |
-- | turnURI       = scheme ":" host [ ":" port ]
-- |                 [ "?transport=" transport ]
-- | scheme        = "turn" / "turns"
-- | transport     = "udp" / "tcp" / transport-ext
-- | transport-ext = 1*unreserved
--
data TurnURI = TurnURI
    { _turiScheme    :: Scheme
    , _turiHost      :: TurnHost
    , _turiPort      :: Port
    , _turiTransport :: Maybe Transport
    } deriving (Eq, Show, Generic)

data Scheme = SchemeTurn
            | SchemeTurns
            deriving (Eq, Show, Generic, Bounded, Enum)

data Transport = TransportUDP
               | TransportTCP
               deriving (Eq, Show, Generic, Enum, Bounded)

data TurnHost = TurnHostIp IpAddr
              | TurnHostName Text
    deriving (Eq, Show, Generic)

isHostName :: TurnHost -> Bool
isHostName (TurnHostIp   _) = False
isHostName (TurnHostName _) = True

data TurnUsername = TurnUsername
    { _tuExpiresAt :: POSIXTime
    , _tuVersion   :: Word
    , _tuKeyindex  :: Word32     -- seems to large, but uint32_t is used in C
    , _tuT         :: Char       -- undocumented, always 's'
    , _tuRandom    :: Text       -- [a-z0-9]+
    } deriving (Show, Generic)


rtcConfiguration :: List1 RTCIceServer -> Word32 -> RTCConfiguration
rtcConfiguration = RTCConfiguration

rtcIceServer :: List1 TurnURI -> TurnUsername -> AsciiBase64 -> RTCIceServer
rtcIceServer = RTCIceServer

turnURI :: Scheme -> TurnHost -> Port -> Maybe Transport -> TurnURI
turnURI = TurnURI

-- note that the random value is not checked for well-formedness
turnUsername :: POSIXTime -> Text -> TurnUsername
turnUsername expires rnd = TurnUsername
    { _tuExpiresAt = expires
    , _tuVersion   = 1
    , _tuKeyindex  = 0
    , _tuT         = 's'
    , _tuRandom    = rnd
    }


makeLenses ''RTCConfiguration
makeLenses ''RTCIceServer
makeLenses ''TurnURI
makeLenses ''TurnUsername


instance ToJSON RTCConfiguration where
    toEncoding (RTCConfiguration srvs ttl)
        = pairs ("ice_servers" .= srvs <> "ttl" .= ttl)

instance FromJSON RTCConfiguration where
    parseJSON = withObject "RTCConfiguration" $ \o ->
        RTCConfiguration <$> o .: "ice_servers" <*> o .: "ttl"


instance ToJSON RTCIceServer where
    toEncoding (RTCIceServer urls name cred) = pairs
        (  "urls"       .= urls
        <> "username"   .= name
        <> "credential" .= cred
        )

instance FromJSON RTCIceServer where
    parseJSON = withObject "RTCIceServer" $ \o ->
        RTCIceServer <$> o .: "urls" <*> o .: "username" <*> o .: "credential"

parseTurnHost :: Text -> Maybe TurnHost
parseTurnHost h = case BC.fromByteString host of
    Just ip@(IpAddr _)           -> Just $ TurnHostIp ip
    Nothing | validHostname host -> Just $ TurnHostName h -- NOTE: IPv4 addresses are also valid hostnames...
    _                            -> Nothing
  where
    host = TE.encodeUtf8 h

instance BC.FromByteString TurnHost where
    parser = BC.parser >>= maybe (fail "Invalid turn host") return . parseTurnHost

instance BC.ToByteString TurnHost where
    builder (TurnHostIp ip)  = BC.builder ip
    builder (TurnHostName n) = BC.builder n

instance BC.ToByteString TurnURI where
    builder (TurnURI s h (Port p) tp) =
           BC.builder s
        <> byteString ":"
        <> BC.builder h
        <> byteString ":"
        <> BC.builder p
        <> maybe mempty ((byteString "?transport=" <>) . BC.builder) tp

instance ToJSON TurnURI where
    toJSON = String . TE.decodeUtf8 . BC.toByteString'

instance BC.FromByteString TurnURI where
    parser = BC.parser >>= either fail pure . parseTurnURI

instance FromJSON TurnURI where
    parseJSON = withText "TurnURI" $ either fail pure . parseTurnURI

parseTurnURI :: Text -> Either String TurnURI
parseTurnURI = parseOnly (parser <* endOfInput)
  where
    parser = TurnURI
          <$> ((takeWhile1 (/=':') <* char ':' >>= parseScheme)                   <?> "parsingScheme")
          <*> ((takeWhile1 (/=':') <* char ':' >>= parseHost)                     <?> "parsingHost")
          <*> (decimal                                                            <?> "parsingPort")
          <*> ((optional ((string "?transport=" *> takeText) >>= parseTransport)) <?> "parsingTransport")

    parseScheme    = parse "parseScheme"
    parseHost      = parse "parseHost"
    parseTransport = parse "parseTransport"

    parse :: (BC.FromByteString b, Monad m) => String -> Text -> m b
    parse err x = case BC.fromByteString (TE.encodeUtf8 x) of
        Just ok -> return ok
        Nothing -> fail (err ++ " failed when parsing: " ++ show x)

instance ToJSON   TurnHost
instance FromJSON TurnHost


instance ToJSON TurnUsername where
    toEncoding = text . view utf8 . BC.toByteString'

instance FromJSON TurnUsername where
    parseJSON = withText "TurnUsername" $
        either fail pure . parseOnly (parseTurnUsername <* endOfInput)

instance BC.ToByteString TurnUsername where
    builder tu
         = shortByteString "d="
        <> word64Dec (round (view tuExpiresAt tu))
        <> shortByteString ".v="
        <> wordDec (view tuVersion tu)
        <> shortByteString ".k="
        <> word32Dec (view tuKeyindex tu)
        <> shortByteString ".t="
        <> charUtf8 (view tuT tu)
        <> shortByteString ".r="
        <> byteString (view (tuRandom . re utf8) tu)

parseTurnUsername :: Parser TurnUsername
parseTurnUsername = TurnUsername
    <$> (string "d="  *> fmap (fromIntegral :: Word64 -> POSIXTime) decimal)
    <*> (string ".v=" *> decimal)
    <*> (string ".k=" *> decimal)
    <*> (string ".t=" *> anyChar)
    <*> (string ".r=" *> takeWhile1 (inClass "a-z0-9"))

instance BC.FromByteString Scheme where
    parser = BC.parser >>= \t -> case (t :: ByteString) of
        "turn"  -> pure SchemeTurn
        "turns" -> pure SchemeTurns
        _       -> fail $ "Invalid turn scheme: " ++ show t

instance BC.ToByteString Scheme where
    builder SchemeTurn  = "turn"
    builder SchemeTurns = "turns"

instance FromJSON Scheme where
    parseJSON = withText "Scheme" $
        either fail pure . BC.runParser BC.parser . TE.encodeUtf8

instance ToJSON Scheme where
    toJSON = String . TE.decodeUtf8 . BC.toByteString'

instance BC.FromByteString Transport where
    parser = BC.parser >>= \t -> case (t :: ByteString) of
        "udp" -> pure TransportUDP
        "tcp" -> pure TransportTCP
        _     -> fail $ "Invalid turn transport: " ++ show t

instance BC.ToByteString Transport where
    builder TransportUDP = "udp"
    builder TransportTCP = "tcp"

instance FromJSON Transport where
    parseJSON = withText "Transport" $
        either fail pure . BC.runParser BC.parser . TE.encodeUtf8

instance ToJSON Transport where
    toJSON = String . TE.decodeUtf8 . BC.toByteString'

-- Convenience
optional :: (Alternative f, Functor f) => f a -> f (Maybe a)
optional x = option Nothing (Just <$> x)
