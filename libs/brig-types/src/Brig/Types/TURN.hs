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
    , turiHost
    , turiPort

    , TurnHost
    , _TurnHost

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
import           Data.Aeson.Lens
import           Data.Attoparsec.Text
import           Data.ByteString.Builder
import qualified Data.ByteString.Conversion as BC
import           Data.List1
import           Data.Misc                  (IpAddr, Port (portNumber))
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.Ascii
import qualified Data.Text.Lazy.Builder.Int as TB
import           Data.Text.Lazy.Lens        (builder)
import           Data.Text.Strict.Lens      (utf8)
import           Data.Time.Clock.POSIX
import           Data.Word
import           GHC.Generics               (Generic)
import           Safe                       (readMay)

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

-- | TURN server URI of the form \"turn:<addr>:<port>\"
data TurnURI = TurnURI
    { _turiHost :: TurnHost
    , _turiPort :: Port
    }
    deriving (Eq, Show, Generic)

-- future versions may allow using a hostname
newtype TurnHost = TurnHost IpAddr
    deriving (Eq, Show, Generic)

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

turnURI :: TurnHost -> Port -> TurnURI
turnURI = TurnURI

-- turn into a 'Prism'' once hostnames are supported
_TurnHost :: Iso' TurnHost IpAddr
_TurnHost = iso (\(TurnHost ip) -> ip) TurnHost

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


instance ToJSON TurnURI where
    toEncoding uri = text . view (from builder . strict) $
          "turn:"^.builder
       <> view (turiHost . re (_JSON :: Prism' Value TurnHost) . _String . lazy . builder) uri
       <> ":"^.builder
       <> TB.decimal (portNumber (view turiPort uri))

instance FromJSON TurnURI where
    parseJSON = withText "TurnURI" $
        either fail pure . parseOnly (parseTurnURI <* endOfInput)

parseTurnURI :: Parser TurnURI
parseTurnURI = TurnURI 
    <$> ((string "turn:" *> takeWhile1 (/=':') <* char ':') >>= txtToTurnHost)
    <*> decimal

txtToTurnHost :: Text -> Parser TurnHost
txtToTurnHost t = case readMay (T.unpack t) of
    Just h  -> return (TurnHost h)
    Nothing -> fail ("txtToTurnHost: Could not parse as IpAddr: " ++ show t)


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
