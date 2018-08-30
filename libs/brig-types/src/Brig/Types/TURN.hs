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

    , TurnHost -- Re-export
    , isHostName
    , parseTurnHost

    , TurnUsername
    , turnUsername
    , tuExpiresAt
    , tuVersion
    , tuKeyindex
    , tuT
    , tuRandom

    , isUdp
    , isTcp
    , isTls
    , limitServers
    )
where

import           Brig.Types.TURN.Internal
import           Control.Lens               hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Encoding        (text)
import           Data.Attoparsec.Text       hiding (parse)
import           Data.ByteString            (ByteString)
import           Data.ByteString.Builder
import qualified Data.ByteString.Conversion as BC
import           Data.List1
import           Data.Misc                  (Port (..))
import           Data.Monoid
import           Data.Text                  (Text)
import           Data.Text.Ascii
import qualified Data.Text.Encoding         as TE
import           Data.Text.Strict.Lens      (utf8)
import           Data.Time.Clock.POSIX
import           Data.Word
import           GHC.Base                   (Alternative)
import           GHC.Generics               (Generic)

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

-- | given a list of URIs and a size, limit URIs
-- with order priority from highest to lowest: UDP -> TLS -> TCP
-- i.e. (if enough servers of each type are available)
--   1 -> 1x UDP
--   2 -> 1x UDP, 1x TLS
--   3 -> 1x UDP, 1x TLS, 1x TCP
--   4 -> 2x UDP, 1x TLS, 1x TCP
--   5 -> 2x UDP, 2x TLS, 1x TCP
--    ... etc
-- if not enough servers are available, prefer udp, then tls
limitServers :: [TurnURI] -> Int -> [TurnURI]
limitServers uris = limitHeuristic [] (splitByType ([],[],[],[]) uris)
  where
    splitByType acc       [] = acc
    splitByType (a,b,c,d) (x:xs)
        | isUdp x   = splitByType (x:a, b  , c  ,  d) xs
        | isTls x   = splitByType (a  , x:b, c  ,  d) xs
        | isTcp x   = splitByType (a  , b  , x:c,  d) xs
        | otherwise = splitByType (a  , b  , c  ,x:d) xs

-- | helper function for 'limitServers'
limitHeuristic :: [TurnURI]
  -> ([TurnURI], [TurnURI], [TurnURI], [TurnURI])
  -> Int
  -> [TurnURI]
-- case at least one of each available
limitHeuristic xs (udp:udps, tls:tlss, tcp:tcps, o) lim = case lim - length(xs) of
        x | x >= 3 ->  limitHeuristic (udp:tls:tcp:xs) (udps, tlss, tcps, o) lim
        2          ->  limitHeuristic (udp:tls:xs) (udps, tlss, tcp:tcps, o) lim
        1          ->  limitHeuristic (udp:xs) (udps, tls:tlss, tcp:tcps, o) lim
        _          -> xs
-- case at least one of udp/tls available
limitHeuristic xs (udp:udps, tls:tlss, [], o) lim = case lim - length(xs) of
        x | x >= 2 ->  limitHeuristic (udp:tls:xs) (udps, tlss, [], o) lim
        1          ->  limitHeuristic (udp:xs) (udps, tls:tlss, [], o) lim
        _          -> xs
-- case at least one udp available
limitHeuristic xs (udp:udps, tlss, tcps, o) lim = case lim - length(xs) of
        x | x >= 1 ->  limitHeuristic (udp:xs) (udps, tlss, tcps, o) lim
        _          -> xs
-- case at least one tls available
limitHeuristic xs (udps, tls:tlss, tcps, o) lim = case lim - length(xs) of
        x | x >= 1 ->  limitHeuristic (tls:xs) (udps, tlss, tcps, o) lim
        _          -> xs
-- case limit not reached yet, but no more udp/tls: fill with whatever is left
limitHeuristic xs (udps, tlss, tcps, o) lim = case lim - length(xs) of
        x | x >= 1 -> (Prelude.take x (udps++tlss++tcps++o))++xs
        _          -> xs

isUdp :: TurnURI -> Bool
isUdp uri = uri^.turiScheme == SchemeTurn
        && ( uri^.turiTransport == Just(TransportUDP)
            || uri^.turiTransport == Nothing )

isTcp :: TurnURI -> Bool
isTcp uri = uri^.turiScheme == SchemeTurn
        && uri^.turiTransport == Just(TransportTCP)

isTls :: TurnURI -> Bool
isTls uri = uri^.turiScheme == SchemeTurns
        && uri^.turiTransport == Just(TransportTCP)
