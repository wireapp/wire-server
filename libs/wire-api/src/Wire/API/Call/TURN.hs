{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

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

module Wire.API.Call.TURN
  ( -- * RTCConfiguration
    RTCConfiguration,
    rtcConfiguration,
    rtcConfIceServers,
    rtcConfTTL,

    -- * RTCIceServer
    RTCIceServer,
    rtcIceServer,
    iceURLs,
    iceUsername,
    iceCredential,

    -- * TurnURI
    TurnURI,
    turnURI,
    turiScheme,
    Scheme (..),
    turiHost,
    turiPort,
    turiTransport,
    Transport (..),
    TurnHost (..),
    isHostName,

    -- * TurnUsername
    TurnUsername,
    turnUsername,
    tuExpiresAt,
    tuVersion,
    tuKeyindex,
    tuT,
    tuRandom,

    -- * convenience
    isUdp,
    isTcp,
    isTls,
    limitServers,

    -- * Swagger
    modelRtcConfiguration,
    modelRtcIceServer,
  )
where

import Control.Applicative (optional)
import Control.Lens hiding ((.=))
import Data.Aeson hiding ((<?>))
import Data.Attoparsec.Text hiding (parse)
import Data.ByteString.Builder
import qualified Data.ByteString.Conversion as BC
import qualified Data.IP as IP
import Data.List1
import Data.Misc (IpAddr (IpAddr), Port (..))
import qualified Data.Swagger.Build.Api as Doc
import qualified Data.Text as Text
import Data.Text.Ascii
import qualified Data.Text.Encoding as TE
import Data.Text.Strict.Lens (utf8)
import Data.Time.Clock.POSIX
import Imports
import qualified Test.QuickCheck as QC
import Text.Hostname (validHostname)
import Wire.API.Arbitrary (Arbitrary (arbitrary), GenericUniform (..))

--------------------------------------------------------------------------------
-- RTCConfiguration

-- | A configuration object resembling \"RTCConfiguration\"
--
-- The \"ttl\" field is a proprietary extension
--
-- cf. https://developer.mozilla.org/en-US/docs/Web/API/RTCPeerConnection/RTCPeerConnection#RTCConfiguration_dictionary
data RTCConfiguration = RTCConfiguration
  { _rtcConfIceServers :: List1 RTCIceServer,
    _rtcConfTTL :: Word32
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform RTCConfiguration)

rtcConfiguration :: List1 RTCIceServer -> Word32 -> RTCConfiguration
rtcConfiguration = RTCConfiguration

modelRtcConfiguration :: Doc.Model
modelRtcConfiguration = Doc.defineModel "RTCConfiguration" $ do
  Doc.description "A subset of the WebRTC 'RTCConfiguration' dictionary"
  Doc.property "ice_servers" (Doc.array (Doc.ref modelRtcIceServer)) $
    Doc.description "Array of 'RTCIceServer' objects"
  Doc.property "ttl" Doc.int32' $
    Doc.description "Number of seconds after which the configuration should be refreshed (advisory)"

instance ToJSON RTCConfiguration where
  toJSON (RTCConfiguration srvs ttl) =
    object
      [ "ice_servers" .= srvs,
        "ttl" .= ttl
      ]

instance FromJSON RTCConfiguration where
  parseJSON = withObject "RTCConfiguration" $ \o ->
    RTCConfiguration <$> o .: "ice_servers" <*> o .: "ttl"

--------------------------------------------------------------------------------
-- RTCIceServer

-- | A configuration object resembling \"RTCIceServer\"
--
-- cf. https://developer.mozilla.org/en-US/docs/Web/API/RTCIceServer
data RTCIceServer = RTCIceServer
  { _iceURLs :: List1 TurnURI,
    _iceUsername :: TurnUsername,
    _iceCredential :: AsciiBase64
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform RTCIceServer)

rtcIceServer :: List1 TurnURI -> TurnUsername -> AsciiBase64 -> RTCIceServer
rtcIceServer = RTCIceServer

modelRtcIceServer :: Doc.Model
modelRtcIceServer = Doc.defineModel "RTCIceServer" $ do
  Doc.description "A subset of the WebRTC 'RTCIceServer' object"
  Doc.property "urls" (Doc.array Doc.string') $
    Doc.description "Array of TURN server addresses of the form 'turn:<addr>:<port>'"
  Doc.property "username" Doc.string' $
    Doc.description "Username to use for authenticating against the given TURN servers"
  Doc.property "credential" Doc.string' $
    Doc.description "Password to use for authenticating against the given TURN servers"

instance ToJSON RTCIceServer where
  toJSON (RTCIceServer urls name cred) =
    object
      [ "urls" .= urls,
        "username" .= name,
        "credential" .= cred
      ]

instance FromJSON RTCIceServer where
  parseJSON = withObject "RTCIceServer" $ \o ->
    RTCIceServer <$> o .: "urls" <*> o .: "username" <*> o .: "credential"

--------------------------------------------------------------------------------
-- TurnURI

-- | TURN server URI as described in https://tools.ietf.org/html/rfc7065, minus ext
-- |
-- | turnURI       = scheme ":" host [ ":" port ]
-- |                 [ "?transport=" transport ]
-- | scheme        = "turn" / "turns"
-- | transport     = "udp" / "tcp" / transport-ext
-- | transport-ext = 1*unreserved
--
-- FUTUREWORK: Can contain, but refuses to deserialize IPv6 hosts, see 'parseTurnURI'
-- and the 'Arbitrary' instance. Please fix this.
data TurnURI = TurnURI
  { _turiScheme :: Scheme,
    _turiHost :: TurnHost,
    _turiPort :: Port,
    _turiTransport :: Maybe Transport
  }
  deriving stock (Eq, Show, Generic)

turnURI :: Scheme -> TurnHost -> Port -> Maybe Transport -> TurnURI
turnURI = TurnURI

instance BC.ToByteString TurnURI where
  builder (TurnURI s h (Port p) tp) =
    BC.builder s
      <> byteString ":"
      <> BC.builder h
      <> byteString ":"
      <> BC.builder p
      <> maybe mempty ((byteString "?transport=" <>) . BC.builder) tp

instance BC.FromByteString TurnURI where
  parser = BC.parser >>= either fail pure . parseTurnURI

parseTurnURI :: Text -> Either String TurnURI
parseTurnURI = parseOnly (parser <* endOfInput)
  where
    parser =
      TurnURI
        <$> ((takeWhile1 (/= ':') <* char ':' >>= parseScheme) <?> "parsingScheme")
        <*> ((takeWhile1 (/= ':') <* char ':' >>= parseHost) <?> "parsingHost")
        <*> (decimal <?> "parsingPort")
        <*> ((optional ((string "?transport=" *> takeText) >>= parseTransport)) <?> "parsingTransport")
    parseScheme = parse "parseScheme"
    parseHost = parse "parseHost"
    parseTransport = parse "parseTransport"
    parse :: (BC.FromByteString b, Monad m) => String -> Text -> m b
    parse err x = case BC.fromByteString (TE.encodeUtf8 x) of
      Just ok -> return ok
      Nothing -> fail (err ++ " failed when parsing: " ++ show x)

instance ToJSON TurnURI where
  toJSON = String . TE.decodeUtf8 . BC.toByteString'

instance FromJSON TurnURI where
  parseJSON = withText "TurnURI" $ either fail pure . parseTurnURI

instance Arbitrary TurnURI where
  arbitrary = (getGenericUniform <$> arbitrary) `QC.suchThat` (not . isIPv6)
    where
      isIPv6 h = case _turiHost h of
        TurnHostIp (IpAddr (IP.IPv6 _)) -> True
        _ -> False

data Scheme
  = SchemeTurn
  | SchemeTurns
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Scheme)

instance BC.ToByteString Scheme where
  builder SchemeTurn = "turn"
  builder SchemeTurns = "turns"

instance BC.FromByteString Scheme where
  parser =
    BC.parser >>= \t -> case (t :: ByteString) of
      "turn" -> pure SchemeTurn
      "turns" -> pure SchemeTurns
      _ -> fail $ "Invalid turn scheme: " ++ show t

instance ToJSON Scheme where
  toJSON = String . TE.decodeUtf8 . BC.toByteString'

instance FromJSON Scheme where
  parseJSON =
    withText "Scheme" $
      either fail pure . BC.runParser BC.parser . TE.encodeUtf8

data TurnHost
  = TurnHostIp IpAddr
  | TurnHostName Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance BC.FromByteString TurnHost where
  parser = BC.parser >>= maybe (fail "Invalid turn host") return . parseTurnHost

instance BC.ToByteString TurnHost where
  builder (TurnHostIp ip) = BC.builder ip
  builder (TurnHostName n) = BC.builder n

instance Arbitrary TurnHost where
  arbitrary =
    QC.oneof
      [ TurnHostIp <$> arbitrary,
        TurnHostName <$> genHostName
      ]
    where
      -- values that should fulfill 'validHostname'
      genHostName =
        QC.elements
          [ "host.name",
            "a-c",
            "123",
            "007.com",
            "xn--mgbh0fb.xn--kgbechtv"
          ]

isHostName :: TurnHost -> Bool
isHostName (TurnHostIp _) = False
isHostName (TurnHostName _) = True

parseTurnHost :: Text -> Maybe TurnHost
parseTurnHost h = case BC.fromByteString host of
  Just ip@(IpAddr _) -> Just $ TurnHostIp ip
  Nothing | validHostname host -> Just $ TurnHostName h -- NOTE: IP addresses are also valid hostnames
  _ -> Nothing
  where
    host = TE.encodeUtf8 h

data Transport
  = TransportUDP
  | TransportTCP
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Transport)

instance BC.ToByteString Transport where
  builder TransportUDP = "udp"
  builder TransportTCP = "tcp"

instance BC.FromByteString Transport where
  parser =
    BC.parser >>= \t -> case (t :: ByteString) of
      "udp" -> pure TransportUDP
      "tcp" -> pure TransportTCP
      _ -> fail $ "Invalid turn transport: " ++ show t

instance ToJSON Transport where
  toJSON = String . TE.decodeUtf8 . BC.toByteString'

instance FromJSON Transport where
  parseJSON =
    withText "Transport" $
      either fail pure . BC.runParser BC.parser . TE.encodeUtf8

--------------------------------------------------------------------------------
-- TurnUsername

data TurnUsername = TurnUsername
  { -- | must be positive, integral number of seconds
    _tuExpiresAt :: POSIXTime,
    _tuVersion :: Word,
    -- | seems to large, but uint32_t is used in C
    _tuKeyindex :: Word32,
    -- | undocumented, always 's'
    _tuT :: Char,
    -- | [a-z0-9]+
    _tuRandom :: Text
  }
  deriving stock (Eq, Show, Generic)

-- note that the random value is not checked for well-formedness
turnUsername :: POSIXTime -> Text -> TurnUsername
turnUsername expires rnd =
  TurnUsername
    { _tuExpiresAt = expires,
      _tuVersion = 1,
      _tuKeyindex = 0,
      _tuT = 's',
      _tuRandom = rnd
    }

instance ToJSON TurnUsername where
  toJSON = String . view utf8 . BC.toByteString'

instance FromJSON TurnUsername where
  parseJSON =
    withText "TurnUsername" $
      either fail pure . parseOnly (parseTurnUsername <* endOfInput)

instance BC.ToByteString TurnUsername where
  builder tu =
    shortByteString "d="
      <> word64Dec (round (_tuExpiresAt tu))
      <> shortByteString ".v="
      <> wordDec (_tuVersion tu)
      <> shortByteString ".k="
      <> word32Dec (_tuKeyindex tu)
      <> shortByteString ".t="
      <> charUtf8 (_tuT tu)
      <> shortByteString ".r="
      <> byteString (view (re utf8) (_tuRandom tu))

parseTurnUsername :: Parser TurnUsername
parseTurnUsername =
  TurnUsername
    <$> (string "d=" *> fmap (fromIntegral :: Word64 -> POSIXTime) decimal)
    <*> (string ".v=" *> decimal)
    <*> (string ".k=" *> decimal)
    <*> (string ".t=" *> anyChar)
    <*> (string ".r=" *> takeWhile1 (inClass "a-z0-9"))

instance Arbitrary TurnUsername where
  arbitrary =
    TurnUsername
      <$> (fromIntegral <$> arbitrary @Word64)
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> (Text.pack <$> QC.listOf1 genAlphaNum)
    where
      genAlphaNum = QC.elements $ ['a' .. 'z'] <> ['0' .. '9']

--------------------------------------------------------------------------------
-- convenience

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
limitServers uris limit = limitServers' [] limit uris
  where
    limitServers' acc x _ | x <= 0 = acc -- Already have accumulated enough
    limitServers' acc _ [] = acc -- No more input
    limitServers' acc _ input = do
      let (udps, noUdps) = partition isUdp input
          (udp, forTls) = (Imports.take 1 udps, noUdps ++ drop 1 udps)
          (tlss, noTlss) = partition isTls forTls
          (tls, forTcp) = (Imports.take 1 tlss, noTlss ++ drop 1 tlss)
          (tcps, noTcps) = partition isTcp forTcp
          (tcp, rest) = (Imports.take 1 tcps, noTcps ++ drop 1 tcps)
          new = udp ++ tls ++ tcp
          newAcc = Imports.take limit $ acc ++ new
      if null new -- Did we find anything interesting? If not, time to go
        then Imports.take limit $ acc ++ rest
        else limitServers' newAcc (limit - length newAcc) rest

isUdp :: TurnURI -> Bool
isUdp uri =
  _turiScheme uri == SchemeTurn
    && ( _turiTransport uri == Just (TransportUDP)
           || _turiTransport uri == Nothing
       )

isTcp :: TurnURI -> Bool
isTcp uri =
  _turiScheme uri == SchemeTurn
    && _turiTransport uri == Just (TransportTCP)

isTls :: TurnURI -> Bool
isTls uri =
  _turiScheme uri == SchemeTurns
    && _turiTransport uri == Just (TransportTCP)

makeLenses ''RTCConfiguration
makeLenses ''RTCIceServer
makeLenses ''TurnURI
makeLenses ''TurnUsername
