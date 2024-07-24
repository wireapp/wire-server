{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Call.Config
  ( -- * RTCConfiguration
    RTCConfiguration,
    rtcConfiguration,
    rtcConfIceServers,
    rtcConfSftServers,
    rtcConfSftServersAll,
    rtcConfTTL,
    rtcConfIsFederating,

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

    -- * SFTUsername
    SFTUsername,
    mkSFTUsername,
    suExpiresAt,
    suVersion,
    suKeyindex,
    suShared,
    suRandom,

    -- * TurnUsername
    TurnUsername,
    turnUsername,
    tuExpiresAt,
    tuVersion,
    tuKeyindex,
    tuT,
    tuRandom,

    -- * SFTServer
    SFTServer,
    sftServer,
    sftURL,

    -- * AuthSFTServer
    AuthSFTServer,
    authSFTServer,
    nauthSFTServer,
    authURL,
    authUsername,
    authCredential,

    -- * convenience
    isUdp,
    isTcp,
    isTls,
    limitServers,
  )
where

import Control.Applicative (optional)
import Control.Lens hiding (element, enum, (.=))
import Data.Aeson qualified as A hiding ((<?>))
import Data.Aeson.Types qualified as A
import Data.Attoparsec.Text hiding (Parser, parse)
import Data.Attoparsec.Text qualified as Text
import Data.ByteString (toStrict)
import Data.ByteString.Builder
import Data.ByteString.Conversion (toByteString)
import Data.ByteString.Conversion qualified as BC
import Data.IP qualified as IP
import Data.List.NonEmpty (NonEmpty)
import Data.Misc (HttpsUrl (..), IpAddr (IpAddr), Port (..))
import Data.OpenApi qualified as S
import Data.Schema
import Data.Text qualified as Text
import Data.Text.Ascii
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error
import Data.Text.Strict.Lens (utf8)
import Data.Time.Clock.POSIX
import Imports
import Test.QuickCheck qualified as QC
import Text.Hostname (validHostname)
import Wire.Arbitrary (Arbitrary (arbitrary), GenericUniform (..))

--------------------------------------------------------------------------------
-- RTCConfiguration

-- | A configuration object resembling \"RTCConfiguration\"
--
-- The \"ttl\" field is a proprietary extension
-- The \"sft_servers\" field is a proprietary extension
--
-- cf. https://developer.mozilla.org/en-US/docs/Web/API/RTCPeerConnection/RTCPeerConnection#RTCConfiguration_dictionary
data RTCConfiguration = RTCConfiguration
  { _rtcConfIceServers :: NonEmpty RTCIceServer,
    _rtcConfSftServers :: Maybe (NonEmpty SFTServer),
    _rtcConfTTL :: Word32,
    _rtcConfSftServersAll :: Maybe [AuthSFTServer],
    _rtcConfIsFederating :: Maybe Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform RTCConfiguration)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema RTCConfiguration)

rtcConfiguration ::
  NonEmpty RTCIceServer ->
  Maybe (NonEmpty SFTServer) ->
  Word32 ->
  Maybe [AuthSFTServer] ->
  Maybe Bool ->
  RTCConfiguration
rtcConfiguration = RTCConfiguration

instance ToSchema RTCConfiguration where
  schema =
    objectWithDocModifier "RTCConfiguration" (description ?~ "A subset of the WebRTC 'RTCConfiguration' dictionary") $
      RTCConfiguration
        <$> _rtcConfIceServers
          .= fieldWithDocModifier "ice_servers" (description ?~ "Array of 'RTCIceServer' objects") (nonEmptyArray schema)
        <*> _rtcConfSftServers
          .= maybe_ (optFieldWithDocModifier "sft_servers" (description ?~ "Array of 'SFTServer' objects (optional)") (nonEmptyArray schema))
        <*> _rtcConfTTL
          .= fieldWithDocModifier "ttl" (description ?~ "Number of seconds after which the configuration should be refreshed (advisory)") schema
        <*> _rtcConfSftServersAll
          .= maybe_ (optFieldWithDocModifier "sft_servers_all" (description ?~ "Array of all SFT servers") (array schema))
        <*> _rtcConfIsFederating
          .= maybe_ (optFieldWithDocModifier "is_federating" (description ?~ "True if the client should connect to an SFT in the sft_servers_all and request it to federate") schema)

--------------------------------------------------------------------------------
-- SFTServer

newtype SFTServer = SFTServer
  { _sftURL :: HttpsUrl
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving (Arbitrary) via (GenericUniform SFTServer)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema SFTServer)

instance ToSchema SFTServer where
  schema =
    objectWithDocModifier "SftServer" (description ?~ "Inspired by WebRTC 'RTCIceServer' object, contains details of SFT servers") $
      SFTServer
        <$> (pure . _sftURL)
          .= fieldWithDocModifier "urls" (description ?~ "Array containing exactly one SFT server address of the form 'https://<addr>:<port>'") (withParser (array schema) p)
    where
      p :: [HttpsUrl] -> A.Parser HttpsUrl
      p [url] = pure url
      p xs = fail $ "SFTServer can only have exactly one URL, found " <> show (length xs)

sftServer :: HttpsUrl -> SFTServer
sftServer = SFTServer

--------------------------------------------------------------------------------
-- AuthSFTServer

data AuthSFTServer = AuthSFTServer
  { _authURL :: HttpsUrl,
    _authUsername :: Maybe SFTUsername,
    _authCredential :: Maybe AsciiBase64
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving (Arbitrary) via (GenericUniform AuthSFTServer)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema AuthSFTServer)

instance ToSchema AuthSFTServer where
  schema =
    objectWithDocModifier "SftServer" (description ?~ "Inspired by WebRTC 'RTCIceServer' object, contains details of SFT servers") $
      AuthSFTServer
        <$> (pure . _authURL)
          .= fieldWithDocModifier "urls" (description ?~ "Array containing exactly one SFT server address of the form 'https://<addr>:<port>'") (withParser (array schema) p)
        <*> _authUsername
          .= maybe_ (optFieldWithDocModifier "username" (description ?~ "String containing the SFT username") schema)
        <*> _authCredential
          .= maybe_ (optFieldWithDocModifier "credential" (description ?~ "String containing the SFT credential") schema)
    where
      p :: [HttpsUrl] -> A.Parser HttpsUrl
      p [url] = pure url
      p xs = fail $ "SFTServer can only have exactly one URL, found " <> show (length xs)

nauthSFTServer :: SFTServer -> AuthSFTServer
nauthSFTServer = (\u -> AuthSFTServer u Nothing Nothing) . _sftURL

authSFTServer :: SFTServer -> SFTUsername -> AsciiBase64 -> AuthSFTServer
authSFTServer svr u = AuthSFTServer (_sftURL svr) (Just u) . Just

--------------------------------------------------------------------------------
-- RTCIceServer

-- | A configuration object resembling \"RTCIceServer\"
--
-- cf. https://developer.mozilla.org/en-US/docs/Web/API/RTCIceServer
data RTCIceServer = RTCIceServer
  { _iceURLs :: NonEmpty TurnURI,
    _iceUsername :: TurnUsername,
    _iceCredential :: AsciiBase64
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform RTCIceServer)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema RTCIceServer)

rtcIceServer :: NonEmpty TurnURI -> TurnUsername -> AsciiBase64 -> RTCIceServer
rtcIceServer = RTCIceServer

instance ToSchema RTCIceServer where
  schema =
    objectWithDocModifier "RTCIceServer" (description ?~ "A subset of the WebRTC 'RTCIceServer' object") $
      RTCIceServer
        <$> _iceURLs
          .= fieldWithDocModifier "urls" (description ?~ "Array of TURN server addresses of the form 'turn:<addr>:<port>'") (nonEmptyArray schema)
        <*> _iceUsername
          .= fieldWithDocModifier "username" (description ?~ "Username to use for authenticating against the given TURN servers") schema
        <*> _iceCredential
          .= fieldWithDocModifier "credential" (description ?~ "Password to use for authenticating against the given TURN servers") schema

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
  deriving stock (Eq, Show, Ord, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema TurnURI)

instance ToSchema TurnURI where
  schema =
    (TE.decodeUtf8With lenientDecode . toStrict . toByteString)
      .= parsedText "TurnURI" parseTurnURI

turnURI :: Scheme -> TurnHost -> Port -> Maybe Transport -> TurnURI
turnURI = TurnURI

instance BC.ToByteString TurnURI where
  builder (TurnURI s h (Port p) tp) =
    BC.builder s
      <> byteString ":"
      <> BC.builder h
      <> byteString ":"
      <> BC.builder p
      <> foldMap ((byteString "?transport=" <>) . BC.builder) tp

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
        <*> (optional ((string "?transport=" *> takeText) >>= parseTransport) <?> "parsingTransport")
    parseScheme = parse "parseScheme"
    parseHost = parse "parseHost"
    parseTransport = parse "parseTransport"
    parse :: (BC.FromByteString b, MonadFail m) => String -> Text -> m b
    parse err x = case BC.fromByteString (TE.encodeUtf8 x) of
      Just ok -> pure ok
      Nothing -> fail (err ++ " failed when parsing: " ++ show x)

instance Arbitrary TurnURI where
  arbitrary = (getGenericUniform <$> arbitrary) `QC.suchThat` (not . isIPv6)
    where
      isIPv6 h = case _turiHost h of
        TurnHostIp (IpAddr (IP.IPv6 _)) -> True
        _ -> False

data Scheme
  = SchemeTurn
  | SchemeTurns
  deriving stock (Eq, Show, Ord, Generic)
  deriving (Arbitrary) via (GenericUniform Scheme)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema Scheme)

instance BC.ToByteString Scheme where
  builder SchemeTurn = "turn"
  builder SchemeTurns = "turns"

instance BC.FromByteString Scheme where
  parser =
    BC.parser >>= \t -> case (t :: ByteString) of
      "turn" -> pure SchemeTurn
      "turns" -> pure SchemeTurns
      _ -> fail $ "Invalid turn scheme: " ++ show t

instance ToSchema Scheme where
  schema =
    enum @Text "Scheme" $
      mconcat
        [ element "turn" SchemeTurn,
          element "turns" SchemeTurns
        ]

data TurnHost
  = TurnHostIp IpAddr
  | TurnHostName Text
  deriving stock (Eq, Show, Ord, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema TurnHost)

instance ToSchema TurnHost where
  schema = turnHostSchema

data TurnHostTag = TurnHostIpTag | TurnHostNameTag
  deriving (Eq, Enum, Bounded)

tagSchema :: ValueSchema NamedSwaggerDoc TurnHostTag
tagSchema =
  enum @Text "TurnHostTag" $
    mconcat
      [ element "TurnHostIp" TurnHostIpTag,
        element "TurnHostName" TurnHostNameTag
      ]

turnHostSchema :: ValueSchema NamedSwaggerDoc TurnHost
turnHostSchema =
  object "TurnHost" $
    fromTagged
      <$> toTagged
        .= bind
          (fst .= field "tag" tagSchema)
          (snd .= fieldOver _1 "contents" untaggedSchema)
  where
    toTagged :: TurnHost -> (TurnHostTag, TurnHost)
    toTagged d@(TurnHostIp _) = (TurnHostIpTag, d)
    toTagged d@(TurnHostName _) = (TurnHostNameTag, d)

    fromTagged :: (TurnHostTag, TurnHost) -> TurnHost
    fromTagged = snd

    untaggedSchema = dispatch $ \case
      TurnHostIpTag -> tag _TurnHostIp (unnamed schema)
      TurnHostNameTag -> tag _TurnHostName (unnamed schema)

    _TurnHostIp :: Prism' TurnHost IpAddr
    _TurnHostIp = prism' TurnHostIp $ \case
      TurnHostIp a -> Just a
      _ -> Nothing

    _TurnHostName :: Prism' TurnHost Text
    _TurnHostName = prism' TurnHostName $ \case
      TurnHostName b -> Just b
      _ -> Nothing

instance BC.FromByteString TurnHost where
  parser = BC.parser >>= maybe (fail "Invalid turn host") pure . parseTurnHost

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
  deriving stock (Eq, Show, Ord, Generic)
  deriving (Arbitrary) via (GenericUniform Transport)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema Transport)

instance BC.ToByteString Transport where
  builder TransportUDP = "udp"
  builder TransportTCP = "tcp"

instance BC.FromByteString Transport where
  parser =
    BC.parser >>= \t -> case (t :: ByteString) of
      "udp" -> pure TransportUDP
      "tcp" -> pure TransportTCP
      _ -> fail $ "Invalid turn transport: " ++ show t

instance ToSchema Transport where
  schema =
    enum @Text "Transport" $
      mconcat
        [ element "udp" TransportUDP,
          element "tcp" TransportTCP
        ]

--------------------------------------------------------------------------------
-- SFTUsername

data SFTUsername = SFTUsername
  { -- | must be positive, integral number of seconds
    _suExpiresAt :: POSIXTime,
    _suVersion :: Word,
    -- | seems to large, but uint32_t is used in C
    _suKeyindex :: Word32,
    -- | whether the user is allowed to initialise an SFT conference
    _suShared :: Bool,
    -- | [a-z0-9]+
    _suRandom :: Text
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema SFTUsername)

-- note that the random value is not checked for well-formedness
mkSFTUsername :: Bool -> POSIXTime -> Text -> SFTUsername
mkSFTUsername shared expires rnd =
  SFTUsername
    { _suExpiresAt = expires,
      _suVersion = 1,
      _suKeyindex = 0,
      _suShared = shared,
      _suRandom = rnd
    }

instance ToSchema SFTUsername where
  schema = toText .= parsedText "" fromText
    where
      fromText :: Text -> Either String SFTUsername
      fromText = parseOnly (parseSFTUsername <* endOfInput)

      toText :: SFTUsername -> Text
      toText = TE.decodeUtf8With lenientDecode . toStrict . toByteString

instance BC.ToByteString SFTUsername where
  builder su =
    shortByteString "d="
      <> word64Dec (round (_suExpiresAt su))
      <> shortByteString ".v="
      <> wordDec (_suVersion su)
      <> shortByteString ".k="
      <> word32Dec (_suKeyindex su)
      <> shortByteString ".s="
      <> wordDec (boolToWord $ _suShared su)
      <> shortByteString ".r="
      <> byteString (view (re utf8) (_suRandom su))
    where
      boolToWord :: (Num a) => Bool -> a
      boolToWord False = 0
      boolToWord True = 1

parseSFTUsername :: Text.Parser SFTUsername
parseSFTUsername =
  SFTUsername
    <$> (string "d=" *> fmap (fromIntegral :: Word64 -> POSIXTime) decimal)
    <*> (string ".v=" *> decimal)
    <*> (string ".k=" *> decimal)
    <*> (string ".s=" *> (wordToBool <$> decimal))
    <*> (string ".r=" *> takeWhile1 (inClass "a-z0-9"))
  where
    wordToBool :: Word -> Bool
    wordToBool = odd

instance Arbitrary SFTUsername where
  arbitrary =
    SFTUsername
      <$> (fromIntegral <$> arbitrary @Word64)
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> (Text.pack <$> QC.listOf1 genAlphaNum)
    where
      genAlphaNum = QC.elements $ ['a' .. 'z'] <> ['0' .. '9']

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
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema TurnUsername)

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

instance ToSchema TurnUsername where
  schema = toText .= parsedText "" fromText
    where
      fromText :: Text -> Either String TurnUsername
      fromText = parseOnly (parseTurnUsername <* endOfInput)

      toText :: TurnUsername -> Text
      toText = TE.decodeUtf8With lenientDecode . toStrict . toByteString

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

parseTurnUsername :: Text.Parser TurnUsername
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
    && ( _turiTransport uri == Just TransportUDP
           || isNothing (_turiTransport uri)
       )

isTcp :: TurnURI -> Bool
isTcp uri =
  _turiScheme uri == SchemeTurn
    && _turiTransport uri == Just TransportTCP

isTls :: TurnURI -> Bool
isTls uri =
  _turiScheme uri == SchemeTurns
    && _turiTransport uri == Just TransportTCP

makeLenses ''RTCConfiguration
makeLenses ''RTCIceServer
makeLenses ''TurnURI
makeLenses ''SFTUsername
makeLenses ''TurnUsername
makeLenses ''SFTServer
makeLenses ''AuthSFTServer
