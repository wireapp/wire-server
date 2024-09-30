module Wire.API.Presence (Presence (..), URI (..), parse) where

import Control.Lens ((?~))
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Data.Attoparsec.ByteString (takeByteString)
import Data.ByteString.Char8 qualified as Bytes
import Data.ByteString.Conversion
import Data.ByteString.Lazy qualified as Lazy
import Data.Id
import Data.Misc (Milliseconds)
import Data.OpenApi qualified as S
import Data.Proxy
import Data.Schema
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Imports
import Network.URI qualified as Net
import Servant.API (ToHttpApiData (toUrlPiece))

-- FUTUREWORK: use Network.URI and toss this newtype.  servant should have all these instances for us these days.
newtype URI = URI
  { fromURI :: Net.URI
  }
  deriving (Eq, Ord, Show)

instance A.FromJSON URI where
  parseJSON = A.withText "URI" (parse . Text.unpack)

instance A.ToJSON URI where
  toJSON uri = A.String $ Text.pack (show (fromURI uri))

instance ToByteString URI where
  builder = builder . show . fromURI

instance FromByteString URI where
  parser = takeByteString >>= parse . Bytes.unpack

instance ToHttpApiData URI where
  toUrlPiece = decodeUtf8 . toByteString'

instance S.ToParamSchema URI where
  toParamSchema _ =
    S.toParamSchema (Proxy @Text)
      & S.type_ ?~ S.OpenApiString
      & S.description ?~ "Valid URI"

parse :: (MonadFail m) => String -> m URI
parse = maybe (fail "Invalid URI") (pure . URI) . Net.parseURI

-- | This is created in gundeck by cannon every time the client opens a new websocket connection.
-- (That's why we always have a 'ConnId' from the most recent connection by that client.)
data Presence = Presence
  { userId :: !UserId,
    connId :: !ConnId,
    -- | cannon instance hosting the presence
    resource :: !URI,
    -- | This is 'Nothing' if either (a) the presence is older
    -- than mandatory end-to-end encryption, or (b) the client is
    -- operating the team settings pages without the need for
    -- end-to-end crypto.
    clientId :: !(Maybe ClientId),
    createdAt :: !Milliseconds,
    -- | REFACTOR: temp. addition to ease migration
    __field :: !Lazy.ByteString
  }
  deriving (Eq, Ord, Show)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via (Schema Presence)

instance ToSchema Presence where
  schema =
    object "Presence" $
      ( Presence
          <$> userId .= field "user_id" schema
          <*> connId .= field "device_id" schema
          <*> resource .= field "resource" uriSchema
          <*> clientId .= maybe_ (optField "client_id" schema)
          <*> createdAt .= field "created_at" schema
      )
        <&> ($ ("" :: Lazy.ByteString))

uriSchema :: ValueSchema NamedSwaggerDoc URI
uriSchema = mkSchema desc uriFromJSON (Just . uriToJSON)
  where
    desc :: NamedSwaggerDoc
    desc =
      swaggerDoc @Text
        & (S.schema . S.type_ ?~ S.OpenApiString)
        & (S.schema . S.description ?~ "Valid URI.")

uriFromJSON :: A.Value -> A.Parser URI
uriFromJSON = A.withText "URI" (p . Text.unpack)
  where
    p :: (MonadFail m) => String -> m URI
    p = maybe (fail "Invalid URI") pure . parse

uriToJSON :: URI -> A.Value
uriToJSON uri = A.String $ Text.pack (show uri)
