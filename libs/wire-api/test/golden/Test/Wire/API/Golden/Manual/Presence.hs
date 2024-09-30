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

module Test.Wire.API.Golden.Manual.Presence
  ( testObject_Presence_1,
    testObject_Presence_2,
  )
where

import Data.Aeson
import Data.Attoparsec.ByteString (takeByteString)
import Data.ByteString.Char8 qualified as Bytes
import Data.ByteString.Conversion
import Data.ByteString.Lazy as Lazy
import Data.Id
import Data.Misc
import Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Data.UUID qualified as UUID
import Imports
import Network.URI qualified as Net
import Servant.API qualified as Servant

newtype URI = URI
  { fromURI :: Net.URI
  }
  deriving (Eq, Ord, Show)

instance FromJSON URI where
  parseJSON = withText "URI" (parse . Text.unpack)

instance ToJSON URI where
  toJSON uri = String $ Text.pack (show (fromURI uri))

instance ToByteString URI where
  builder = builder . show . fromURI

instance FromByteString URI where
  parser = takeByteString >>= parse . Bytes.unpack

instance Servant.ToHttpApiData URI where
  toUrlPiece = decodeUtf8 . toByteString'

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

instance ToJSON Presence where
  toJSON p =
    object
      [ "user_id" .= userId p,
        "device_id" .= connId p,
        "resource" .= resource p,
        "client_id" .= clientId p,
        "created_at" .= createdAt p
      ]

instance FromJSON Presence where
  parseJSON = withObject "Presence" $ \o ->
    Presence
      <$> o .: "user_id"
      <*> o .: "device_id"
      <*> o .: "resource"
      <*> o .:? "client_id"
      <*> o .:? "created_at" .!= 0
      <*> pure ""

testObject_Presence_1 :: Presence
testObject_Presence_1 =
  Presence
    (Id . fromJust $ UUID.fromString "174ccaea-7f26-11ef-86cc-27bb6bf3b319")
    (ConnId "wef")
    (fromJust $ parse "http://example.com/")
    Nothing
    0
    ""

testObject_Presence_2 :: Presence
testObject_Presence_2 =
  Presence
    (Id . fromJust $ UUID.fromString "174ccaea-7f26-11ef-86cc-37bb6bf3b319")
    (ConnId "wef3")
    (fromJust $ parse "http://example.com/3")
    (Just (ClientId 1))
    12323
    "" -- __field always has to be "", see ToSchema instance.
