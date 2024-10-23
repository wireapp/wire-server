module Wire.API.User.Phone
  ( Phone (..),
    parsePhone,
    isValidPhone,
  )
where

import Cassandra qualified as C
import Control.Applicative (optional)
import Control.Lens (over, (?~))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Attoparsec.Text
import Data.ByteString.Conversion
import Data.OpenApi (ToParamSchema (..))
import Data.OpenApi qualified as S
import Data.Schema
import Data.Text qualified as Text
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Imports
import SAML2.WebSSO.Test.Arbitrary ()
import Servant
import Servant.API qualified as S
import Test.QuickCheck qualified as QC
import Web.Scim.Schema.User.Email ()
import Wire.Arbitrary (Arbitrary (arbitrary))

--------------------------------------------------------------------------------
-- Phone

newtype Phone = Phone {fromPhone :: Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema Phone)

instance ToParamSchema Phone where
  toParamSchema _ = toParamSchema (Proxy @Text)

instance ToSchema Phone where
  schema =
    over doc (S.description ?~ "E.164 phone number") $
      fromPhone
        .= parsedText "PhoneNumber" (maybe (Left "Invalid phone number. Expected E.164 format.") Right . parsePhone)

instance ToByteString Phone where
  builder = builder . fromPhone

instance FromByteString Phone where
  parser = parser >>= maybe (fail "Invalid phone") pure . parsePhone

instance S.FromHttpApiData Phone where
  parseUrlPiece = maybe (Left "Invalid phone") Right . fromByteString . encodeUtf8

instance S.ToHttpApiData Phone where
  toUrlPiece = decodeUtf8With lenientDecode . toByteString'

instance Arbitrary Phone where
  arbitrary =
    Phone . Text.pack <$> do
      let mkdigits n = replicateM n (QC.elements ['0' .. '9'])
      mini <- mkdigits 8
      maxi <- mkdigits =<< QC.chooseInt (0, 7)
      pure $ '+' : mini <> maxi

deriving instance C.Cql Phone

-- | Parses a phone number in E.164 format with a mandatory leading '+'.
parsePhone :: Text -> Maybe Phone
parsePhone p
  | isValidPhone p = Just $! Phone p
  | otherwise = Nothing

-- | Checks whether a phone number is valid, i.e. it is in E.164 format
-- with a mandatory leading '+' followed by 10-15 digits.
isValidPhone :: Text -> Bool
isValidPhone = either (const False) (const True) . parseOnly e164
  where
    e164 = char '+' *> count 8 digit *> count 7 (optional digit) *> endOfInput
