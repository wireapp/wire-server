module Data.LegalHold where

import Cassandra.CQL
import Data.Aeson
import qualified Data.Text as T
import Imports
import Test.QuickCheck

data UserLegalHoldStatus
  = UserLegalHoldDisabled
  | UserLegalHoldPending
  | UserLegalHoldEnabled
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic)

instance ToJSON UserLegalHoldStatus where
  toJSON UserLegalHoldDisabled = "disabled"
  toJSON UserLegalHoldPending = "pending"
  toJSON UserLegalHoldEnabled = "enabled"

instance FromJSON UserLegalHoldStatus where
  parseJSON = withText "UserLegalHoldStatus" $ \case
    "disabled" -> pure UserLegalHoldDisabled
    "pending" -> pure UserLegalHoldPending
    "enabled" -> pure UserLegalHoldEnabled
    x -> fail $ "unexpected status type: " <> T.unpack x

instance Cql UserLegalHoldStatus where
  ctype = Tagged IntColumn

  fromCql (CqlInt n) = case n of
    0 -> pure $ UserLegalHoldDisabled
    1 -> pure $ UserLegalHoldPending
    2 -> pure $ UserLegalHoldEnabled
    _ -> fail "fromCql: Invalid UserLegalHoldStatus"
  fromCql _ = fail "fromCql: UserLegalHoldStatus: CqlInt expected"

  toCql UserLegalHoldDisabled = CqlInt 0
  toCql UserLegalHoldPending = CqlInt 1
  toCql UserLegalHoldEnabled = CqlInt 2

instance Arbitrary UserLegalHoldStatus where
  arbitrary = elements [minBound ..]
