{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.History
  ( History (..),
    historyConfig,
    HistorySharingConfig (..),
    HistoryDuration (..),
  )
where

import Cassandra qualified as C
import Control.Lens (makePrisms)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Attoparsec.Text qualified as A
import Data.Default
import Data.OpenApi qualified as S
import Data.Schema
import Data.Text qualified as T
import Imports
import Wire.Arbitrary

data History = HistoryPrivate | HistoryShared HistorySharingConfig
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via GenericUniform History

historyConfig :: History -> Maybe HistorySharingConfig
historyConfig HistoryPrivate = Nothing
historyConfig (HistoryShared cfg) = Just cfg

instance Default History where
  def = HistoryPrivate

data HistorySharingConfig = HistorySharingConfig
  { depth :: HistoryDuration
  }
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via GenericUniform HistorySharingConfig

data HistoryDuration
  = HistoryDurationInfinite
  | HistoryDurationFinite Int64
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via GenericUniform HistoryDuration

historyDurationToText :: HistoryDuration -> Text
historyDurationToText HistoryDurationInfinite = "infinite"
historyDurationToText (HistoryDurationFinite secs) = T.show secs

historyDurationFromText :: Text -> Either String HistoryDuration
historyDurationFromText =
  A.parseOnly $
    (HistoryDurationInfinite <$ A.string "infinite")
      <|> (HistoryDurationFinite <$> A.decimal)

instance C.Cql HistoryDuration where
  ctype = C.Tagged C.BigIntColumn
  toCql HistoryDurationInfinite = C.CqlBigInt 0
  toCql (HistoryDurationFinite secs) = C.CqlBigInt secs
  fromCql (C.CqlBigInt n)
    | n == 0 = pure HistoryDurationInfinite
    | n > 0 = pure (HistoryDurationFinite n)
    | n < 0 = Left "duration: positive bigint expected"
  fromCql _ = Left "duration: bigint expected"

makePrisms ''History

instance ToSchema History where
  schema =
    named "History" $
      tag _HistoryPrivate null_ <> tag _HistoryShared (unnamed schema)

instance ToSchema HistorySharingConfig where
  schema =
    object "HistorySharingConfig" $
      HistorySharingConfig
        <$> (.depth) .= field "depth" schema

instance ToSchema HistoryDuration where
  schema = historyDurationToText .= parsedText "HistoryDuration" historyDurationFromText

deriving via Schema History instance FromJSON History

deriving via Schema History instance ToJSON History

deriving via Schema History instance S.ToSchema History
