{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Gundeck.Types.Presence
  ( module Gundeck.Types.Presence,
    module Common,
  )
where

import Data.Aeson
import qualified Data.ByteString.Lazy as Lazy
import Data.Id
import Data.Misc (Milliseconds)
import Gundeck.Types.Common as Common
import Imports

-- | This is created in gundeck by cannon every time the client opens a new websocket connection.
-- (That's why we always have a 'ConnId' from the most recent connection by that client.)
data Presence
  = Presence
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
