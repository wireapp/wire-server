{-# LANGUAGE OverloadedStrings #-}

module Brig.Types.Provider.External
    ( module Brig.Types.Provider.External

    , BotUserView (..)

      -- * Re-exports
    , BotConvView
    , botConvView
    , botConvId
    , botConvName
    , botConvMembers
    ) where

import Brig.Types.Client.Prekey
import Brig.Types.Common
import Data.Aeson
import Data.Id
import Data.Json.Util ((#))
import Data.Text (Text)
import Galley.Types.Bot

--------------------------------------------------------------------------------
-- NewBotRequest

-- | Request for a bot in a conversation.
data NewBotRequest = NewBotRequest
    { newBotId :: !BotId
        -- ^ The user ID to use for the bot.
    , newBotClient :: !ClientId
        -- ^ The client ID to use for the bot.
    , newBotOrigin :: !BotUserView
        -- ^ The origin (user) of the bot request.
    , newBotConv :: !BotConvView
        -- ^ The conversation as seen by the bot.
    , newBotToken :: !Text
        -- ^ The API access token.
    , newBotLocale :: !Locale
        -- ^ The preferred locale (i.e. language) for the bot
        -- to use.
    }

instance FromJSON NewBotRequest where
    parseJSON = withObject "NewBotRequest" $ \o ->
        NewBotRequest <$> o .: "id"
                      <*> o .: "client"
                      <*> o .: "origin"
                      <*> o .: "conversation"
                      <*> o .: "token"
                      <*> o .: "locale"

instance ToJSON NewBotRequest where
    toJSON n = object
        $ "id"           .= newBotId n
        # "client"       .= newBotClient n
        # "origin"       .= newBotOrigin n
        # "conversation" .= newBotConv n
        # "token"        .= newBotToken n
        # "locale"       .= newBotLocale n
        # []

--------------------------------------------------------------------------------
-- NewBotResponse

-- | Bot data provided by a service in response to a 'NewBotRequest'.
-- The returned optional data overrides the defaults taken from
-- the 'Service' definition.
data NewBotResponse = NewBotResponse
    { rsNewBotPrekeys    :: ![Prekey]
    , rsNewBotLastPrekey :: !LastPrekey
    , rsNewBotName       :: !(Maybe Name)
    , rsNewBotColour     :: !(Maybe ColourId)
    , rsNewBotAssets     :: !(Maybe [Asset])
    }

instance FromJSON NewBotResponse where
    parseJSON = withObject "NewBotResponse" $ \o ->
        NewBotResponse <$> o .:  "prekeys"
                       <*> o .:  "last_prekey"
                       <*> o .:? "name"
                       <*> o .:? "accent_id"
                       <*> o .:? "assets"

instance ToJSON NewBotResponse where
    toJSON r = object
        $ "prekeys"     .= rsNewBotPrekeys r
        # "last_prekey" .= rsNewBotLastPrekey r
        # "name"        .= rsNewBotName r
        # "accent_id"   .= rsNewBotColour r
        # "assets"      .= rsNewBotAssets r
        # []

--------------------------------------------------------------------------------
-- BotUserView

data BotUserView = BotUserView
    { botUserViewId     :: !UserId
    , botUserViewName   :: !Name
    , botUserViewColour :: !ColourId
    , botUserViewHandle :: !(Maybe Handle)
    , botUserViewTeam   :: !(Maybe TeamId)
    } deriving (Eq, Show)

instance FromJSON BotUserView where
    parseJSON = withObject "BotUserView" $ \o ->
        BotUserView <$> o .:  "id"
                    <*> o .:  "name"
                    <*> o .:  "accent_id"
                    <*> o .:? "handle"
                    <*> o .:? "team"

instance ToJSON BotUserView where
    toJSON u = object
        [ "id"        .= botUserViewId u
        , "name"      .= botUserViewName u
        , "accent_id" .= botUserViewColour u
        , "handle"    .= botUserViewHandle u
        , "team"      .= botUserViewTeam u
        ]

