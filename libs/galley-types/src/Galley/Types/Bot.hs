{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Galley.Types.Bot
  ( AddBot,
    addBot,
    addBotService,
    addBotConv,
    addBotId,
    addBotClient,
    RemoveBot,
    removeBot,
    rmBotConv,
    rmBotId,
    BotConvView,
    botConvView,
    botConvId,
    botConvName,
    botConvMembers,

    -- * Re-exports
    module Service,
  )
where

import Control.Lens (makeLenses)
import Data.Aeson
import Data.Id
import Data.Json.Util ((#))
import Galley.Types (OtherMember (..))
import Galley.Types.Bot.Service as Service
import Imports

-- AddBot ----------------------------------------------------------------------

data AddBot
  = AddBot
      { _addBotService :: !ServiceRef,
        _addBotConv :: !ConvId,
        _addBotId :: !BotId,
        _addBotClient :: !ClientId
      }

makeLenses ''AddBot

addBot :: ServiceRef -> ConvId -> BotId -> ClientId -> AddBot
addBot = AddBot

instance FromJSON AddBot where
  parseJSON = withObject "AddBot" $ \o ->
    AddBot <$> o .: "service"
      <*> o .: "conversation"
      <*> o .: "bot"
      <*> o .: "client"

instance ToJSON AddBot where
  toJSON a =
    object
      [ "service" .= _addBotService a,
        "conversation" .= _addBotConv a,
        "bot" .= _addBotId a,
        "client" .= _addBotClient a
      ]

-- RemoveBot ------------------------------------------------------------------

data RemoveBot
  = RemoveBot
      { _rmBotConv :: !ConvId,
        _rmBotId :: !BotId
      }

makeLenses ''RemoveBot

removeBot :: ConvId -> BotId -> RemoveBot
removeBot = RemoveBot

instance FromJSON RemoveBot where
  parseJSON = withObject "RemoveBot" $ \o ->
    RemoveBot <$> o .: "conversation"
      <*> o .: "bot"

instance ToJSON RemoveBot where
  toJSON a =
    object
      [ "conversation" .= _rmBotConv a,
        "bot" .= _rmBotId a
      ]

-- BotConvView -----------------------------------------------------------------

-- | A conversation as seen by a bot.
data BotConvView
  = BotConvView
      { _botConvId :: !ConvId,
        _botConvName :: !(Maybe Text),
        _botConvMembers :: ![OtherMember]
      }
  deriving (Eq, Show)

makeLenses ''BotConvView

botConvView :: ConvId -> Maybe Text -> [OtherMember] -> BotConvView
botConvView = BotConvView

instance FromJSON BotConvView where
  parseJSON = withObject "BotConvView" $ \o ->
    BotConvView <$> o .: "id"
      <*> o .:? "name"
      <*> o .: "members"

instance ToJSON BotConvView where
  toJSON c =
    object $
      "id" .= _botConvId c
        # "name" .= _botConvName c
        # "members" .= _botConvMembers c
        # []
