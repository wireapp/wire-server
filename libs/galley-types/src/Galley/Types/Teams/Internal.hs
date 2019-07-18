{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module Galley.Types.Teams.Internal where

import Imports
import Data.Json.Util
import Data.Aeson
import Data.Id (TeamId, UserId)
import Data.Range


data TeamBinding =
      Binding
    | NonBinding
    deriving (Eq, Show, Generic)

data Team = Team
    { _teamId      :: TeamId
    , _teamCreator :: UserId
    , _teamName    :: Text
    , _teamIcon    :: Text
    , _teamIconKey :: Maybe Text
    , _teamBinding :: TeamBinding
    } deriving (Eq, Show, Generic)

data NewTeam a = NewTeam
    { _newTeamName    :: Range 1 256 Text
    , _newTeamIcon    :: Range 1 256 Text
    , _newTeamIconKey :: Maybe (Range 1 256 Text)
    , _newTeamMembers :: Maybe a
    }
    deriving (Eq, Show, Generic)


instance ToJSON TeamBinding where
    toJSON Binding    = Bool True
    toJSON NonBinding = Bool False

instance FromJSON TeamBinding where
    parseJSON (Bool True)  = pure Binding
    parseJSON (Bool False) = pure NonBinding
    parseJSON other        = fail $ "Unknown binding type: " <> show other

instance ToJSON Team where
    toJSON t = object
        $ "id"       .= _teamId t
        # "creator"  .= _teamCreator t
        # "name"     .= _teamName t
        # "icon"     .= _teamIcon t
        # "icon_key" .= _teamIconKey t
        # "binding"  .= _teamBinding t
        # []

instance FromJSON Team where
    parseJSON = withObject "team" $ \o -> do
        Team <$> o .:  "id"
             <*> o .:  "creator"
             <*> o .:  "name"
             <*> o .:  "icon"
             <*> o .:? "icon_key"
             <*> o .:? "binding" .!= NonBinding

instance (FromJSON a) => FromJSON (NewTeam a) where
    parseJSON = withObject "new-team" $ \o -> do
        name <- o .:  "name"
        icon <- o .:  "icon"
        key  <- o .:? "icon_key"
        mems <- o .:? "members"
        either fail pure $ NewTeam <$> checkedEitherMsg "name" name
                                   <*> checkedEitherMsg "icon" icon
                                   <*> maybe (pure Nothing) (fmap Just . checkedEitherMsg "icon_key") key
                                   <*> pure mems
