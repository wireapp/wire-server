{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Galley.Data.Types
    ( Conversation (..)
    , isSelfConv
    , isO2OConv
    , isTeamConv
    , isConvDeleted
    , selfConv
    ) where

import Data.Id
import Data.List1
import Data.Text
import Data.Maybe (fromMaybe, isJust)
import Galley.Types (ConvType (..), Access, Member (..))

data Conversation = Conversation
    { convId      :: ConvId
    , convType    :: ConvType
    , convCreator :: UserId
    , convName    :: Maybe Text
    , convAccess  :: List1 Access
    , convMembers :: [Member]
    , convTeam    :: Maybe TeamId
    , convDeleted :: Maybe Bool
    } deriving (Eq, Show)

isSelfConv :: Conversation -> Bool
isSelfConv = (SelfConv ==) . convType

isO2OConv :: Conversation -> Bool
isO2OConv = (One2OneConv ==) . convType

isTeamConv :: Conversation -> Bool
isTeamConv = isJust . convTeam

isConvDeleted :: Conversation -> Bool
isConvDeleted = fromMaybe False . convDeleted

selfConv :: UserId -> ConvId
selfConv uid = Id (toUUID uid)
