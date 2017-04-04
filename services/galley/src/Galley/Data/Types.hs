{-# LANGUAGE OverloadedStrings #-}

module Galley.Data.Types
    ( Conversation (..)
    , isSelfConv
    , isO2OConv
    ) where

import Data.Id
import Data.List1
import Data.Text
import Galley.Types (ConvType (..), Access, Member (..))

data Conversation = Conversation
    { convId        :: !ConvId
    , convType      :: !ConvType
    , convCreator   :: !UserId
    , convName      :: !(Maybe Text)
    , convAccess    :: !(List1 Access)
    , convMembers   :: ![Member]
    } deriving (Eq, Show)

isSelfConv :: Conversation -> Bool
isSelfConv = (SelfConv ==) . convType

isO2OConv :: Conversation -> Bool
isO2OConv = (One2OneConv ==) . convType

