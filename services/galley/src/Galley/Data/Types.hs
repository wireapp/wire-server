{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Galley.Data.Types
    ( Conversation (..)
    , isSelfConv
    , isO2OConv
    , isTeamConv
    , isConvDeleted
    , selfConv
    , Code (..)
    , toCode
    , Join (..)
    ) where

import Brig.Types.Code
import Data.Aeson hiding (Value)
import Data.Int (Int32)
import Data.Json.Util
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

--------------------------------------------------------------------------------
-- Code

data Code = Code
    { codeKey           :: !Key
    , codeValue         :: !Value
    , codeTTL           :: !Timeout
    , codeConversation  :: !ConvId
    } deriving (Eq, Show)


toCode :: Key -> (Value, Int32, ConvId) -> Code
toCode k (val, ttl, cnv) = Code
        { codeKey = k
        , codeValue = val
        , codeTTL = Timeout (fromIntegral ttl)
        , codeConversation = cnv
        }

data Join = Join
    { conversationKey   :: !Key
    , conversationCode  :: !Value
    } deriving (Eq, Show)

instance ToJSON Join where
    toJSON j = object
        $ "conversationKey"  .= conversationKey j
        # "conversationCode" .= conversationCode j
        # []

instance FromJSON Join where
    parseJSON = withObject "join" $ \o ->
        Join <$> o .: "conversationKey"
            <*> o .: "conversationCode"
