{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}

module Brig.Types.Connection
    ( module Brig.Types.Connection
    , module C
    ) where

import Brig.Types.Common as C
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Attoparsec.ByteString (takeByteString)
import Data.ByteString.Conversion
import Data.Id
import Data.Json.Util
import Data.Monoid ((<>))
import Data.Range
import Data.Text (Text, pack, toLower)
import Data.Time.Clock (UTCTime)

newtype Message = Message { messageText :: Text }
    deriving (Eq, Ord, Show, ToJSON)

-- | Look at @services\/brig\/doc@ for descriptions of these states.
data Relation
    = Accepted
    | Blocked
    | Pending
    | Ignored
    | Sent
    | Cancelled
    deriving (Eq, Ord, Show)

data UserConnection = UserConnection
    { ucFrom       :: !UserId
    , ucTo         :: !UserId
    , ucStatus     :: !Relation
    , ucLastUpdate :: !UTCTime
    , ucMessage    :: !(Maybe Message)
    , ucConvId     :: !(Maybe ConvId)
    } deriving (Eq, Show)

data ConnectionRequest = ConnectionRequest
    { crUser    :: !UserId
    , crName    :: !Text   -- ^ Conversation name
    , crMessage :: !Message
    } deriving (Eq, Show)

data ConnectionUpdate = ConnectionUpdate
    { cuStatus :: !Relation
    } deriving (Eq, Show)

data InvitationRequest = InvitationRequest
    { irEmail    :: !Email
    , irName     :: !Name
    , irMessage  :: !Message
    , irLocale   :: !(Maybe Locale)
    } deriving (Eq, Show)

data Invitation = Invitation
    { inInviter    :: !UserId
    , inInvitation :: !InvitationId
    , inIdentity   :: !(Either Email Phone)
    , inCreatedAt  :: !UTCTime
    , inName       :: !Name
    } deriving (Eq, Show)

data UserConnectionList = UserConnectionList
    { clConnections :: [UserConnection]
    , clHasMore     :: !Bool
    } deriving (Eq, Show)

data InvitationList = InvitationList
    { ilInvitations :: [Invitation]
    , ilHasMore     :: !Bool
    } deriving (Eq, Show)

data UserIds = UserIds
    { cUsers :: [UserId] }

-- | Data that is passed to the @\/i\/users\/connections-status@ endpoint.
data ConnectionsStatusRequest = ConnectionsStatusRequest
    { csrFrom :: ![UserId]
    , csrTo   :: ![UserId]
    } deriving (Eq, Show)

-- * JSON Instances:

instance FromJSON Message where
    parseJSON x = Message . fromRange <$> (parseJSON x :: Parser (Range 1 256 Text))

instance ToJSON Relation where
    toJSON = String . toLower . pack . show

instance FromJSON Relation where
    parseJSON (String "accepted")  = return Accepted
    parseJSON (String "blocked")   = return Blocked
    parseJSON (String "pending")   = return Pending
    parseJSON (String "ignored")   = return Ignored
    parseJSON (String "sent")      = return Sent
    parseJSON (String "cancelled") = return Cancelled
    parseJSON _                    = mzero

instance FromByteString Relation where
    parser = takeByteString >>= \b -> case b of
        "accepted"  -> return Accepted
        "blocked"   -> return Blocked
        "pending"   -> return Pending
        "ignored"   -> return Ignored
        "sent"      -> return Sent
        "cancelled" -> return Cancelled
        x           -> fail $ "Invalid relation-type " <> show x

instance FromJSON UserConnection where
    parseJSON = withObject "user-connection" $ \o ->
        UserConnection <$> o .:  "from"
                       <*> o .:  "to"
                       <*> o .:  "status"
                       <*> o .:  "last_update"
                       <*> o .:? "message"
                       <*> o .:? "conversation"

instance ToJSON UserConnection where
    toJSON uc = object
        [ "from"         .= ucFrom uc
        , "to"           .= ucTo uc
        , "status"       .= ucStatus uc
        , "last_update"  .= UTCTimeMillis (ucLastUpdate uc)
        , "message"      .= ucMessage uc
        , "conversation" .= ucConvId uc
        ]

instance FromJSON ConnectionRequest where
    parseJSON = withObject "connection-request" $ \o ->
        ConnectionRequest <$> o .: "user"
                          <*> (fromRange <$> ((o .: "name") :: Parser (Range 1 256 Text)))
                          <*> o .: "message"

instance ToJSON ConnectionUpdate where
    toJSON c = object ["status" .= cuStatus c]

instance FromJSON ConnectionUpdate where
    parseJSON = withObject "connection-update" $ \o ->
        ConnectionUpdate <$> o .: "status"

instance ToJSON ConnectionRequest where
    toJSON c = object [ "user"    .= crUser c
                      , "name"    .= crName c
                      , "message" .= crMessage c
                      ]

instance FromJSON InvitationRequest where
    parseJSON = withObject "invitation-request" $ \o ->
        InvitationRequest <$> o .:  "email"
                          <*> o .:  "invitee_name"
                          <*> o .:  "message"
                          <*> o .:? "locale"

instance ToJSON InvitationRequest where
    toJSON i = object [ "email"        .= irEmail i
                      , "invitee_name" .= irName i
                      , "message"      .= irMessage i
                      , "locale"       .= irLocale i
                      ]

instance FromJSON Invitation where
    parseJSON = withObject "invitation" $ \o ->
        Invitation <$> o .: "inviter"
                   <*> o .: "id"
                   <*> parseInvitationIdentity o
                   <*> o .: "created_at"
                   <*> o .: "name"

instance ToJSON Invitation where
    toJSON i = object [ "inviter"       .= inInviter i
                      , "id"            .= inInvitation i
                      , either ("email" .=) ("phone" .=) (inIdentity i)
                      , "created_at"    .= UTCTimeMillis (inCreatedAt i)
                      , "name"          .= inName i
                      ]

parseInvitationIdentity :: Object -> Parser (Either Email Phone)
parseInvitationIdentity o = do
    email <- o .:? "email"
    phone <- o .:? "phone"
    case (email, phone) of
      (Just e , Nothing) -> return $ Left e
      (Nothing, Just p ) -> return $ Right p
      (_      , _      ) -> fail "Use either 'email' or 'phone'."

instance ToJSON UserConnectionList where
    toJSON (UserConnectionList l m) = object
        [ "connections" .= l
        , "has_more"    .= m
        ]

instance FromJSON UserConnectionList where
    parseJSON = withObject "UserConnectionList" $ \o ->
        UserConnectionList <$> o .: "connections"
                           <*> o .: "has_more"

instance ToJSON InvitationList where
    toJSON (InvitationList l m) = object
        [ "invitations" .= l
        , "has_more"    .= m
        ]

instance FromJSON InvitationList where
    parseJSON = withObject "InvitationList" $ \o ->
        InvitationList <$> o .: "invitations"
                       <*> o .: "has_more"

instance FromJSON UserIds where
    parseJSON = withObject "userids" $ \o ->
        UserIds <$> o .: "ids"

instance ToJSON UserIds where
    toJSON (UserIds us) = object
        [ "ids" .= us ]

instance FromJSON ConnectionsStatusRequest where
    parseJSON = withObject "ConnectionsStatusRequest" $ \o -> do
        csrFrom <- o .: "from"
        csrTo   <- o .: "to"
        pure ConnectionsStatusRequest{..}

instance ToJSON ConnectionsStatusRequest where
    toJSON ConnectionsStatusRequest{csrFrom, csrTo} = object
        [ "from" .= csrFrom
        , "to"   .= csrTo
        ]
