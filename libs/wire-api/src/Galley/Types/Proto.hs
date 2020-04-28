{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

-- FUTUREWORK: generate this file
module Galley.Types.Proto
  ( UserId,
    userId,
    fromUserId,
    ClientId,
    clientId,
    newClientId,
    fromClientId,
    toClientId,
    ClientEntry,
    clientEntry,
    clientEntryId,
    clientEntryMessage,
    UserEntry,
    userEntry,
    userEntryId,
    userEntryClients,
    toOtrRecipients,
    fromOtrRecipients,
    Priority (..),
    toPriority,
    fromPriority,
    NewOtrMessage,
    newOtrMessage,
    newOtrMessageSender,
    newOtrMessageRecipients,
    newOtrMessageNativePush,
    newOtrMessageNativePriority,
    newOtrMessageData,
    newOtrMessageTransient,
    newOtrMessageReportMissing,
    toNewOtrMessage,
  )
where

import Control.Lens (view)
import qualified Data.ByteString.Base64 as B64
import qualified Data.Id as Id
import qualified Data.Map.Strict as Map
import Data.ProtocolBuffers
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy.Read (hexadecimal)
import qualified Galley.Types as Galley
import Imports
import qualified Wire.API.Push as Gundeck

-- UserId -------------------------------------------------------------------

newtype UserId = UserId
  { _user :: Required 1 (Value Id.OpaqueUserId)
  }
  deriving (Eq, Show, Generic)

instance Encode UserId

instance Decode UserId

fromUserId :: Id.OpaqueUserId -> UserId
fromUserId u = UserId {_user = putField u}

userId :: Functor f => (Id.OpaqueUserId -> f Id.OpaqueUserId) -> UserId -> f UserId
userId f c = (\x -> c {_user = x}) <$> field f (_user c)

-- ClientId ------------------------------------------------------------------

newtype ClientId = ClientId
  { _client :: Required 1 (Value Word64)
  }
  deriving (Eq, Show, Generic)

instance Encode ClientId

instance Decode ClientId

newClientId :: Word64 -> ClientId
newClientId c = ClientId {_client = putField c}

clientId :: Functor f => (Word64 -> f Word64) -> ClientId -> f ClientId
clientId f c = (\x -> c {_client = x}) <$> field f (_client c)

toClientId :: ClientId -> Id.ClientId
toClientId c = Id.newClientId $ getField (_client c)

fromClientId :: Id.ClientId -> ClientId
fromClientId c =
  either
    (error "Invalid client ID")
    (newClientId . fst)
    (hexadecimal (Text.fromStrict $ Id.client c))

-- ClientEntry --------------------------------------------------------------

data ClientEntry = ClientEntry
  { _clientId :: !(Required 1 (Message ClientId)),
    _clientVal :: !(Required 2 (Value ByteString))
  }
  deriving (Eq, Show, Generic)

instance Encode ClientEntry

instance Decode ClientEntry

clientEntry :: ClientId -> ByteString -> ClientEntry
clientEntry c t =
  ClientEntry
    { _clientId = putField c,
      _clientVal = putField t
    }

clientEntryId :: Functor f => (ClientId -> f ClientId) -> ClientEntry -> f ClientEntry
clientEntryId f c = (\x -> c {_clientId = x}) <$> field f (_clientId c)

clientEntryMessage :: Functor f => (ByteString -> f ByteString) -> ClientEntry -> f ClientEntry
clientEntryMessage f c = (\x -> c {_clientVal = x}) <$> field f (_clientVal c)

-- UserEntry ----------------------------------------------------------------

data UserEntry = UserEntry
  { _userId :: !(Required 1 (Message UserId)),
    _userVal :: !(Repeated 2 (Message ClientEntry))
  }
  deriving (Eq, Show, Generic)

instance Encode UserEntry

instance Decode UserEntry

userEntry :: UserId -> [ClientEntry] -> UserEntry
userEntry u c =
  UserEntry
    { _userId = putField u,
      _userVal = putField c
    }

userEntryId :: Functor f => (UserId -> f UserId) -> UserEntry -> f UserEntry
userEntryId f c = (\x -> c {_userId = x}) <$> field f (_userId c)

userEntryClients :: Functor f => ([ClientEntry] -> f [ClientEntry]) -> UserEntry -> f UserEntry
userEntryClients f c = (\x -> c {_userVal = x}) <$> field f (_userVal c)

toOtrRecipients :: [UserEntry] -> Galley.OtrRecipients
toOtrRecipients =
  Galley.OtrRecipients . Galley.UserClientMap
    . foldl' userEntries mempty
  where
    userEntries acc x =
      let u = view userEntryId x
          c = view userEntryClients x
          m = foldl' clientEntries mempty c
       in Map.insert (view userId u) m acc
    clientEntries acc x =
      let c = toClientId $ view clientEntryId x
          t = toBase64Text $ view clientEntryMessage x
       in Map.insert c t acc

fromOtrRecipients :: Galley.OtrRecipients -> [UserEntry]
fromOtrRecipients rcps =
  let m = Galley.userClientMap (Galley.otrRecipientsMap rcps)
   in map mkProtoRecipient (Map.toList m)
  where
    mkProtoRecipient (usr, clts) =
      let xs = map mkClientEntry (Map.toList clts)
       in UserEntry (putField (fromUserId usr)) (putField xs)
    mkClientEntry (clt, t) = clientEntry (fromClientId clt) (fromBase64Text t)

-- Priority -----------------------------------------------------------------

data Priority = LowPriority | HighPriority
  deriving (Eq, Show, Ord, Generic)

instance Encode Priority

instance Decode Priority

instance Enum Priority where
  toEnum 1 = LowPriority
  toEnum 2 = HighPriority
  toEnum x = error $ "Galley.Types.Proto.Priority: invalid enum value: " ++ show x

  fromEnum LowPriority = 1
  fromEnum HighPriority = 2

instance Bounded Priority where
  minBound = LowPriority
  maxBound = HighPriority

toPriority :: Priority -> Gundeck.Priority
toPriority LowPriority = Gundeck.LowPriority
toPriority HighPriority = Gundeck.HighPriority

fromPriority :: Gundeck.Priority -> Priority
fromPriority Gundeck.LowPriority = LowPriority
fromPriority Gundeck.HighPriority = HighPriority

-- NewOtrMessage ------------------------------------------------------------

data NewOtrMessage = NewOtrMessage
  { _newOtrSender :: !(Required 1 (Message ClientId)),
    _newOtrRecipients :: !(Repeated 2 (Message UserEntry)),
    _newOtrNativePush :: !(Optional 3 (Value Bool)),
    _newOtrData :: !(Optional 4 (Value ByteString)),
    _newOtrNativePriority :: !(Optional 5 (Enumeration Priority)), -- See note [orphans]
    _newOtrTransient :: !(Optional 6 (Value Bool)),
    _newOtrReportMissing :: !(Repeated 7 (Message UserId))
  }
  deriving (Eq, Show, Generic)

instance Encode NewOtrMessage

instance Decode NewOtrMessage

newOtrMessage :: ClientId -> [UserEntry] -> NewOtrMessage
newOtrMessage c us =
  NewOtrMessage
    { _newOtrSender = putField c,
      _newOtrRecipients = putField us,
      _newOtrNativePush = putField Nothing,
      _newOtrData = putField Nothing,
      _newOtrNativePriority = putField Nothing,
      _newOtrTransient = putField Nothing,
      _newOtrReportMissing = putField []
    }

newOtrMessageSender :: Functor f => (ClientId -> f ClientId) -> NewOtrMessage -> f NewOtrMessage
newOtrMessageSender f c = (\x -> c {_newOtrSender = x}) <$> field f (_newOtrSender c)

newOtrMessageRecipients :: Functor f => ([UserEntry] -> f [UserEntry]) -> NewOtrMessage -> f NewOtrMessage
newOtrMessageRecipients f c = (\x -> c {_newOtrRecipients = x}) <$> field f (_newOtrRecipients c)

newOtrMessageNativePush :: Functor f => (Bool -> f Bool) -> NewOtrMessage -> f NewOtrMessage
newOtrMessageNativePush f c =
  let g x = Just <$> f (fromMaybe True x)
   in (\x -> c {_newOtrNativePush = x}) <$> field g (_newOtrNativePush c)

newOtrMessageTransient :: Functor f => (Bool -> f Bool) -> NewOtrMessage -> f NewOtrMessage
newOtrMessageTransient f c =
  let g x = Just <$> f (fromMaybe False x)
   in (\x -> c {_newOtrTransient = x}) <$> field g (_newOtrTransient c)

newOtrMessageData :: Functor f => (Maybe ByteString -> f (Maybe ByteString)) -> NewOtrMessage -> f NewOtrMessage
newOtrMessageData f c = (\x -> c {_newOtrData = x}) <$> field f (_newOtrData c)

newOtrMessageNativePriority :: Functor f => (Maybe Priority -> f (Maybe Priority)) -> NewOtrMessage -> f NewOtrMessage
newOtrMessageNativePriority f c = (\x -> c {_newOtrNativePriority = x}) <$> field f (_newOtrNativePriority c)

newOtrMessageReportMissing :: Functor f => ([UserId] -> f [UserId]) -> NewOtrMessage -> f NewOtrMessage
newOtrMessageReportMissing f c = (\x -> c {_newOtrReportMissing = x}) <$> field f (_newOtrReportMissing c)

toNewOtrMessage :: NewOtrMessage -> Galley.NewOtrMessage
toNewOtrMessage msg =
  Galley.NewOtrMessage
    { Galley.newOtrSender = toClientId (view newOtrMessageSender msg),
      Galley.newOtrRecipients = toOtrRecipients (view newOtrMessageRecipients msg),
      Galley.newOtrNativePush = view newOtrMessageNativePush msg,
      Galley.newOtrTransient = view newOtrMessageTransient msg,
      Galley.newOtrData = toBase64Text <$> view newOtrMessageData msg,
      Galley.newOtrNativePriority = toPriority <$> view newOtrMessageNativePriority msg,
      Galley.newOtrReportMissing = toReportMissing $ view newOtrMessageReportMissing msg
    }

toReportMissing :: [UserId] -> Maybe [Id.OpaqueUserId]
toReportMissing [] = Nothing
toReportMissing us = Just $ view userId <$> us

-- Utilities ----------------------------------------------------------------

fromBase64Text :: Text -> ByteString
fromBase64Text = B64.decodeLenient . encodeUtf8

toBase64Text :: ByteString -> Text
toBase64Text = decodeUtf8 . B64.encode
