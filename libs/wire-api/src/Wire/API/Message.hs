{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

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

module Wire.API.Message
  ( -- * Galley conversation types
    NewOtrMessage (..),
    ClientMismatch (..),
    OtrRecipients (..),
    foldrOtrRecipients,
    OtrFilterMissing (..),

    -- * Other galley types
    UserClientMap (..),
    UserClients (..),
    filterClients,
  )
where

import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.Id
import Data.Json.Util
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as T
import Data.Time
import Data.UUID (toASCIIBytes)
import Imports
import Wire.API.Push (Priority)

newtype UserClientMap a = UserClientMap
  { userClientMap :: Map OpaqueUserId (Map ClientId a)
  }
  deriving
    ( Eq,
      Show,
      Functor,
      Foldable,
      Semigroup,
      Monoid,
      Traversable
    )

newtype OtrRecipients = OtrRecipients
  { otrRecipientsMap :: UserClientMap Text
  }
  deriving
    ( Eq,
      Show,
      ToJSON,
      FromJSON,
      Semigroup,
      Monoid
    )

foldrOtrRecipients :: (OpaqueUserId -> ClientId -> Text -> a -> a) -> a -> OtrRecipients -> a
foldrOtrRecipients f a =
  Map.foldrWithKey go a
    . userClientMap
    . otrRecipientsMap
  where
    go u cs acc = Map.foldrWithKey (f u) acc cs

-- | A setting for choosing what to do when a message has not been encrypted
-- for all recipients.
data OtrFilterMissing
  = -- | Pretend everything is okay
    OtrIgnoreAllMissing
  | -- | Complain (default)
    OtrReportAllMissing
  | -- | Complain only about missing
    --      recipients who are /not/ on this list
    OtrIgnoreMissing (Set OpaqueUserId)
  | -- | Complain only about missing
    --      recipients who /are/ on this list
    OtrReportMissing (Set OpaqueUserId)
  deriving (Eq, Show, Generic)

data NewOtrMessage = NewOtrMessage
  { newOtrSender :: !ClientId,
    newOtrRecipients :: !OtrRecipients,
    newOtrNativePush :: !Bool,
    newOtrTransient :: !Bool,
    newOtrNativePriority :: !(Maybe Priority),
    newOtrData :: !(Maybe Text),
    newOtrReportMissing :: !(Maybe [OpaqueUserId])
    -- FUTUREWORK: if (and only if) clients can promise this uid list will always exactly
    -- be the list of uids we could also extract from the messages' recipients field, we
    -- should do the latter, for two reasons: (1) no need for an artificial limit on the
    -- body field length, because it'd be just a boolean; (2) less network consumption.
  }

newtype UserClients = UserClients
  { userClients :: Map OpaqueUserId (Set ClientId)
  }
  deriving (Eq, Show, Semigroup, Monoid, Generic)

filterClients :: (Set ClientId -> Bool) -> UserClients -> UserClients
filterClients p (UserClients c) = UserClients $ Map.filter p c

data ClientMismatch = ClientMismatch
  { cmismatchTime :: !UTCTime,
    -- | Clients that the message /should/ have been encrypted for, but wasn't.
    missingClients :: !UserClients,
    -- | Clients that the message /should not/ have been encrypted for, but was.
    redundantClients :: !UserClients,
    deletedClients :: !UserClients
  }
  deriving (Eq, Show, Generic)

-- Instances ----------------------------------------------------------------

-- JSON

instance ToJSON UserClients where
  toJSON =
    toJSON . Map.foldrWithKey' fn Map.empty . userClients
    where
      fn u c m =
        let k = T.decodeLatin1 (toASCIIBytes (toUUID u))
         in Map.insert k c m

instance FromJSON UserClients where
  parseJSON =
    withObject "UserClients" (fmap UserClients . foldrM fn Map.empty . HashMap.toList)
    where
      fn (k, v) m = Map.insert <$> parseJSON (String k) <*> parseJSON v <*> pure m

instance ToJSON ClientMismatch where
  toJSON m =
    object
      [ "time" .= toUTCTimeMillis (cmismatchTime m),
        "missing" .= missingClients m,
        "redundant" .= redundantClients m,
        "deleted" .= deletedClients m
      ]

instance FromJSON ClientMismatch where
  parseJSON = withObject "ClientMismatch" $ \o ->
    ClientMismatch <$> o .: "time"
      <*> o .: "missing"
      <*> o .: "redundant"
      <*> o .: "deleted"

instance ToJSON a => ToJSON (UserClientMap a) where
  toJSON = toJSON . Map.foldrWithKey' f Map.empty . userClientMap
    where
      f (Id u) clients m =
        let key = T.decodeLatin1 (toASCIIBytes u)
            val = Map.foldrWithKey' g Map.empty clients
         in Map.insert key val m
      g (ClientId c) a = Map.insert c (toJSON a)

instance FromJSON a => FromJSON (UserClientMap a) where
  parseJSON = withObject "user-client-map" $ \o ->
    UserClientMap <$> foldrM f Map.empty (HashMap.toList o)
    where
      f (k, v) m = do
        u <- parseJSON (String k)
        flip (withObject "client-value-map") v $ \c -> do
          e <- foldrM g Map.empty (HashMap.toList c)
          return (Map.insert u e m)
      g (k, v) m = do
        c <- parseJSON (String k)
        t <- parseJSON v
        return (Map.insert c t m)

instance ToJSON NewOtrMessage where
  toJSON otr =
    object $
      "sender" .= newOtrSender otr
        # "recipients" .= newOtrRecipients otr
        # "native_push" .= newOtrNativePush otr
        # "transient" .= newOtrTransient otr
        # "native_priority" .= newOtrNativePriority otr
        # "data" .= newOtrData otr
        # "report_missing" .= newOtrReportMissing otr
        # []

instance FromJSON NewOtrMessage where
  parseJSON = withObject "new-otr-message" $ \o ->
    NewOtrMessage <$> o .: "sender"
      <*> o .: "recipients"
      <*> o .:? "native_push" .!= True
      <*> o .:? "transient" .!= False
      <*> o .:? "native_priority"
      <*> o .:? "data"
      <*> o .:? "report_missing"
