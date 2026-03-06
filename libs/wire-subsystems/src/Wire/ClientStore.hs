{-# LANGUAGE TemplateHaskell #-}

module Wire.ClientStore where

import Data.Id
import Data.Json.Util (UTCTimeMillis)
import Data.Time
import Imports
import Polysemy
import Wire.API.MLS.CipherSuite
import Wire.API.User.Client
import Wire.API.User.Client.Prekey
import Wire.API.UserMap

data DuplicateMLSPublicKey = DuplicateMLSPublicKey

data ClientStore m a where
  -- Lifecycle
  Upsert :: UserId -> ClientId -> UTCTimeMillis -> NewClient -> ClientStore m (Maybe DuplicateMLSPublicKey)
  Delete :: UserId -> ClientId -> ClientStore m ()
  UpdateLabel :: UserId -> ClientId -> Maybe Text -> ClientStore m ()
  UpdateCapabilities :: UserId -> ClientId -> Maybe ClientCapabilityList -> ClientStore m ()
  UpdateLastActive :: UserId -> ClientId -> UTCTime -> ClientStore m ()
  -- Lookups
  LookupClient :: UserId -> ClientId -> ClientStore m (Maybe Client)
  LookupClients :: UserId -> ClientStore m [Client]
  LookupClientIds :: UserId -> ClientStore m [ClientId]
  LookupClientIdsBulk :: [UserId] -> ClientStore m UserClients
  LookupClientsBulk :: [UserId] -> ClientStore m (UserMap (Set Client))
  LookupPubClientsBulk :: [UserId] -> ClientStore m (UserMap (Set PubClient))
  LookupPrekeyIds :: UserId -> ClientId -> ClientStore m [PrekeyId]
  GetActivityTimestamps :: UserId -> ClientStore m [Maybe UTCTime]
  -- Proteus
  UpdatePrekeys :: UserId -> ClientId -> [UncheckedPrekeyBundle] -> ClientStore m ()
  ClaimPrekey :: UserId -> ClientId -> ClientStore m (Maybe ClientPrekey)
  -- MLS
  AddMLSPublicKeys :: UserId -> ClientId -> [(SignatureSchemeTag, ByteString)] -> ClientStore m (Maybe DuplicateMLSPublicKey)
  LookupMLSPublicKey :: UserId -> ClientId -> SignatureSchemeTag -> ClientStore m (Maybe LByteString)

makeSem ''ClientStore
