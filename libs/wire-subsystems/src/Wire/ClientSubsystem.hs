{-# LANGUAGE TemplateHaskell #-}

module Wire.ClientSubsystem where

import Data.Default
import Data.Domain
import Data.Id
import Data.Misc
import Data.Qualified
import Data.Time.Clock
import Imports
import Polysemy
import Wire.API.Team.LegalHold
import Wire.API.Team.LegalHold.Internal
import Wire.API.User.Client
import Wire.API.User.Client.Prekey
import Wire.API.UserEvent
import Wire.API.UserMap

-- | Re-authentication policy.
--
-- For a potential new client, a policy is a function that takes as arguments
-- the number of existing clients of the same type, and whether the client
-- already exists, and returns whether the user should be forced to
-- re-authenticate.
newtype ReAuthPolicy = ReAuthPolicy {unReAuthPolicy :: Int -> Bool -> Bool}

-- | Default re-authentication policy.
--
-- Re-authenticate if there is at least one other client.
reAuthForNewClients :: ReAuthPolicy
reAuthForNewClients = ReAuthPolicy {unReAuthPolicy = \count upsert -> count > 0 && not upsert}

instance Default ReAuthPolicy where
  def = reAuthForNewClients

data ClientSubsystem m a where
  InternalGetActivityTimestamps :: UserId -> ClientSubsystem m [Maybe UTCTime]
  LookupLocalClient :: UserId -> ClientId -> ClientSubsystem m (Maybe Client)
  LookupLocalClients :: UserId -> ClientSubsystem m [Client]
  LookupLocalPublicClientsBulk :: [UserId] -> ClientSubsystem m (UserMap (Set PubClient))
  LookupPublicClient :: Qualified UserId -> ClientId -> ClientSubsystem m (Maybe PubClient)
  LookupPublicClients :: Qualified UserId -> ClientSubsystem m [PubClient]
  LookupPublicClientsBulk :: [Qualified UserId] -> ClientSubsystem m (QualifiedUserMap (Set PubClient))
  AddClient :: Local UserId -> Maybe ConnId -> NewClient -> ClientSubsystem m Client
  AddClientWithPolicy :: ReAuthPolicy -> Local UserId -> Maybe ConnId -> NewClient -> ClientSubsystem m Client
  UpsertClient :: Local UserId -> ClientId -> NewClient -> Maybe ClientCapabilityList -> ClientSubsystem m (Client, [Client], Word)
  OnClientEvent :: UserId -> Maybe ConnId -> ClientEvent -> ClientSubsystem m ()
  EnqueueClientDeletion :: UserId -> Maybe ConnId -> Client -> ClientSubsystem m ()
  RemoveClient :: UserId -> ConnId -> ClientId -> Maybe PlainTextPassword6 -> ClientSubsystem m ()
  RemoveLegalHoldClient :: UserId -> ClientSubsystem m ()
  PublishLegalHoldClientRequested :: UserId -> LegalHoldClientRequest -> ClientSubsystem m ()
  UpdateClient :: UserId -> ClientId -> UpdateClient -> ClientSubsystem m ()
  -- Prekeys
  ClaimPrekey :: LegalholdProtectee -> UserId -> Domain -> ClientId -> ClientSubsystem m (Maybe ClientPrekey)
  ClaimLocalPrekey :: LegalholdProtectee -> UserId -> ClientId -> ClientSubsystem m (Maybe ClientPrekey)

makeSem ''ClientSubsystem
