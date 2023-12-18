{-# LANGUAGE TemplateHaskell #-}

module Wire.Notification where

import Bilge as B
import Control.Lens
import Data.Aeson
import Data.Id
import Data.List.NonEmpty (NonEmpty)
import Data.Text.Encoding (encodeUtf8)
import Gundeck.Types hiding (Push)
import Gundeck.Types.Push.V2 qualified as V2
import Imports
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types
import Polysemy
import Util.Options
import Wire.API.Team.Member
import Wire.Arbitrary

data RecipientBy user = Recipient
  { _recipientUserId :: user,
    _recipientClients :: RecipientClients
  }
  deriving stock (Functor, Foldable, Traversable, Show, Ord, Eq, Generic)
  deriving (Arbitrary) via GenericUniform (RecipientBy user)

makeLenses ''RecipientBy

type Recipient = RecipientBy UserId

data PushTo user = PushTo
  { _pushConn :: Maybe ConnId,
    _pushTransient :: Bool,
    _pushRoute :: Route,
    _pushNativePriority :: Maybe Priority,
    _pushAsync :: Bool,
    pushOrigin :: Maybe UserId,
    _pushRecipients :: NonEmpty (RecipientBy user),
    pushJson :: Object,
    pushRecipientListType :: ListType
  }
  deriving stock (Eq, Generic, Functor, Foldable, Traversable, Show)
  deriving (Arbitrary) via GenericUniform (PushTo user)

makeLenses ''PushTo

type PushToUser = PushTo UserId

data NotificationSubsystem m a where
  Push :: [PushToUser] -> NotificationSubsystem m ()
  PushSlowly :: [PushToUser] -> NotificationSubsystem m ()

makeSem ''NotificationSubsystem

data GundeckAccessDetails = GundeckAccessDetails
  { endpoint :: Endpoint,
    httpManager :: HTTP.Manager
  }

data GundeckAPIAccess m a where
  PushV2 :: [V2.Push] -> GundeckAPIAccess m ()

makeSem ''GundeckAPIAccess

-- | We interpret this using 'GundeckAPIAccess' so we can mock it out for testing.
runNotificationSubsystemGundeck :: Member (GundeckAPIAccess) r => Sem (NotificationSubsystem : r) a -> Sem r a
runNotificationSubsystemGundeck = interpret $ \case
  Push _ ->
    pushV2 []
  PushSlowly _ ->
    pushV2 []

-- TODO: write a test which says all listed notification are sent
-- TODO: write a test which tests the chunking of notifications
-- TODO: write a test for listtype and maximum fanout limit thing
pushImpl :: Member (GundeckAPIAccess) r => [PushToUser] -> Sem r ()
pushImpl _ = pushV2 []

-- TODO: Test manually if this even works.
runGundeckAPIAccess :: Member (Embed IO) r => GundeckAccessDetails -> Sem (GundeckAPIAccess : r) a -> Sem r a
runGundeckAPIAccess accessDetails = interpret $ \case
  PushV2 pushes -> do
    chunkedReq <- jsonChunkedIO pushes
    let req =
          B.host (encodeUtf8 accessDetails.endpoint._host)
            . B.port accessDetails.endpoint._port
            . path "/i/push/v2"
            . expect2xx
            . chunkedReq
    B.runHttpT accessDetails.httpManager $
      -- Because of 'expect2xx' we don't actually need to check the response
      void $
        B.post req
