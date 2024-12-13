{-# LANGUAGE TemplateHaskell #-}

module Wire.GundeckAPIAccess where

import Bilge
import Data.ByteString.Conversion
import Data.Id
import Imports
import Network.HTTP.Types
import Polysemy
import Util.Options
import Wire.API.Push.V2 qualified as V2
import Wire.Rpc

data GundeckAPIAccess m a where
  PushV2 :: [V2.Push] -> GundeckAPIAccess m ()
  UserDeleted :: UserId -> GundeckAPIAccess m ()
  UnregisterPushClient :: UserId -> ClientId -> GundeckAPIAccess m ()
  GetPushTokens :: UserId -> GundeckAPIAccess m [V2.PushToken]
  RegisterConsumableNotifcationsClient :: UserId -> ClientId -> GundeckAPIAccess m ()

deriving instance Show (GundeckAPIAccess m a)

makeSem ''GundeckAPIAccess

runGundeckAPIAccess :: (Member Rpc r, Member (Embed IO) r) => Endpoint -> Sem (GundeckAPIAccess : r) a -> Sem r a
runGundeckAPIAccess ep = interpret $ \case
  PushV2 pushes -> do
    chunkedReq <- jsonChunkedIO pushes
    -- No retries because the chunked request body cannot be replayed.
    void . rpc "gundeck" ep $
      method POST
        . path "/i/push/v2"
        . expect2xx
        . chunkedReq
  UserDeleted uid -> do
    void . rpcWithRetries "gundeck" ep $
      method DELETE
        . path "/i/user"
        . zUser uid
        . expect2xx
  UnregisterPushClient uid cid -> do
    void . rpcWithRetries "gundeck" ep $
      method DELETE
        . paths ["i", "clients", toByteString' cid]
        . zUser uid
        . expect [status200, status204, status404]
  GetPushTokens uid -> do
    rsp <-
      rpcWithRetries "gundeck" ep $
        method GET
          . paths ["i", "push-tokens", toByteString' uid]
          . zUser uid
          . expect2xx
    responseJsonMaybe rsp & maybe (pure []) (pure . V2.pushTokens)
  RegisterConsumableNotifcationsClient uid cid -> do
    void . rpcWithRetries "gundeck" ep $
      method POST
        . paths ["i", "users", toByteString' uid, "clients", toByteString' cid, "consumable-notifications"]
        . expect2xx
