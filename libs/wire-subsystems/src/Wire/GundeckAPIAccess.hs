{-# LANGUAGE TemplateHaskell #-}

module Wire.GundeckAPIAccess where

import Bilge as B
import Data.Text.Encoding
import Gundeck.Types.Push.V2 qualified as V2
import Imports
import Network.HTTP.Client qualified as HTTP
import Polysemy
import Util.Options

data GundeckAPIAccess m a where
  PushV2 :: [V2.Push] -> GundeckAPIAccess m ()

makeSem ''GundeckAPIAccess

data GundeckAccessDetails = GundeckAccessDetails
  { endpoint :: Endpoint,
    httpManager :: HTTP.Manager
  }

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
