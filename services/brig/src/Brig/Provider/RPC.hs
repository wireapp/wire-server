{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | RPCs towards service providers.
module Brig.Provider.RPC
    ( -- * External RPC
      ServiceError (..)
    , createBot

      -- Internal RPC
    , setServiceConn
    , removeServiceConn
    , addBotMember
    , removeBotMember
    ) where

import Imports
import Bilge
import Bilge.Retry (httpHandlers)
import Brig.App
import Brig.Provider.DB (ServiceConn (..))
import Brig.RPC
import Brig.Types.Provider (HttpsUrl (..))
import Brig.Types.Provider.External
import Control.Error
import Control.Lens (view, set, (^.), (<&>))
import Control.Monad.Catch
import Control.Retry (recovering)
import Data.Aeson
import Data.ByteString.Conversion
import Data.Id
import Galley.Types (Event)
import Network.HTTP.Types.Method
import Ssl.Util (withVerifiedSslConnection)
import Network.HTTP.Types.Status
import System.Logger.Class (MonadLogger, msg, val, field, (~~))
import URI.ByteString

import qualified Data.List1                   as List1
import qualified Galley.Types.Bot             as Galley
import qualified Network.HTTP.Client          as Http
import qualified System.Logger.Class          as Log

--------------------------------------------------------------------------------
-- External RPC

data ServiceError
    = ServiceUnavailable
    | ServiceBotConflict

-- | Request a new bot to be created by an external service.
--
-- If the external service is unavailable, returns a specific error
-- or the response body cannot be parsed, a 'ServiceError' is returned.
createBot :: ServiceConn -> NewBotRequest -> ExceptT ServiceError AppIO NewBotResponse
createBot scon new = do
    let fprs = toList (sconFingerprints scon)
    (man, verifyFingerprints) <- view extGetManager
    extHandleAll onExc $ do
        rs <- lift $ recovering x3 httpHandlers $ const $ liftIO $
            withVerifiedSslConnection (verifyFingerprints fprs) man reqBuilder $ \req ->
                Http.httpLbs req man
        case Bilge.statusCode rs of
            201 -> decodeBytes "External" (responseBody rs)
            409 -> throwE ServiceBotConflict
            _   -> extLogError scon rs >> throwE ServiceUnavailable
  where
    reqBuilder
        = extReq scon ["bots"]
        . method POST
        . Bilge.json new

    onExc ex = extLogError scon ex >> throwE ServiceUnavailable

extReq :: ServiceConn -> [ByteString] -> Request -> Request
extReq scon ps =
      maybe id host (extHost url)
    . port (fromMaybe 443 (extPort url))
    . header "Authorization" ("Bearer " <> toByteString' tok)
    . paths (url^.pathL : ps)
    . secure
  where
    url = httpsUrl (sconBaseUrl scon)
    tok = List1.head (sconAuthTokens scon)

extHost :: URI -> Maybe ByteString
extHost u = u^.authorityL <&> view (authorityHostL.hostBSL)

extPort :: URI -> Maybe Word16
extPort u = do
    a <- u^.authorityL
    p <- a^.authorityPortL
    return (fromIntegral (p^.portNumberL))

extHandleAll :: MonadCatch m => (SomeException -> m a) -> m a -> m a
extHandleAll f ma = catches ma
    [ Handler $ \(ex :: SomeAsyncException) -> throwM ex
    , Handler $ \(ex :: SomeException)      -> f ex
    ]

-- nb. We log these errors on 'Info' level since we're usually not
-- able to do anything about them and don't want to distract from
-- other important errors.
extLogError :: (MonadLogger m, Show e) => ServiceConn -> e -> m ()
extLogError scon e = Log.info
     $ field "provider" (toByteString pid)
    ~~ field "service"  (toByteString sid)
    ~~ field "error"    (show e)
    ~~ msg (val "External service error")
  where
    pid = sconProvider scon
    sid = sconService  scon

--------------------------------------------------------------------------------
-- Internal RPC

-- | Set service connection information in galley.
setServiceConn :: ServiceConn -> AppIO ()
setServiceConn scon = do
    Log.debug $ remote "galley"
              . field "provider" (toByteString pid)
              . field "service"  (toByteString sid)
              . msg (val "Setting service connection")
    void $ galleyRequest POST req
  where
    pid = sconProvider scon
    sid = sconService scon
    ref = Galley.newServiceRef sid pid
    url = sconBaseUrl scon
    tok = List1.head (sconAuthTokens scon)
    fps = toList (sconFingerprints scon)

    req = path "/i/services"
        . contentJson
        . lbytes (encode svc)
        . expect2xx

    svc = Galley.newService ref url tok fps
        & set Galley.serviceEnabled (sconEnabled scon)

-- | Remove service connection information from galley.
removeServiceConn :: ProviderId -> ServiceId -> AppIO ()
removeServiceConn pid sid = do
    Log.debug $ remote "galley"
              . field "provider" (toByteString pid)
              . field "service"  (toByteString sid)
              . msg (val "Removing service connection")
    void $ galleyRequest DELETE req
  where
    req = path "/i/services"
        . contentJson
        . lbytes (encode (Galley.newServiceRef sid pid))
        . expect2xx

-- | Tell galley to add a service bot as a member to a conversation.
addBotMember
    :: UserId
    -> ConnId
    -> ConvId
    -> BotId
    -> ClientId
    -> ProviderId
    -> ServiceId
    -> AppIO Event
addBotMember zusr zcon conv bot clt pid sid = do
    Log.debug $ remote "galley"
              . field "provider" (toByteString pid)
              . field "service"  (toByteString sid)
              . field "conv"     (toByteString conv)
              . field "user"     (toByteString zusr)
              . field "bot"      (toByteString bot)
              . msg (val "Adding bot member")
    decodeBody "galley" =<< galleyRequest POST req
  where
    req = path "/i/bots"
        . header "Z-User" (toByteString' zusr)
        . header "Z-Connection" (toByteString' zcon)
        . contentJson
        . lbytes (encode (Galley.addBot (Galley.newServiceRef sid pid) conv bot clt))
        . expect2xx

-- | Tell galley to remove a service bot from a conversation.
removeBotMember
    :: UserId
    -> Maybe ConnId
    -> ConvId
    -> BotId
    -> AppIO (Maybe Event)
removeBotMember zusr zcon conv bot = do
    Log.debug $ remote "galley"
              . field "user"     (toByteString zusr)
              . field "conv"     (toByteString conv)
              . field "bot"      (toByteString bot)
              . msg (val "Removing bot member")
    rs <- galleyRequest DELETE req
    if isJust (responseBody rs) && Bilge.statusCode rs == 200
        then Just <$> decodeBody "galley" rs
        else return Nothing
  where
    req = path "/i/bots"
        . header "Z-User" (toByteString' zusr)
        . maybe id (header "Z-Connection" . toByteString') zcon
        . contentJson
        . lbytes (encode (Galley.removeBot conv bot))
        . expect [status200, status404] -- 404 is allowed: a given conversation may no longer exist
