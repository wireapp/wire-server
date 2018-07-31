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

import Bilge
import Bilge.Retry (httpHandlers)
import Brig.App
import Brig.Provider.DB (ServiceConn (..))
import Brig.RPC
import Brig.Types.Provider (HttpsUrl (..))
import Brig.Types.Provider.External
import Control.Error
import Control.Exception (SomeAsyncException)
import Control.Lens (view, set, (^.), (&), (<&>))
import Control.Monad (void)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Retry (recovering)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Conversion
import Data.Dynamic (fromDynamic)
import Data.Foldable (toList)
import Data.Id
import Data.Monoid
import Data.Word
import Galley.Types (Event)
import Network.HTTP.Types.Method
import System.Logger.Class (MonadLogger, msg, val, field ,(~~))
import URI.ByteString

import qualified Data.List1                   as List1
import qualified Galley.Types.Bot             as Galley
import qualified Network.HTTP.Client          as Http
import qualified Network.HTTP.Client.Internal as Http
import qualified System.Logger.Class          as Log
import qualified OpenSSL.Session              as SSL

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
            -- TODO: not sure if 'Reuse' is the right thing to do here --
            -- maybe instead of returning the connection to the pool we want
            -- to close it
            Http.withConnection' req man Http.Reuse $ \mConn -> do
                -- If we see this connection for the first time, verify fingerprints
                let conn = Http.managedResource mConn
                    seen = Http.managedReused   mConn
                case (seen, fromDynamic @SSL.SSL (Http.connectionRaw conn)) of
                    (True, _) -> pure ()
                    (_, Nothing) -> error "Should not be possible, \
                                          \services only allow SSL connections"
                    (_, Just ssl) -> verifyFingerprints fprs ssl
                -- Make a request using this connection and return it back to the pool
                Http.httpLbs req{Http.connectionOverride = Just mConn} man
        case Bilge.statusCode rs of
            201 -> decodeBytes "External" (responseBody rs)
            409 -> throwE ServiceBotConflict
            _   -> extLogError scon rs >> throwE ServiceUnavailable
  where
    req = extReq scon ["bots"]
        . method POST
        . Bilge.json new
        $ empty

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
    if isJust (responseBody rs)
        then Just <$> decodeBody "galley" rs
        else return Nothing
  where
    req = path "/i/bots"
        . header "Z-User" (toByteString' zusr)
        . maybe id (header "Z-Connection" . toByteString') zcon
        . contentJson
        . lbytes (encode (Galley.removeBot conv bot))
        . expect2xx
