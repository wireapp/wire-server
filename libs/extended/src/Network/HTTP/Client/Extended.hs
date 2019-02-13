{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A version of "Network.HTTP.Client" with extra utilities.
module Network.HTTP.Client.Extended
    ( module Network.HTTP.Client
    -- * Connection limits
    , addPerHostConnectionLimit
    , PerHostConnectionLimitReached(..)
    ) where

import Imports
import Control.Exception (throwIO)
import Control.Lens
import Data.ByteString.Short
import Data.Hashable
import Network.HTTP.Client
import Network.HTTP.Client.Internal (Connection(..), ManagerSettings(..))

import qualified STMContainers.Map as STMMap

-- | Connection destination: just host and port.
data Destination = Destination {-# UNPACK #-} !ShortByteString !Int
    deriving (Eq, Show, Generic)

instance Hashable Destination

-- | Add a per-host hard connection limit. When there are more than N open connections to a
-- host, any attempt to establish a new connection to that host will immediately fail with
-- 'PerHostConnectionLimitReached'.
--
-- Existing limits in 'Manager' are more fine-grained: for instance, those limits apply to raw
-- and TLS connections separately. The limit set by 'addHardConnectionLimit' applies to all
-- connections to the host/port, regardless of their type.
--
-- Hosts are defined "whatever host is set in the 'Request'" so if there are several servers
-- behind a load balancer, we're still going to count them as a single host.
--
-- /Note 2019-02-05:/ @http-client@ Github issue requesting this functionality:
-- <https://github.com/snoyberg/http-client/issues/307>. We might try to upstream our
-- implementation.
addPerHostConnectionLimit :: MonadIO m => Int -> ManagerSettings -> m ManagerSettings
addPerHostConnectionLimit limit managerSettings = liftIO $ do
    -- Create a map counting active connections per host.
    connCount :: STMMap.Map Destination Int <- STMMap.newIO

    -- When a connection is created, increment a counter (and possibly throw an exception).
    let onCreate :: Destination -> IO ()
        onCreate dest@(Destination host_ port_) = do
            success <- atomically $ do
                current <- fromMaybe 0 <$> STMMap.lookup dest connCount
                when (current < limit) $ STMMap.insert (current + 1) dest connCount
                pure (current < limit)
            liftIO $ print (show success)
            unless success $ throwIO $
                PerHostConnectionLimitReached (fromShort host_) port_

    -- When a connection is destroyed, which is something 'Manager' should always do,
    -- decrement the counter.
    let onDestroy :: Destination -> IO ()
        onDestroy dest = do
            liftIO $ putStrLn "Destroying"
            atomically $ do
                current <- fromMaybe 0 <$> STMMap.lookup dest connCount
                if current <= 1
                    then STMMap.delete dest connCount
                    else STMMap.insert (current - 1) dest connCount

    -- Modify all connection-creating functions in the 'Manager' to perform 'onCreate' and
    -- 'onDestroy'. Technically 'connectionClose' can be performed on an already closed
    -- connection and we have to handle that gracefully (at least according to the
    -- 'Connection' docs), but 'Manager' seems to close connections exactly once.
    let managerRawConnection' = do
            create <- managerRawConnection managerSettings
            pure $ \addr host_ port_ -> do
                let dest = Destination (fromString host_) port_
                conn <- onCreate dest >> create addr host_ port_
                pure (conn & connectionCloseL %~ (onDestroy dest >>))

        managerTlsConnection' = do
            create <- managerTlsConnection managerSettings
            pure $ \addr host_ port_ -> do
                let dest = Destination (fromString host_) port_
                conn <- onCreate dest >> create addr host_ port_
                pure (conn & connectionCloseL %~ (onDestroy dest >>))

        managerTlsProxyConnection' = do
            create <- managerTlsProxyConnection managerSettings
            pure $ \connstr check server ha host_ port_ -> do
                let dest = Destination (fromString host_) port_
                conn <- onCreate dest >> create connstr check server ha host_ port_
                pure (conn & connectionCloseL %~ (onDestroy dest >>))

    pure managerSettings
        { managerRawConnection = managerRawConnection'
        , managerTlsConnection = managerTlsConnection'
        , managerTlsProxyConnection = managerTlsProxyConnection'
        }

-- | A lens for 'connectionClose'.
connectionCloseL :: Lens' Connection (IO ())
connectionCloseL = lens connectionClose (\conn cl -> conn { connectionClose = cl })

-- | The exception that gets thrown when the hard limit set by 'addPerHostConnectionLimit' is
-- reached.
data PerHostConnectionLimitReached = PerHostConnectionLimitReached
    { connHost :: !ByteString
    , connPort :: !Int
    } deriving (Eq, Show)

instance Exception PerHostConnectionLimitReached
