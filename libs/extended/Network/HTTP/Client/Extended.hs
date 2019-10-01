module Network.HTTP.Client.Extended where

import Imports

import Network.HTTP.Client
import Data.ByteString.Short
import Data.Hashable


-- | Connection destination: just host and port.
data Destination = Destination {-# UNPACK #-} !ShortByteString !Int
    deriving (Eq, Show)

instance Hashable Destination

-- | The exception that gets thrown when the hard limit set by 'addPerHostConnectionLimit' is
-- reached.
data PerHostConnectionLimit = PerHostConnectionLimit
    { connHost :: ByteString
    , connPort :: Int
    }

instance Exception PerHostConnectionLimit


-- | Add a per-host hard connection limit. When there are more than N open connections to a
-- host, any new connection to that host will immediately fail with 'PerHostConnectionLimit'.
--
-- Existing limits in 'Manager' are more fine-grained: for instance, those limits apply to raw
-- and TLS connections separately. The limit set by 'addHardConnectionLimit' applies to all
-- connections to the host/port, regardless of their type.
addPerHostConnectionLimit :: MonadIO m => Int -> ManagerSettings -> m ManagerSettings
addPerHostConnectionLimit limit manager = liftIO $ do
    -- Create a map counting active connections per host.
    connCount :: STMMap.Map Destination Int <- STMMap.newIO

    -- When a connection is created, increment a counter (and possibly throw an exception).
    let onCreate :: Destination -> IO ()
        onCreate dest@(Destination host port) = do
            success <- atomically $ do
                current <- fromMaybe 0 <$> STMMap.lookup dest
                when (current < limit) $ STMMap.insert dest (current + 1)
                pure (current < limit)
            unless success $ throwIO (PerHostConnectionLimit (fromShort host) port)

    -- When a connection is destroyed, which is something 'Manager' should always do,
    -- decrement the counter.
    let onDestroy :: Destination -> IO ()
        onDestroy dest = atomically $ do
            current <- fromMaybe 0 <$> STMMap.lookup dest
            if current <= 1
                then STMMap.delete dest
                else STMMap.insert dest (current - 1)

    -- Modify all connection-creating functions in the 'Manager' to perform 'onCreate' and
    -- 'onDestroy'. Technically 'connectionClose' can be performed on an already closed
    -- connection and we have to handle that gracefully (at least according to the
    -- 'Connection' docs), but 'Manager' seems to close connections exactly once.
    let managerRawConnection' = do
            create <- managerRawConnection manager
            pure $ \addr host port -> do
                let dest = Destination (fromString host) port
                conn <- onCreate dest >> create addr host port
                pure (conn & connectionCloseL %~ (onDestroy dest >>))

        managerTlsConnection' = do
            create <- managerTlsConnection manager
            pure $ \addr host port -> do
                let dest = Destination (fromString host) port
                conn <- onCreate dest >> create addr host port
                pure (conn & connectionCloseL %~ (onDestroy dest >>))

        managerTlsProxyConnection' = do
            create <- managerTlsProxyConnection manager
            pure $ \connstr check server ha host port -> do
                let dest = Destination (fromString host) port
                conn <- onCreate dest >> create connstr check server ha host port
                pure (conn & connectionCloseL %~ (onDestroy dest >>))

    pure ManagerSettings
        { managerRawConnection = managerRawConnection'
        , managerTlsConnection = managerTlsConnection'
        , managerTlsProxyConnection = managerTlsProxyConnection'
        }

-- | A lens for 'connectionClose'.
connectionCloseL :: Lens' Connection (IO ())
connectionCloseL = lens connectionClose (\conn cl -> conn { connectionClose = cl })
